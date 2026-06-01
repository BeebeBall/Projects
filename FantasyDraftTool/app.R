library(shiny)
library(dplyr)
library(stringr)
library(tidyr)
library(DT)
library(lpSolve)
library(ggplot2)

### LOAD DATA -------------------------------------------------------

batters <- read.csv("batters_weighted_projections.csv") %>% 
  mutate(Position = ifelse(
      Position == "DH",
      "UTIL",
      Position
    )
  )
pitchers <- read.csv("pitchers_weighted_projections.csv")

pitchers$Position <- "P"

players <- bind_rows(
  batters %>% select(Name, Position, FPTS),
  pitchers %>% select(Name, Position, FPTS)
)

### CLEAN POSITIONS -------------------------------------------------

players <- players %>%
  mutate(
    Position = str_replace_all(Position, "RF|LF|CF", "OF")
  ) %>%
  separate_rows(Position, sep="/") %>%
  distinct()

players$player_id <- as.numeric(factor(players$Name))

### LEAGUE STRUCTURE ------------------------------------------------

league_size <- 8

roster_slots <- c(
  "C","1B","2B","3B","SS",
  "OF","OF","OF",
  "UTIL",
  "BENCH", "BENCH", "BENCH",
  "P","P","P","P","P", "P", "P"
)

pos_demand <- c(
  C = league_size * 1,
  `1B` = league_size * 1,
  `2B` = league_size * 1,
  `3B` = league_size * 1,
  UTIL = league_size * 1,
  SS = league_size * 1,
  OF = league_size * 3,
  P = league_size * 7
)

### POSITIONAL SCARCITY ---------------------------------------------

replacement <- players %>%
  group_by(Position) %>%
  arrange(desc(FPTS)) %>% 
  mutate(rank=row_number()) %>%
  filter(rank == pos_demand[Position]) %>%
  select(Position, replacement=FPTS)

players <- players %>%
  left_join(replacement, by="Position") %>%
  mutate(VORP = FPTS - replacement)


### POSITION ELIGIBILITY --------------------------------------------

eligible_for_slot <- function(player_pos, slot){
  
  if(slot == "UTIL"){
    return(player_pos != "P")
  }
  
  if(slot == "BENCH"){
    return(player_pos != "P")
  }
  
  if(slot == "P"){
    return(player_pos == "P")
  }
  
  player_pos == slot
}




############################
# FIND SLOT FOR PLAYER
############################

find_slot <- function(player_pos, roster){
  
  for(i in seq_along(roster_slots)){
    
    if(is.na(roster[i])){
      
      if(eligible_for_slot(player_pos, roster_slots[i])){
        
        return(i)
        
      }
      
    }
    
  }
  
  return(NA)
}

### ROSTER OPTIMIZER ------------------------------------------------

optimize_roster <- function(pool){
  
  n_players <- nrow(pool)
  n_slots <- length(roster_slots)
  
  obj <- pool$VORP
  
  const <- matrix(0, nrow=n_slots, ncol=n_players)
  
  for(i in seq_along(roster_slots)){
    slot <- roster_slots[i]
    const[i,] <- sapply(pool$Position, eligible_for_slot, slot=slot)
  }
  
  player_constraints <- model.matrix(~factor(player_id)-1, pool)
  
  const <- rbind(const, t(player_constraints))
  
  const.dir <- c(rep("<=",n_slots), rep("<=",ncol(player_constraints)))
  const.rhs <- c(rep(1,n_slots), rep(1,ncol(player_constraints)))
  
  res <- lp(
    direction="max",
    objective.in=obj,
    const.mat=const,
    const.dir=const.dir,
    const.rhs=const.rhs,
    all.bin=TRUE
  )
  
  pool[res$solution==1,]
}


### SHINY UI --------------------------------------------------------

teams <- paste0("Team ",1:8)

ui <- fluidPage(
  
  titlePanel("Fantasy Draft Assistant"),

    
    mainPanel(
      
      do.call(tabsetPanel, c(
        
        list(
          
          tabPanel("Draft Board",
                   
                   selectInput(
                     "position_filter",
                     "Filter by Position",
                     choices = c("All", sort(unique(players$Position))),
                     selected = "All"
                   ),
                   
                   DTOutput("draft_board"),
                
          ),
          
          tabPanel("Best Available",
                   tableOutput("hitterboard"),
                   tableOutput("pitcherboard")
          ),
          
          tabPanel("Positional Scarcity",
                   plotOutput("scarcity_plot")
          )
          
        ),
        
        lapply(teams, function(team){
          
          tabPanel(team,
                   
                   selectizeInput(
                     paste0(team,"_pick"),
                     "Select Player",
                     choices = players$Name,
                     multiple = FALSE,
                     options = list(
                       placeholder = "Search player..."
                     )
                   ),
                   
                   actionButton(
                     paste0(team,"_draft"),
                     "Draft Player"
                   ),
                   
                   tableOutput(paste0(team,"_roster")),
                   tableOutput(paste0(team,"_totals")),

                   
          )
          
        })
        
      ))
      
    )
)
  


### SHINY SERVER ----------------------------------------------------

server <- function(input, output, session){
  
  pool <- reactiveVal(players)
  
  rosters <- reactiveValues()
  
  for(t in teams){
    rosters[[t]] <- rep(NA,19)
  }
  
  observe({
    
    available <- sort(unique(pool()$Name))
    
    for(t in teams){
      
      updateSelectizeInput(
        session,
        paste0(t,"_pick"),
        choices = available,
        server = TRUE
      )
      
    }
    
  })
  
  for(team in teams){
    
    local({
      
      t <- team
      
      observeEvent(input[[paste0(t,"_draft")]],{
        
        player <- input[[paste0(t,"_pick")]]
        
        if(is.null(player) || player=="") return()
        
        # get player info from the pool
        p <- pool() %>% filter(Name==player)
        
        # find the first valid roster slot for this player's position
        roster <- rosters[[t]]
        slot <- find_slot(p$Position[1], roster)
        
        if(is.na(slot)){
          showNotification("No valid roster slot available", type="error")
          return()
        }
        
        # assign player to that slot
        roster[slot] <- player
        rosters[[t]] <- roster
        
        # remove player from the global pool
        pool(pool() %>% filter(Name != player))
        
        #Updating other tables
        pitchers_pool(
          pitchers_pool() %>% filter(Name != player)
        )
        
        batters_pool(
          batters_pool() %>% filter(Name != player)
        )
        
        # reset the dropdown
        updateSelectizeInput(session, paste0(t,"_pick"), selected = "")
      })
      
      output[[paste0(t,"_roster")]] <- renderTable({
        
        data.frame(
          Slot=roster_slots,
          Player=rosters[[t]]
        )
        
      })
      
      team_totals <- reactive({
        
        totals <- lapply(teams, function(tm){
          
          roster_players <- rosters[[tm]][!is.na(rosters[[tm]])]
          
          if(length(roster_players) == 0){
            return(data.frame(Team = tm, Score = 0, VORP = 0))
          }
          
          df <- players %>% filter(Name %in% roster_players)
          batstatdf = batters %>% 
            filter(Name %in% roster_players)
          pitchstatdf = pitchers %>% 
            filter(Name %in% roster_players)
          data.frame(
            Team = t,
            Score = sum(df$FPTS),
            VORP  = sum(df$VORP),
            HR = sum(batstatdf$HR_total),
            RBI = sum(batstatdf$RBI_total),
            SB = sum(batstatdf$SB_total),
            BB = sum(batstatdf$BB_total),
            K_pitchers = sum(pitchstatdf$SO_total),
            W = sum(pitchstatdf$W_total),
            SV = sum(pitchstatdf$SV_total),
            IP = sum(pitchstatdf$IP_total),
            HLD = sum(pitchstatdf$HLD_total)
          )
          
        }) %>% bind_rows()
        
        totals %>%
          mutate(
            ScoreRank = rank(-Score, ties.method = "min"),
            VORPRank  = rank(-VORP, ties.method = "min")
          )
      })
      
      output[[paste0(t,"_totals")]] <- renderTable({
        team_totals()
      })
      
      
    })
    
  }
  
  output$draft_board <- renderDT({
    
    df <- pool()
    
    if(input$position_filter != "All"){
      df <- df %>% filter(Position == input$position_filter)
    }
    
    df %>%
      arrange(desc(VORP)) %>%
      select(Name, Position, FPTS, VORP)
    
  })

  
  output$scarcity_plot <- renderPlot({
    
    pool() %>%
      group_by(Position) %>%
      summarize(avg_VORP=mean(VORP, na.rm=TRUE)*-1) %>%
      ggplot(aes(Position,avg_VORP))+
      geom_col()
    
  })
  
  P_replacement <- pitchers %>%
    group_by(Position) %>%
    arrange(desc(FPTS)) %>% 
    mutate(rank=row_number()) %>%
    filter(rank == pos_demand[Position]) %>%
    select(Position, replacement=FPTS)
  
  pitchersnew <- pitchers %>%
    left_join(replacement, by="Position") %>%
    mutate(VORP = FPTS - replacement)
  
  drafted_players <- unlist(rosters)
  drafted_players <- drafted_players[!is.na(drafted_players)]
  
  reactivepitch = pitchersnew %>% 
    select(Name, FPTS, VORP, ER_total, SO_total, W_total, SV_total, IP_total, HLD_total) %>%
    arrange(desc(VORP)) %>% 
    mutate(VONP = FPTS - lead(FPTS, n = 1),
           VON3P = FPTS - (lead(FPTS, n = 1) + lead(FPTS, n = 2) + lead(FPTS, n = 3)) / 3) %>% 
    select(Name, FPTS, VORP, VONP, VON3P, everything())
  
  pitchers_pool = reactiveVal(reactivepitch)
  
  output$pitcherboard = renderTable({
    pitchers_pool() %>% 
      head(10)
    
  })
  
  P_replacement <- pitchers %>%
    group_by(Position) %>%
    arrange(desc(FPTS)) %>% 
    mutate(rank=row_number()) %>%
    filter(rank == pos_demand[Position]) %>%
    select(Position, replacement=FPTS)
  
  battersnew <- batters %>%
    left_join(replacement, by="Position") %>%
    mutate(VORP = FPTS - replacement)
  
  drafted_players <- unlist(rosters)
  drafted_players <- drafted_players[!is.na(drafted_players)]
  
  reactivebat = battersnew %>% 
    select(Name, Position, FPTS, VORP, HR_total, RBI_total, SB_total, BB_total) %>%
    arrange(desc(VORP)) %>% 
    group_by(Position) %>% 
    mutate(VONP = FPTS - lead(FPTS, n = 1),
           VON3P = FPTS - (lead(FPTS, n = 1) + lead(FPTS, n = 2) + lead(FPTS, n = 3)) / 3) %>% 
    select(Name, FPTS, VORP, VONP, VON3P, everything())
  
  batters_pool = reactiveVal(reactivebat)
  
  output$hitterboard = renderTable({
    batters_pool() %>% 
      head(10)
  })
  
}

### RUN APP ---------------------------------------------------------

shinyApp(ui, server)