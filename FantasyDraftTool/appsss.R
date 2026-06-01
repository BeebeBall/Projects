#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

suggest_pick_team <- function(pool, roster){
  
  pool <- pool[!is.na(pool$VORP) & is.finite(pool$VORP), ]
  if(nrow(pool) == 0) return(NULL)
  
  scores <- sapply(1:nrow(pool), function(i){
    
    pick <- pool[i,]
    
    # simulate adding player to this team's roster
    temp_roster <- roster
    slot <- find_slot(pick$Position, temp_roster)
    
    if(is.na(slot)) return(-Inf)  # can't fit → ignore
    
    temp_roster[slot] <- pick$Name
    
    remaining <- pool[-i,]
    
    # optimize remaining roster spots
    remaining_value <- tryCatch({
      opt <- optimize_roster(remaining)
      sum(opt$VORP)
    }, error = function(e) 0)
    
    pick$VORP + remaining_value
  })
  
  pool %>%
    mutate(score = scores) %>%
    arrange(desc(score)) %>%
    head(5)
}


output[[paste0(t,"_totals")]] <- renderTable({
  
  
  
  # Calculate totals for all teams
  totals <- lapply(teams, function(tm){
    roster_players <- rosters[[tm]][!is.na(rosters[[tm]])]
    if(length(roster_players)==0){
      return(data.frame(Team=tm, Score=0, VORP=0))
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
  
  # Compute rankings (higher is better)
  totals <- totals %>%
    mutate(
      ScoreRank = rank(-Score, ties.method="min"),
      VORPRank = rank(-VORP, ties.method="min")
    )
  
  # Return only the current team
  totals %>% filter(Team == t)
})


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

library(tidyverse)


setwd("C:/Users/STP/Desktop/Baseball Projects/Fantasy App")

pitchers = read.csv("pitchers_weighted_projections.csv") %>% 
  rename(FPTS_Pitcher = FPTS)

batters = read.csv("batters_weighted_projections.csv") %>% 
  rename(FPTS_Batter = FPTS)

test = bind_rows(batters, pitchers) %>% 
  mutate(FPTS = FPTS_Batter + FPTS_Pitcher)

# Define UI for application that draws a histogram
ui <- navbarPage(
  title = "Fantasy Baseball Draft Tool",
  
  tabPanel(
    title = "Nate"),
  tabPanel(
    title = "Maple"),
  tabPanel(
    title = "Ethan"),
  tabPanel(
    title = "Jayden"),
  tabPanel(
    title = "Oleg"),
  tabPanel(
    title = "Tynan"),
  tabPanel(
    title = "Antonio")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


#### NEW APP

library(shiny)
library(dplyr)
library(stringr)
library(tidyr)
library(DT)
library(lpSolve)
library(ggplot2)

### LOAD DATA -------------------------------------------------------

batters <- read.csv("batters_weighted_projections.csv")
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

league_size <- 12

roster_slots <- c(
  "C","1B","2B","3B","SS",
  "OF","OF","OF",
  "UTIL","UTIL",
  "P","P","P","P","P"
)

pos_demand <- c(
  C = league_size * 1,
  `1B` = league_size * 1,
  `2B` = league_size * 1,
  `3B` = league_size * 1,
  SS = league_size * 1,
  OF = league_size * 3,
  P = league_size * 5
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
  
  if(slot == "P"){
    return(player_pos == "P")
  }
  
  player_pos == slot
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

### PICK SUGGESTION -------------------------------------------------

suggest_pick_optimal <- function(pool){
  
  scores <- sapply(1:nrow(pool), function(i){
    
    pick <- pool[i,]
    
    remaining <- pool[-i,]
    
    pick$VORP + sum(optimize_roster(remaining)$VORP)
    
  })
  
  pool[which.max(scores),]
}

### SHINY UI --------------------------------------------------------

teams <- paste0("Team ",1:12)

ui <- fluidPage(
  
  titlePanel("Fantasy Draft Assistant"),
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Suggested Pick"),
      textOutput("suggestion")
    ),
    
    mainPanel(
      
      do.call(tabsetPanel, c(
        
        list(
          
          tabPanel("Draft Board",
                   DTOutput("draft_board")
          ),
          
          tabPanel("Best Available",
                   DTOutput("best_available")
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
                     choices = NULL,
                     multiple = FALSE,
                     options = list(
                       placeholder = "Search player..."
                     )
                   ),
                   
                   actionButton(
                     paste0(team,"_draft"),
                     "Draft Player"
                   ),
                   
                   tableOutput(paste0(team,"_roster"))
                   
          )
          
        })
        
      ))
      
    )
    
  )
  
)

### SHINY SERVER ----------------------------------------------------

server <- function(input, output, session){
  
  pool <- reactiveVal(players)
  
  rosters <- reactiveValues()
  
  for(t in teams){
    rosters[[t]] <- rep(NA,15)
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
        
        p <- pool() %>% filter(Name==player)
        
        roster <- rosters[[t]]
        
        idx <- which(is.na(roster))[1]
        
        roster[idx] <- player
        
        rosters[[t]] <- roster
        
        pool(pool() %>% filter(Name!=player))
        
        updateSelectizeInput(
          session,
          paste0(t,"_pick"),
          selected = ""
        )
        
      })
      
      output[[paste0(t,"_roster")]] <- renderTable({
        
        data.frame(
          Slot=roster_slots,
          Player=rosters[[t]]
        )
        
      })
      
    })
    
  }
  
  output$draft_board <- renderDT({
    
    pool() %>%
      arrange(desc(VORP)) %>%
      select(Name,Position,FPTS,VORP)
    
  })
  
  output$best_available <- renderDT({
    
    pool() %>%
      arrange(desc(VORP)) %>%
      head(25) %>%
      select(Name,Position,FPTS,VORP)
    
  })
  
  output$scarcity_plot <- renderPlot({
    
    pool() %>%
      group_by(Position) %>%
      summarize(avg_VORP=mean(VORP, na.rm=TRUE)) %>%
      ggplot(aes(Position,avg_VORP))+
      geom_col()
    
  })
  
  output$suggestion <- renderText({
    
    s <- suggest_pick_optimal(pool())
    
    paste(
      s$Name,
      "(",
      s$Position,
      ")  VORP:",
      round(s$VORP,1)
    )
    
  })
  
}

### RUN APP ---------------------------------------------------------

shinyApp(ui, server)


