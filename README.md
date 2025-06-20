# Nate Beebe's Personal Baseball Projects
This is a collection of my personal projects that I have created using R and publicly available data sources. Below, I will walk through how each project was made and show examples of them being used. 

## Table of Contents
-[Predicting the Outcome of an At-Bat Based on a Specific Batter and Pitcher](#predicting-the-outcome-of-an-at-bat-based-on-a-specific-batter-and-pitcher)

-[A Display of Standard Counting Statistics for Specific Player Seasons](#a-display-of-standard-counting-statistics-for-specific-player-seasons)

-[A Display of a Specific Player's Rate Statistics Compared to League Average](#a-display-of-a-specific-players-rate-statistics-compared-to-league-average)

-[General Lab Assignments for a 400 Level Baseball Statistics Course](#general-lab-assignments-for-a-400-level-baseball-statistics-course)


### Predicting the Outcome of an At-Bat Based on a Specific Batter and Pitcher

My final project in my Baseball Statistics course was to create an "interesting" application. The idea that I developed with a partner was to predict the likelihood that an at-bat ends in a strikeout, walk, hit-by-pitch, single, double, triple, home run, or an out on a ball in play. To do this, we used an article on FanGraphs for inspiration, but we made all the calculations and performed all the checks ourselves. This app breaks down data from 2022-2024 into different groups depending on the rate of occurrence for each considered outcome, then it utilizes linear modeling to predict the likelihood of each outcome. I am still working on refining this application  to create a better model. The R-squared value for predicting each of the outcomes ranged from 0.83 to 0.97, with home runs being the lowest and triples being the highest. 

Below are many screenshots from different examples of the app calculating likelihoods:

![image](https://github.com/user-attachments/assets/8e5a6b21-a776-4929-90c8-4081b723b1cc)
*This image displays our prediction of what would happen if Bobby Witt Jr. (in 2022) met Luis Gil (in 2024).*

![image](https://github.com/user-attachments/assets/412e524a-088e-4856-8fa5-647ea5e4a44e)
*Here we are predicting what would happen in a matchup between C.J. Abrams and Dustin May, all using their 2023 statistics*

![image](https://github.com/user-attachments/assets/96e7218e-1eee-4f09-abee-51a763387fd7)
*Our "About" Tab*

### A Display of Standard Counting Statistics for Specific Player Seasons

For this project, I set out to build an R Shiny application that would easily let me view statistics for any player within the Lahman database. A driving factor in this project was to simply learn how to build an R Shiny application. This project has multiple tabs allowing me to showcase information in both a graph and a table, and there is an "About" tab detailing the functionality of the app and giving any necessary credits to the inspirations that I took. In the "Graph" tab of the app, statistics are limited to one specific player for one specific season. However, when using the "Table" tab, there is a checkbox that is available on the main tab that will alter the results. When selected, the statistics for every player on the selected team for the selected season are displayed. When left unselected, every player from every season for the selected team will be displayed.

Below are screenshots of the app in use for you to view:

![image](https://github.com/user-attachments/assets/1108ef7e-ddae-4165-99f8-e7221156ac48)
*Judge 2022 Graph*

![image](https://github.com/user-attachments/assets/f51230df-3cb8-4d60-89cf-4191b5e7c8bf)
*2022 Yankees table (this continues in both width and length to include all players and each statistic)*

![image](https://github.com/user-attachments/assets/6dd39d4f-cdd4-4aee-84ed-d8a66a782737)
*The "About" tab*


### A Display of a Specific Player's Rate Statistics Compared to League Average
This application is the next phase of the application that displays counting statistics. I found that the graphs that were displayed can be a little misleading, as statistics like RBI can range well over 100, while triples are often less than 5. This application compares traditional rate statistics (batting average, on-base percentage, slugging percentage, and on-base plus slugging) so that all statistics are on a reasonable scale. 

This application displays a player's rate statistics on a graph and displays the league average of that statistic directly next to it. This allows an easy comparison as to whether or not the player was above or below league average. When using the table, I have included a scaled version of these statistics indicating how far above or below league average a player was (for reference, 1 is exactly league average, 1.2 is 20% above league average, and 0.8 is 20% below league average). 

Below are screenshots of the application in use:

![image](https://github.com/user-attachments/assets/662cbd4f-3f3e-483f-a073-f921cb911eb9)
*Shohei Ohtani in 2023 graph*

![image](https://github.com/user-attachments/assets/1d046de6-d934-43ab-8261-894b48c141d6)
*Angels 2023 table (this continues in both width and length to include all players and each statistic)*

![image](https://github.com/user-attachments/assets/0fc8afd4-27e3-476d-9fac-77bc63ec6ce0)
*The "About" tab*

### General Lab Assignments for a 400-Level Baseball Statistics Course

In the Spring 2025 semester at the University of Illinois at Urbana-Champaign, I enrolled in a course titled "STAT 430: Baseball Statistics." Over the course of the semester, there were many lab assignments, and I have included some of them here. Topics covered include run expectancy, simulation, run value of plays, base stealing break-even percentage, and more. I received an "A+" in this course.
