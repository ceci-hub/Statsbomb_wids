# Welcome to our submission for the Women in Sports Data 2022 Hackathon!

For this project, we focus on analysing passes under pressure.

An unsuccessful pass can change the tide of a game, even allowing the rival team to score. We focus on plays where players made a pass while being pressured by an opponent. 

Our team developed a functional interactive shiny app so managers can better understand those plays.

Our prototype provides the following insights:

-	The aggregated performance tab provide a quick interactive analysis of pass completion under pressure by team and player, which the coaching staff can use to rank teams and players. We also provide information on the source of the pass (left/right foot) and its success rate, which could let the player know weak points to improve.
-	The passes tab provides detailed information about each pass under pressure. Each player’s pass origin and destination are plotted in a plot representing a football pitch. Information is further decomposed by opposing teams. 
-	The ball receipts tab provides a look into the recipients of the passes. We explore ball receipts by the recipient’s location on the pitch and distance to the closest opponent. 
-	The next event tab covers the event following a successful ball receipt, hoping to gain insight into the player’s next move.
-	We also provide a methodology tab, where additional details regarding the classifications and metrics are further explained.

## Software requirements
- R/RStudio to run the R shiny app
- R packages: 
  - tidyverse
  - lubridate
  - shiny
  - ggplot2
  - StatsBombR

## Folder structure
 -  data: Contains the dataset as a backup. Our app accesses the data using the library to get the most up-to-date information. 
 -  MT_file: Contains all the scripts which are the base for the app. It shows how we played with the data.
 -  Shiny_app: Contains the Shiny app. The final project.

 

 ## Troubleshoot:
StatsBombR may need further configuration. If you need assistance please read the installation instruction in [StatsBombR Github](https://github.com/statsbomb/StatsBombR) 

