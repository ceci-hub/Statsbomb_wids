library(tidyverse)
library(lubridate)
#library(sf)
library(shiny)
#library(gganatogram)


# Need this the first time you run R for R 4.2.0 due to plotting looking weird
options(shiny.useragg = TRUE)

# Loads the required data into R session
# Do this at the start of session then comment it out to speed things up when playing around with the code
source("analysis_app.R")


ui <- fluidPage(
  
  navbarPage("Performance under pressure",
             
             tabPanel("Introduction",
                      titlePanel("UEFA 2020: Performances Under Pressure"),
                      sidebarLayout(
                        sidebarPanel(p("Authors:"),
                                     p("Mingmei Teo, Cecilia Regueira")),
                        mainPanel(
                          strong("Introduction:", style = "color:blue"),
                          p("Our objective for this project was to examine performances under pressure. Successful movement of the ball on the pitch is a key element in a team's performance. This includes both successful passes and successful ball receipts. Then, what a player does after a successful ball receipt is also insightful."),
                          
                          p("We divided our analysis into 4 tabs:"),
                          
                          tags$ol(
                            tags$li("Aggregated Performance: Provides an overview of pass success rate for passes under pressure, by team and player"), 
                            tags$li("Passes: Provides detailed information about passes under pressure and visualising the pass trajectory and origin"), 
                            tags$li("Ball Receipts: Provides detailed information about ball receipts by location and distance to closest opponent"),
                            tags$li("Next Event: Provides information on what happens after a successful ball receipt")
                          ),
                          p("We thank ", 
                            a("StatsBomb ", 
                              href = "https://statsbomb.com/"), 
                            " for allowing us use of their ",
                            a("Euro 2020 data ",
                              href = "https://statsbomb.com/2021/11/statsbomb-announce-the-release-of-free-statsbomb-360-data-euro-2020-available-now/"),
                            "for the 2022 ", 
                            a("Women in Sports Data ", 
                              href = "https://www.womeninsportsdata.org/"), 
                            "Hackathon."),
                          img(src = "SB - Brand Icon - Colour Positive.png", height = 72),
                          img(src = "SB - Core Wordmark - Colour positive.png", height = 72)
                        )
                      )
             ),
             
             ## Tab 0
             tabPanel("Aggregated Performance",
                      # Title
                      titlePanel("Overall success rate for passes under pressure"),
                      
                      fluidRow(
                        column(3,
                               wellPanel(
                                 # Choose a team
                                 selectInput(inputId = "chosen_team_pla", 
                                             label = "Team:", 
                                             choices = sort(unique(pup$team.name))),
                                 
                                 # Choose a player - default is empty
                                 selectInput(inputId = "chosen_player_pla", 
                                             label = "Player:", 
                                             choices = NULL),
                               ),
                               wellPanel(
                                 # Heading
                                 h4(tags$b("Information for chosen team:")),
                                 # Info for chosen team
                                 uiOutput(outputId = "Team_info_pla"),
                                 br(),
                                 # Heading
                                 h4(tags$b("Information for chosen player:")),
                                 # Info for chosen player
                                 uiOutput(outputId = "Within_info_pla")
                               ),
                        ),
                        column(9, 
                               fluidRow(
                                 # Heading
                                 h4("Success rate by team"),
                                 p("Chosen team is highlighted"), 
                                 # Plot of pass success rate for teams with chosen team highlighted
                                 plotOutput(outputId = "Leage_rate")
                               ),
                               
                               fluidRow(
                                 # Heading
                                 h4("Success rate within chosen team"),
                                 p("Chosen player is highlighted"), 
                                 # Plot of pass success rate for players in chosen team with chosen player highlighted
                                 plotOutput(outputId = "Team_rate")
                               ),
                               # column(4,
                               #        # Heading
                               #        h4("Information for chosen player:"),
                               #        # Info for chosen player
                               #        uiOutput(outputId = "Within_info_pla")
                               # )
                        )
                      )
             ),
             
             # Tab 1
             tabPanel("Passes",
                      # Title
                      titlePanel("Passes under pressure"),
                      
                      fluidRow(
                        column(3,
                               wellPanel(
                                 # Choose a team
                                 selectInput(inputId = "chosen_team_pup", 
                                             label = "Team:", 
                                             choices = sort(unique(pup$team.name))),
                                 
                                 # Choose a player - default is empty
                                 selectInput(inputId = "chosen_player_pup", 
                                             label = "Player:", 
                                             choices = NULL),
                                 
                               ),
                               wellPanel(
                                 # Heading
                                 h4(tags$b("Information for chosen player:")),
                                 # Info for chosen player
                                 uiOutput(outputId = "player_info_pup"),
                               )
                        ),
                        column(9,
                               fluidRow( 
                                 # Heading
                                 h4("Successful/Unsuccessful passes for chosen player in chosen team"),
                                 # Plot of successful/unsuccessful passes under pressure
                                 plotOutput(outputId = "player_plot_pup")
                               ),
                               fluidRow(
                                 column(6,
                                        # Heading
                                        h4("Successful/Unsuccessful passes for chosen player in chosen team split by opposition"),
                                        # Plot of successful/unsuccessful passes under pressure by opposition
                                        plotOutput(outputId = "player_plot_by_opp_pup")),
                                 column(6,
                                        # Heading
                                        h4("Bar chart of successful/unsuccessful passes breakdown split by opposition"),
                                        # Bar chart of successful/unsuccessful passes under pressure breakdown split by opposition
                                        plotOutput(outputId = "player_plot_by_opp_bar_pup")),
                               )
                        )
                      )
             ),
             
             # Tab 2
             tabPanel("Ball Receipts",
                      # Title
                      titlePanel("Ball receipts under pressure"),
                      
                      fluidRow(
                        column(3,
                               wellPanel(
                                 # Choose a team
                                 selectInput(inputId = "chosen_team_br", 
                                             label = "Team:", 
                                             choices = sort(unique(ball_receipts_all$team.name))),
                                 
                                 # Choose a player - default is empty
                                 selectInput(inputId = "chosen_player_br", 
                                             label = "Player:", 
                                             choices = NULL),
                                 
                               ),
                               wellPanel(
                                 # Heading
                                 h4(tags$b("Information for chosen player:")),
                                 # Info for chosen player
                                 uiOutput(outputId = "player_info_br"),
                               )
                        ),
                        column(9,
                               fluidRow(
                                 # Heading
                                 h4("Location of successful/unsuccessful ball receipts for chosen player in chosen team"),
                                 # Plot of ball receipts
                                 plotOutput(outputId = "player_plot_br")
                               ),
                               
                               fluidRow(
                                 # Heading
                                 h4("Location of successful/unsuccessful ball receipts for chosen player in chosen team split by opposition"),
                                 # Plot of ball receipts by opposition
                                 plotOutput(outputId = "player_plot_by_opp_br")
                               ),
                               
                               fluidRow(
                                 column(6, 
                                        # Heading
                                        h4("Breakdown of successful/unsuccessful ball receipts by distance to closest opponent"),
                                        # Bar plot of ball receipts breakdown
                                        plotOutput(outputId = "player_plot_by_dist_br")),
                                 
                                 column(6, 
                                        # Heading
                                        h4("Breakdown of successful/unsuccessful ball receipts by distance to closest opponent split by opposition"),
                                        # Bar plot of ball receipts by opposition
                                        plotOutput(outputId = "player_plot_by_dist_opp_br")
                                 )
                               )
                        )
                      )
             ),
             
             # Tab 3
             tabPanel("Next Event",
                      # Title
                      titlePanel("Next event after successful ball receipt"),
                      
                      fluidRow(
                        column(3,
                               wellPanel(
                                 # Choose a team
                                 selectInput(inputId = "chosen_team_ne", 
                                             label = "Team:", 
                                             choices = sort(unique(ball_receipts_succ_next_events_all$team.name))),
                                 
                                 # Choose a player - default is empty
                                 selectInput(inputId = "chosen_player_ne", 
                                             label = "Player:", 
                                             choices = NULL),
                                 
                               )
                        ),
                        column(9,
                               fluidRow(
                                 # Heading
                                 h4("What does the chosen player do after receiving the ball"),
                                 # Plot of 
                                 plotOutput(outputId = "player_plot_ne")
                               )
                        )
                      )
             ),
             
             # Tab 4
             tabPanel("Methodology",
                      titlePanel("Methodology"),
                      
                      mainPanel(
                        h4("Aggregated Performance:"),
                        p("Here, we explore the success rate of passes under pressure split by team and by player. We use StatsBomb event data and explore standard passes in the run of play. For each team and each player, we calculate the number of successful and unsuccessful passes under pressure and calculate the success rate."),
                        br(),
                        
                        h4("Passes:"),
                        p("In this section, we focus on a chosen player and look more closely at the location of their successful and unsuccessful passes under pressure as well as the trajectory of their passes. We also breakdown the information further by opposition."),
                        p("For the main position of a player, we aggregated position information into 5 categories, Goalkeeper, Defender, Wing, Midfielder and Forward. Then, calculated the percentage of events where a player was in those positions. The position with the highest percentage was deemed the player's main position."),
                        br(),
                        
                        h4("Ball receipts:"),
                        p("Here, we explore successful and unsuccessful ball receipts by filtering StatsBomb event data by ball receipts. To calculate the distance to closest opponent, we use StatsBomb360 data which provides the location of the receiving player and other players in area. As the location of the ball receipt does not always match the location of the actor (receiving player) in the 360 data, we took the average of the ball receipt location and the location of the actor. Then, we calculated the euclidean distance between the average actor/ball receipt location and the location of opponents."),
                        p("For the main position of a player, we aggregated position information into 5 categories, Goalkeeper, Defender, Wing, Midfielder and Forward. Then, calculated the percentage of events where a player was in those positions. The position with the highest percentage was deemed the player's main position."),
                        br(),
                        
                        h4("Next event:"),
                        p("For the next event after a successful ball receipt, we noticed that the event 'Carry' occurred most frequently. From the StatsBomb data specification, 'Carry events are typically by the attacking team and describe a player possessing the ball at their feet, either moving with the ball or standing still.' As we want to know what a player does after a succcessful ball receipt, we skip events where a player was standing still. To account for any minor discrepancies in location during data collection and processing, we deem a player to be standing still if they did not carried the ball more than 1 yard. For these events, we consider the event after the carry event and use that as the next event.")
                        
                      )
             )
             
  )
)

server <- function(input, output, session) {
  
  # Passes under pressure filter by chosen team
  pup_team <- reactive({
    pup %>%
      filter(team.name == input$chosen_team_pup)
  })
  
  observeEvent(pup_team(), {
    choices <- sort(unique(pup_team()$player.name))
    updateSelectInput(inputId = "chosen_player_pup", choices = choices) 
  })
  
  # Filter pup_team data by chosen player
  pup_team_player <- reactive({
    
    pup_team() %>%
      filter(player.name == input$chosen_player_pup)
    
  })
  
  # Filter pup_succ_uns data by chosen player and team
  pup_succ_uns_player <- reactive({
    
    pup_succ_uns %>%
      filter(player.name == input$chosen_player_pup)
  })
  
  # Plot of successful/unsuccessful passes under pressure for chosen player
  output$player_plot_pup <- renderPlot({
    
    # Plot of successful/unsuccecssful passes under pressure
    pup_team_player() %>%
      ggplot() +
      create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
      geom_segment(aes(x = location.x, y = location.y, xend = pass.end_location.x, yend = pass.end_location.y, colour = pass.outcome), lineend = "round", size = 0.6, arrow = arrow(length = unit(0.08, "inches"))) +
      scale_colour_manual(values = c("Complete" = "#00BFC4",
                                     "Incomplete"= "#F8766D")) +
      # Reverses the y axis. Otherwise the data would be plotted on the wrong side of the pitch.
      scale_y_reverse() +
      #coord_fixed(ratio = 105/100)+
      theme(legend.position = "bottom")
    
    
  }, res = 96)
  
  # Info on player
  output$player_info_pup <- renderUI({
    
    str1 <- paste0("<b>Player: </b>", input$chosen_player_pup)
    str2 <- paste0("<b>Main position: </b>", pup_team_player() %>% distinct(main_position) %>% pull(main_position))
    str3 <- paste0("<b>Total number of passes: </b>", pup_succ_uns_player() %>% summarise(total=sum(num_passes)) %>% pull(total))
    str4 <- paste0("<b>Pass success rate: </b>", (pup_succ_uns_player() %>% filter(pass.outcome == "Complete") %>% summarise(total=sum(num_passes)) %>% pull(total)) / (pup_succ_uns_player() %>% summarise(total=sum(num_passes)) %>% pull(total)) * 100, "%")
    #str5 <- paste0("<b>Total number of unsuccessful passes: </b>", pup_succ_uns_player() %>% filter(pass.outcome == "Incomplete") %>% summarise(total=sum(num_passes)) %>% pull(total))
    
    # Combine all strings and add linebreak between them
    HTML(paste(str1, str2, str3, str4, #str5, 
               sep='<br/>'))
    
  })
  
  # Plot of successful/unsuccessful passes under pressure for chosen player split by opposition
  output$player_plot_by_opp_pup <- renderPlot({
    
    req(nrow(pup_team_player()) > 0)
    
    # Plot of successful/unsuccessful passes under pressure split by opposition
    pup_team_player() %>%
      ggplot() +
      create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
      geom_segment(aes(x = location.x, y = location.y, xend = pass.end_location.x, yend = pass.end_location.y, colour = pass.outcome), lineend = "round", size = 0.6, arrow = arrow(length = unit(0.08, "inches"))) +
      facet_wrap(~ OpposingTeam, scales="free") +
      scale_colour_manual(values = c("Complete" = "#00BFC4",
                                     "Incomplete"= "#F8766D")) +
      # Reverses the y axis. Otherwise the data would be plotted on the wrong side of the pitch.
      scale_y_reverse() +
      #coord_fixed(ratio = 105/100)+
      theme(legend.position = "bottom")
    
    
  }, res = 96)
  
  # Bar chart of successful/unsuccessful passes under pressure breakdown by opponent
  output$player_plot_by_opp_bar_pup <- renderPlot({
    
    # Bar chart of successful/unsuccessful passes under pressure breakdown split by opposition
    pup_succ_uns_player() %>%
      ggplot() + 
      geom_col(aes(x=num_passes, y=OpposingTeam, fill=fct_relevel(pass.outcome, "Incomplete", "Complete"))) +
      scale_fill_manual(values = c("Complete" = "#00BFC4",
                                     "Incomplete"= "#F8766D")) +
      labs(x="Number of passes",
           y="Opposition",
           fill="Pass outcome") +
      theme(legend.position = "bottom")
    
  }, res = 96)
  
  
  #############################
  ## For tab 0
  
  ## Agregated results
  league_team_r <- reactive({
    League_data %>%
      mutate(color=ifelse(team.name == input$chosen_team_pla,"red","black"))
  })
  
  ## Player results
  team_r <- reactive({
    team_data %>%
      filter(team.name == input$chosen_team_pla) %>%
      mutate(color=ifelse(player.name == input$chosen_player_pla,"red","black"))
  })
  
  # observeEvent(team_r(), {
  #   choices <- sort(unique(team_r()$player.name))
  #   updateSelectInput(inputId = "chosen_player_pla", choices = choices) 
  # })
  # 
  # team_r <- reactive({
  #   team_r()
  #   
  # })
  
  ## Body parts
  pla_team_r <- reactive({
    pla_r %>%
      filter(team.name == input$chosen_team_pla)
  })
  
  observeEvent(pla_team_r(), {
    choices <- sort(unique(pla_team_r()$player.name))
    updateSelectInput(inputId = "chosen_player_pla", choices = choices) 
  })
  
  
  
  # Filter pup_team data by chosen player
  pla_team_player_r <- reactive({
    
    pla_team_r() %>%
      filter(player.name == input$chosen_player_pla)
    
  })
  
  pla_team_n <- reactive({
    pla_n %>%
      filter(team.name == input$chosen_team_pla)
  })
  
  observeEvent(pla_team_n(), {
    choices <- sort(unique(pla_team_n()$player.name))
    updateSelectInput(inputId = "chosen_player_pla", choices = choices) 
  })
  
  
  pla_team_player_n <- reactive({
    
    pla_team_n() %>%
      filter(player.name == input$chosen_player_pla)
    
  })
  
  output$Leage_rate <- renderPlot({
    league_team_r() %>% 
      ggplot(aes(y=reorder(team.name,Rate),x=Rate,color=color)) +
      geom_point() +
      geom_segment( aes(y=team.name, yend=team.name, x=0, xend=Rate)) +
      theme_bw() +
      xlab("Success Rate (%)") +
      ylab("Team") +
      theme(legend.position = "none") +
      xlim(0,100)
    
  }, res = 96)
  
  # Info on player
  output$Team_info_pla <- renderUI({
    
    str1 <- paste0("<b>Team: </b>", input$chosen_team_pla)
    str2 <- paste0("<b>Total Passes: </b>", League_data %>% filter(team.name==input$chosen_team_pla) %>% select(n))
    str3 <- paste0("<b>Success Rate: </b>", round(League_data %>% filter(team.name==input$chosen_team_pla) %>% select(Rate),2), "%")
    # str4 <- paste0("Total number of successful passes: ", pup_succ_uns_player() %>% filter(pass.outcome == "complete") %>% summarise(total=sum(num_passes)) %>% pull(total))
    # str5 <- paste0("Total number of unsuccessful passes: ", pup_succ_uns_player() %>% filter(pass.outcome == "incomplete") %>% summarise(total=sum(num_passes)) %>% pull(total))
    
    # Combine all strings and add linebreak between them
    HTML(paste(str1, str2, str3, #str4, str5, 
               sep='<br/>'))
    
  })
  
  
  output$Team_rate <- renderPlot({
    team_r() %>% 
      ggplot(aes(y=reorder(player.name,Rate),x=Rate,color=color)) +
      geom_point() +
      geom_segment( aes(y=player.name, yend=player.name, x=0, xend=Rate)) +
      theme_bw() +
      xlab("Success Rate (%)") +
      ylab("Player") +    
      theme(legend.position = "none") +
      xlim(0,100)
    
  }, res = 96)
  
  # Info on player
  output$Within_info_pla <- renderUI({
    
    str1 <- paste0("<b>Player: </b>", input$chosen_player_pla)
    
    str2 <- paste0("<b>Total Passes (success%): </b>", team_data %>% filter(team.name==input$chosen_team_pla & player.name==input$chosen_player_pla) %>% select(n), " (", round(team_data %>% filter(team.name==input$chosen_team_pla & player.name==input$chosen_player_pla) %>% select(Rate),2), "%)")
    
    str3 <- paste0("<b>Left Foot: </b>", pla_n %>%  filter(team.name==input$chosen_team_pla & player.name==input$chosen_player_pla) %>% select(`Left Foot`), " (", round(pla_r  %>% filter(team.name==input$chosen_team_pla & player.name==input$chosen_player_pla) %>% select(`Left Foot`),2), "%)")
    
    str4 <- paste0("<b>Right Foot: </b>", pla_n %>%  filter(team.name==input$chosen_team_pla & player.name==input$chosen_player_pla) %>% select(`Right Foot`), " (", round( pla_r  %>% filter(team.name==input$chosen_team_pla & player.name==input$chosen_player_pla) %>% select(`Right Foot`),2), "%)")
    
    str5 <- paste0("<b>Head: </b>", pla_n %>%  filter(team.name==input$chosen_team_pla & player.name==input$chosen_player_pla) %>% select(Head), " (", round(pla_r  %>% filter(team.name==input$chosen_team_pla & player.name==input$chosen_player_pla) %>% select(Head),2), "%)")
    
    str6 <- paste0("<b>Other: </b>", pla_n %>%  filter(team.name==input$chosen_team_pla & player.name==input$chosen_player_pla) %>% select(Other), " (", round( pla_r  %>% filter(team.name==input$chosen_team_pla & player.name==input$chosen_player_pla) %>% select(Other),2), "%)")               
    
    # Combine all strings and add linebreak between them
    HTML(paste(str1, str2, str3, str4, str5,str6, 
               sep='<br/>'))
    
  })
  
  
  
  #############################
  
  # Ball receipts data frame filter by chosen team name
  ball_receipts_team <- reactive({
    ball_receipts_all %>%
      filter(team.name == input$chosen_team_br)
  })
  
  observeEvent(ball_receipts_team(), {
    choices <- sort(unique(ball_receipts_team()$player.name))
    updateSelectInput(inputId = "chosen_player_br", choices = choices) 
  })
  
  # Filter ball_receipt_team data by chosen player
  ball_receipts_team_player <- reactive({
    
    ball_receipts_team() %>%
      filter(player.name == input$chosen_player_br) %>%
      distinct(id, .keep_all = TRUE)
    
  })
  
  # Plot of ball receipts for chosen player
  output$player_plot_br <- renderPlot({
    # Plot of successful ball receipt locations with colour by distance to closest opponent
    ball_receipts_team_player() %>%
      select(actor.location.x, actor.location.y, min_distance, ball_receipt.outcome.name) %>%
      ggplot() +
      create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
      geom_point(aes(x=actor.location.x, y=actor.location.y, fill=min_distance, shape=ball_receipt.outcome.name), size=2) +
      scale_fill_gradient(low="red", high="green", limits=c(NA, 20), breaks=seq(0,20,5), labels=seq(0,20,5), oob=scales::squish) + 
      scale_shape_manual(values = c(21, 24)) + 
      # Reverses the y axis. Otherwise the data would be plotted on the wrong side of the pitch.
      scale_y_reverse() +
      labs(fill = "Distance to closest opponent",
           shape = "Ball receipt outcome") +
      theme(legend.position = "bottom")
    
  }, res = 96)
  
  # Info on player
  output$player_info_br <- renderUI({
    
    str1 <- paste0("<b>Player: </b>", input$chosen_player_br)
    str2 <- paste0("<b>Main position: </b>", ball_receipts_team_player() %>% distinct(main_position) %>% pull(main_position))
    str3 <- paste0("<b>Number of matches played: </b>", ball_receipts_team_player() %>% distinct(num_matches) %>% pull(num_matches))
    str4 <- paste0("<b>Total number of ball receipts: </b>", ball_receipts_team_player() %>% distinct(total_receipts) %>% summarise(sum(total_receipts)))
    str5 <- paste0("<b>Success rate of all ball receipts: </b>", sprintf("%.2f", ball_receipts_team_player() %>% filter(ball_receipt.outcome.name == "Complete") %>% distinct(success_rate) %>% pull(success_rate) * 100), '%')
    #str7 <- paste0("Average number of ball receipts (assuming at least 3 matches played): ", ball_receipts_team_player() %>% distinct(ave_receipts) %>% summarise(round(sum(ave_receipts))))
    #str8 <- paste0("Average number of successful ball receipts (assuming at least 3 matches played): ", ball_receipts_team_player() %>% filter(ball_receipt.outcome.name == "Complete") %>% distinct(ave_receipts) %>% pull(round(ave_receipts)))
    #str9 <- paste0("Average number of unsuccessful ball receipts (assuming at least 3 matches played): ", ball_receipts_team_player() %>% filter(ball_receipt.outcome.name == "Incomplete") %>% distinct(ave_receipts) %>% pull(ave_receipts))
    
    # Combine all strings and add linebreak between them
    HTML(paste(str1, str2, str3, str4, str5, #str6, str7, str8, str9, 
               sep='<br/>'))
    
  })
  
  # Plot of ball receipts for chosen player split by opposition
  output$player_plot_by_opp_br <- renderPlot({
    
    req(nrow(ball_receipts_team_player()) > 0)
    
    # Plot of successful ball receipt locations with colour by distance to closest opponent split by opposition
    ball_receipts_team_player() %>%
      select(actor.location.x, actor.location.y, min_distance, OpposingTeam, ball_receipt.outcome.name) %>%
      ggplot() +
      create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
      geom_point(data = transform(ball_receipts_team_player(), OpposingTeam=NULL), aes(x=actor.location.x, y=actor.location.y, shape=ball_receipt.outcome.name), size=2, fill = "grey85", colour = "grey85") +
      geom_point(aes(x=actor.location.x, y=actor.location.y, fill=min_distance, shape=ball_receipt.outcome.name), size=2) +
      scale_fill_gradient(low="red", high="green", limits=c(NA, 20), oob=scales::squish) + 
      scale_shape_manual(values = c(21, 24)) + 
      facet_wrap(~ OpposingTeam, scales="free") +
      # Reverses the y axis. Otherwise the data would be plotted on the wrong side of the pitch.
      scale_y_reverse() +
      labs(fill = "Distance to closest opponent") +
      theme(legend.position = "bottom")
    
  }, res = 96)
  
  
  # Horizontal bar plot of successful/unsuccessful ball receipts breakdown for chosen player
  output$player_plot_by_dist_br <- renderPlot({
    
    ball_receipts_team_player() %>%
      group_by(min_distance_bins, ball_receipt.outcome.name) %>%
      tally() %>%
      ungroup() %>%
      ggplot() + 
      geom_col(aes(x=n, y=min_distance_bins, fill=fct_relevel(ball_receipt.outcome.name, "Incomplete", "Complete"))) +
      scale_fill_manual(values = c("Complete" = "#00BFC4",
                                   "Incomplete"= "#F8766D")) +
      labs(x="Number of ball receipts",
           y="Distance to closest opponent (yards)",
           fill="Pass outcome") +
      theme(legend.position = "bottom")
    
  }, res = 96)
  
  # Horizontal bar plot of successful/unsuccessful ball receipts breakdown for chosen player
  output$player_plot_by_dist_opp_br <- renderPlot({
    
    req(nrow(ball_receipts_team_player()) > 0)
    
    ball_receipts_team_player() %>%
      group_by(OpposingTeam, min_distance_bins, ball_receipt.outcome.name) %>%
      tally() %>%
      ungroup() %>%
      ggplot() + 
      geom_col(aes(x=n, y=min_distance_bins, fill=fct_relevel(ball_receipt.outcome.name, "Incomplete", "Complete"))) +
      scale_fill_manual(values = c("Complete" = "#00BFC4",
                                     "Incomplete"= "#F8766D")) +
      labs(x="Number of ball receipts",
           y="Opposition",
           fill="Pass outcome") +
      facet_wrap(~ OpposingTeam, scales = "free") +
      theme(legend.position = "bottom")
    
  }, res = 96)
  
  
  
  
  ##################################
  
  # Filter ball_receipts_succ_next_events_all by chosen team and chosen player
  ball_receipts_next_events_team <- reactive({
    
    ball_receipts_succ_next_events_all %>%
      filter(team.name == input$chosen_team_ne)
    
  })
  
  observeEvent(ball_receipts_next_events_team(), {
    choices <- sort(unique(ball_receipts_next_events_team()$player.name))
    updateSelectInput(inputId = "chosen_player_ne", choices = choices) 
  })
  
  # Filter ball_receipts_next_events_team data by chosen player
  ball_receipts_next_events_team_player <- reactive({
    
    ball_receipts_next_events_team() %>%
      filter(player.name == input$chosen_player_ne)
    
  })
  
  
  
  # Plot of ball receipts for chosen player
  output$player_plot_ne <- renderPlot({
    
    # Horizontal bar plot of the number of the next event types, fill by next event type, split by opposition
    ball_receipts_next_events_team_player() %>%
    group_by(type.name, min_distance_bins) %>%
      mutate(num_type = n()) %>%
      ungroup() %>%
      select(player.name, team.name, main_position, type.name, min_distance_bins, num_type) %>%
      distinct() %>%
      ggplot() +
      geom_col(aes(x=num_type, y=min_distance_bins, fill=reorder(type.name, num_type)), colour="black") +
      scale_fill_discrete(breaks = unique(ball_receipts_next_events_team_player()$type.name)) +
      labs(x="Number of event type",
           y="Distance to closest opponent \n(successful ball receipt)",
           fill="Next event type") +
      theme(legend.position = "bottom")
    
    
  }, res = 96)
  
}

# Call to shinyApp function
shinyApp(ui, server)