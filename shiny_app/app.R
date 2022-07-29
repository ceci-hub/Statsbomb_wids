library(tidyverse)
library(lubridate)
library(sf)
library(shiny)

# Need this the first time you run R for R 4.2.0 due to plotting looking weird
options(shiny.useragg = TRUE)

# Loads the required data into R session
# Do this at the start of session then comment it out to speed things up when playing around with the code
source("analysis_app.R")


ui <- fluidPage(
  
  navbarPage("Performance under pressure",
             # Tab 1
             tabPanel("Passes",
                      # Title
                      titlePanel("Passes under pressure"),
                      
                      sidebarLayout(
                        sidebarPanel(width=3,
                                     # Choose a team
                                     selectInput(inputId = "chosen_team_pup", 
                                                 label = "Team:", 
                                                 choices = sort(unique(pup$team.name))),
                                     
                                     # Choose a player - default is empty
                                     selectInput(inputId = "chosen_player_pup", 
                                                 label = "Player:", 
                                                 choices = NULL),
                                     
                        ),
                        
                        mainPanel(
                          fluidRow(
                            column(9, 
                                   # Heading
                                   h4("Successful/Unsuccessful passes for chosen player in chosen team"),
                                   # Plot of successful/unsuccessful passes under pressure
                                   plotOutput(outputId = "player_plot_pup")),
                            
                            column(3, 
                                   # Heading
                                   h4("Information for chosen player"),
                                   # Info for chosen player
                                   uiOutput(outputId = "player_info_pup"),
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
                      )
                      
             ),
             
             tabPanel("Passes",
                      # Title
                      titlePanel("Passes under pressure"),
                      
                      sidebarLayout(
                        sidebarPanel(width=3,
                                     # Choose a team
                                     selectInput(inputId = "chosen_team_pup", 
                                                 label = "Team:", 
                                                 choices = sort(unique(pup$team.name))),
                                     
                                     # Choose a player - default is empty
                                     selectInput(inputId = "chosen_player_pup", 
                                                 label = "Player:", 
                                                 choices = NULL),
                                     
                        ),
                        
                        mainPanel(
                          fluidRow(
                            column(9, 
                                   # Heading
                                   h4("Successful/Unsuccessful passes for chosen player in chosen team"),
                                   # Plot of successful/unsuccessful passes under pressure
                                   plotOutput(outputId = "player_plot_pup")),
                            
                            column(3, 
                                   # Heading
                                   h4("Information for chosen player"),
                                   # Info for chosen player
                                   uiOutput(outputId = "player_info_pup"),
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
                      )
                      
             ),
             
             
             
             
             
             # Tab 2
             tabPanel("Ball receipts",
                      # Title
                      titlePanel("Ball receipts under pressure"),
                      
                      sidebarLayout(
                        sidebarPanel(width=3,
                                     # Choose a team
                                     selectInput(inputId = "chosen_team_br", 
                                                 label = "Team:", 
                                                 choices = sort(unique(ball_receipts_all$team.name))),
                                     
                                     # Choose a player - default is empty
                                     selectInput(inputId = "chosen_player_br", 
                                                 label = "Player:", 
                                                 choices = NULL),
                                     
                        ),
                        
                        mainPanel(
                          fluidRow(
                            column(9, 
                                   # Heading
                                   h4("Location of successful/unsuccessful ball receipts for chosen player in chosen team"),
                                   # Plot of ball receipts
                                   plotOutput(outputId = "player_plot_br")),
                            
                            column(3, 
                                   # Heading
                                   h4("Information for chosen player"),
                                   # Info for chosen player
                                   uiOutput(outputId = "player_info_br"),
                            ),
                            fluidRow(
                              column(9,
                                     # Heading
                                     h4("Location of ball receipts for chosen player in chosen team split by opposition"),
                                     # Plot of ball receipts by opposition
                                     plotOutput(outputId = "player_plot_by_opp_br"))
                            )
                          )
                        )
                      )
             ),
             # Tab 3
             tabPanel("Next event",
                      # Title
                      titlePanel("Next event after successful ball receipt"),
                      
                      sidebarLayout(
                        sidebarPanel(width=3,
                                     # Choose a team
                                     selectInput(inputId = "chosen_team_ne", 
                                                 label = "Team:", 
                                                 choices = sort(unique(ball_receipts_next_events_all$team.name))),
                                     
                                     # Choose a player - default is empty
                                     selectInput(inputId = "chosen_player_ne", 
                                                 label = "Player:", 
                                                 choices = NULL),
                                     
                        ),
                        
                        mainPanel(
                          fluidRow(
                            column(9,
                                   # Heading
                                   h4("What does the chosen player do after receiving the ball"),
                                   # Plot of 
                                   plotOutput(outputId = "player_plot_ne"))
                          )
                        )
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
    
    str1 <- paste0("Player: ", input$chosen_player_pup)
    str2 <- paste0("Main position: ", pup_team_player() %>% distinct(main_position) %>% pull(main_position))
    str3 <- paste0("Total number of passes: ", pup_succ_uns_player() %>% summarise(total=sum(num_passes)) %>% pull(total))
    str4 <- paste0("Total number of successful passes: ", pup_succ_uns_player() %>% filter(pass.outcome == "complete") %>% summarise(total=sum(num_passes)) %>% pull(total))
    str5 <- paste0("Total number of unsuccessful passes: ", pup_succ_uns_player() %>% filter(pass.outcome == "incomplete") %>% summarise(total=sum(num_passes)) %>% pull(total))
    
    # Combine all strings and add linebreak between them
    HTML(paste(str1, str2, str3, str4, str5, sep='<br/>'))
    
  })
  
  # Plot of successful/unsuccessful passes under pressure for chosen player split by opposition
  output$player_plot_by_opp_pup <- renderPlot({
    
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
      geom_col(aes(x=num_passes, y=OpposingTeam, fill=pass.outcome)) +
      scale_colour_manual(values = c("Complete" = "#00BFC4",
                                     "Incomplete"= "#F8766D")) +
      labs(x="Number of passes",
           y="Opposition",
           fill="Pass outcome")
    
  }, res = 96)
  
  
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
      select(location.x, location.y, min_distance, ball_receipt.outcome.name) %>%
      ggplot() +
      create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
      geom_point(aes(x=location.x, y=location.y, fill=min_distance, shape=ball_receipt.outcome.name), size=2) +
      scale_fill_gradient(low="red", high="green", na.value="grey", limits=c(NA, 20)) + 
      scale_shape_manual(values = c(21, 24)) + 
      # Reverses the y axis. Otherwise the data would be plotted on the wrong side of the pitch.
      scale_y_reverse() +
      labs(fill = "Distance to closest opponent")
    

  }, res = 96)

  # Info on player
  output$player_info_br <- renderUI({
    
    str1 <- paste0("Player: ", input$chosen_player_br)
    str2 <- paste0("Main position: ", ball_receipts_team_player() %>% distinct(main_position) %>% pull(main_position))
    str3 <- paste0("Total number of ball receipts: ", ball_receipts_team_player() %>% distinct(total_receipts) %>% pull(total_receipts))
    str4 <- paste0("Average number of ball receipts (assuming at least 3 matches played): ", ball_receipts_team_player() %>% distinct(ave_receipts) %>% pull(ave_receipts))
    str5 <- paste0("Number of matches played: ", ball_receipts_team_player() %>% distinct(num_matches) %>% pull(num_matches))

    # Combine all strings and add linebreak between them
    HTML(paste(str1, str2, str3, str4, str5, sep='<br/>'))

  })
  
  # Plot of ball receipts for chosen player split by opposition
  output$player_plot_by_opp_br <- renderPlot({
    
    # Plot of successful ball receipt locations with colour by distance to closest opponent split by opposition
    ball_receipts_team_player() %>%
      select(location.x, location.y, min_distance, OpposingTeam, ball_receipt.outcome.name) %>%
      ggplot() +
      create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
      geom_point(data = transform(ball_receipts_team_player(), OpposingTeam=NULL), aes(x=location.x, y=location.y, shape=ball_receipt.outcome.name), size=2, fill = "grey85", colour = "grey85") +
      geom_point(aes(x=location.x, y=location.y, fill=min_distance, shape=ball_receipt.outcome.name), size=2) +
      scale_fill_gradient(low="red", high="green", na.value="grey", limits=c(NA, 20)) + 
      scale_shape_manual(values = c(21, 24)) + 
      facet_wrap(~ OpposingTeam, scales="free") +
      # Reverses the y axis. Otherwise the data would be plotted on the wrong side of the pitch.
      scale_y_reverse() +
      labs(fill = "Distance to closest opponent")
    
  }, res = 96)
  
  
  ##################################
  
  # Filter ball_receipts_next_events_all by chosen team and chosen player
  ball_receipts_next_events_team_player <- reactive({
    
    ball_receipts_next_events_all %>%
      filter(team.name == input$chosen_team_ne,
             player.name == input$chosen_player_ne)
    
  })
  
  
  # Plot of ball receipts for chosen player
  output$player_plot_ne <- renderPlot({
    
    # This only uses data for players with most ball receipts!!! Either change or new tab?
    ball_receipts_next_events_team_player() %>%
      group_by(OpposingTeam, type.name) %>%
      mutate(num_type = n()) %>%
      ungroup() %>%
      select(player.name, team.name, main_position, OpposingTeam, type.name, num_type) %>%
      distinct() %>%
      group_by(OpposingTeam) %>%
      mutate(total_num_type = sum(num_type)) %>%
      ungroup() %>%
      ggplot() +
      geom_col(aes(x=num_type, y=reorder(OpposingTeam, total_num_type), fill=reorder(type.name, num_type)), colour="black") +
      # Reverses the y axis. Otherwise the data would be plotted on the wrong side of the pitch.
      scale_y_reverse() +
      labs(x="Number of event type",
           y="Opposition",
           fill="Next event type")
    
  }, res = 96)
  
}

# Call to shinyApp function
shinyApp(ui, server)

