library(tidyverse)
library(lubridate)
library(sf)
library(shiny)
library(gganatogram)


# Need this the first time you run R for R 4.2.0 due to plotting looking weird
options(shiny.useragg = TRUE)

# Loads the required data into R session
# Do this at the start of session then comment it out to speed things up when playing around with the code
source("analysis_app.R")


ui <- fluidPage(
  
  navbarPage("Performance under pressure",
             
             tabPanel("Introduction",
                      titlePanel("UEFA 2020: Passes Under Preasure "),
                      sidebarLayout(
                        sidebarPanel(p("Authors:"),
                                     p("Mingmei Teo- Cecilia Regueira")),
                        mainPanel(
                          strong("Introduction:", style = "color:blue"),
                          p("Our objetives for this project was to examine passes under preassure. Succesul passes are a key element in a team performance. "),
                          
                          p("We divided our results into 4 tabs:"),
                          
                          p("Aggregated Performance: Provides a overview by teams and player"),
                          p("Passes: Provides detailed information about the plays looking at pass trayectory and origin"),
                          p("Ball Receipts: Provides detailed information about the plays at pass recipient and where player where positioned"),
                          p("Next event: xxxxxx"),

                        )
                      )          
                      
                      
                      
             ),
             
             ## Tab 0
             tabPanel("Agregated Performance",
                      # Title
                      titlePanel("Select team and player "),
                      
                      sidebarLayout(
                        sidebarPanel(width=3,
                                     # Choose a team
                                     selectInput(inputId = "chosen_team_pla", 
                                                 label = "Team:", 
                                                 choices = sort(unique(pup$team.name))),
                                     
                                     # Choose a player - default is empty
                                     selectInput(inputId = "chosen_player_pla", 
                                                 label = "Player:", 
                                                 choices = NULL),
                                     
                        ),
                        mainPanel(
                          fluidRow(
                            column(9, 
                                   # Heading
                                   h4("Team Aggregated Analitics"),
                                   # Plot of successful/unsuccessful passes under pressure
                                   plotOutput(outputId = "Leage_rate")),
                            
                            column(3, 
                                   # Heading
                                   h4("Information choosen Team:"),
                                   # Info for chosen player
                                   uiOutput(outputId = "Team_info_pla"),
                            ),
                            fluidRow(
                              column(9,
                                     # Heading
                                     h4("Performance within team:"),
                                     # Plot of successful/unsuccessful passes under pressure by opposition
                                     plotOutput(outputId = "Team_rate")
                              ),
                              column(3,
                                     # Heading
                                     h4("Information choosen player:"),
                                     # Bar chart of successful/unsuccessful passes under pressure breakdown split by opposition
                                     tableOutput(outputId = "Within_info_pla")
                              ),
                            )
                          )
                        )
                      )
                      
             ),
             
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
    # str4 <- paste0("Total number of successful passes: ", pup_succ_uns_player() %>% filter(pass.outcome == "complete") %>% summarise(total=sum(num_passes)) %>% pull(total))
    # str5 <- paste0("Total number of unsuccessful passes: ", pup_succ_uns_player() %>% filter(pass.outcome == "incomplete") %>% summarise(total=sum(num_passes)) %>% pull(total))
    str4 <- paste0("Total number of successful passes: ", pup_succ_uns_player() %>% filter(pass.outcome == "complete") %>% summarise(total=sum(num_passes)) %>% select(total))
    str5 <- paste0("Total number of unsuccessful passes: ", pup_succ_uns_player() %>% filter(pass.outcome == "incomplete") %>% summarise(total=sum(num_passes)) %>% select(total))
    
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
           fill="Pass outcome")+theme_light()
    
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
    league_team_r() %>% ggplot(aes(y=reorder(team.name,Rate),x=Rate,color=color))+
    geom_point()+geom_segment( aes(y=team.name, yend=team.name, x=0, xend=Rate))+theme_bw()+xlab("Succes Rate")+ylab("Team")+
      theme(legend.position = "none")+xlim(0,100)
  
  }, res = 96)
  
    # Info on player
  output$Team_info_pla <- renderUI({

    str1 <- paste0("Team: ", input$chosen_team_pla)
    str2 <- paste0("Total Passes: ", League_data %>% filter(team.name==input$chosen_team_pla) %>% select(n))
    str3 <- paste0("Sucess Rate: ", round(League_data %>% filter(team.name==input$chosen_team_pla) %>% select(Rate),2))
    # str4 <- paste0("Total number of successful passes: ", pup_succ_uns_player() %>% filter(pass.outcome == "complete") %>% summarise(total=sum(num_passes)) %>% pull(total))
    # str5 <- paste0("Total number of unsuccessful passes: ", pup_succ_uns_player() %>% filter(pass.outcome == "incomplete") %>% summarise(total=sum(num_passes)) %>% pull(total))

    # Combine all strings and add linebreak between them
    HTML(paste(str1, str2, str3, #str4, str5, 
               sep='<br/>'))

  })
  
  
  output$Team_rate <- renderPlot({
    team_r() %>% ggplot(aes(y=reorder(player.name,Rate),x=Rate,color=color))+
      geom_point()+geom_segment( aes(y=player.name, yend=player.name, x=0, xend=Rate))+theme_bw()+xlab("Succes Rate")+ylab("Player")+    
      theme(legend.position = "none")+xlim(0,100)
    
  }, res = 96)
  
  # Info on player
  output$Within_info_pla <- renderUI({
    
    str1 <- paste0("Player: ", input$chosen_player_pla)
    str2 <- paste0("Total Passes: ", team_data %>% filter(team.name==input$chosen_team_pla &
                                                          player.name==input$chosen_player_pla) %>% select(n),
                   
                   " (",
                   round(team_data %>% filter(team.name==input$chosen_team_pla &
                                              player.name==input$chosen_player_pla)  %>% select(Rate),2),
                   "%)"
                   )
    str3 <- paste0("Left Foot: ",
                   pla_n %>%  filter(team.name==input$chosen_team_pla &
                                   player.name==input$chosen_player_pla) %>% select(`Left Foot`),
                   
                   
                   " (",
                   round(pla_r  %>% filter(team.name==input$chosen_team_pla &
                                  player.name==input$chosen_player_pla) %>% select(`Left Foot`),2),
                   "%)"
                   )
    
    str4 <- paste0("Rigth Foot: ",
                   pla_n %>%  filter(team.name==input$chosen_team_pla &
                                       player.name==input$chosen_player_pla) %>% select(`Right Foot`),
                   
                   
                   " (",
                   round( pla_r  %>% filter(team.name==input$chosen_team_pla &
                                       player.name==input$chosen_player_pla) %>% select(`Right Foot`),2),
                   "%)"
    )
                  
    
    str5 <- paste0("Head: ",
                   pla_n %>%  filter(team.name==input$chosen_team_pla &
                                       player.name==input$chosen_player_pla) %>% select(Head),
                   
                   
                   " (",
                   round(pla_r  %>% filter(team.name==input$chosen_team_pla &
                                       player.name==input$chosen_player_pla) %>% select(Head),2),
                   "%)"
    )
                   
    
    
    str6 <- paste0("Others: ",
                   pla_n %>%  filter(team.name==input$chosen_team_pla &
                                       player.name==input$chosen_player_pla) %>% select(Other),
                   
                   
                   " (",
                   round( pla_r  %>% filter(team.name==input$chosen_team_pla &
                                       player.name==input$chosen_player_pla) %>% select(Other),2),
                   "%)"
    )               
    

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

