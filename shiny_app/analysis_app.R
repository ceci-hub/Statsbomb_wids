#install.packages("devtools")
#devtools::install_github("statsbomb/SDMTools")
#devtools::install_github("statsbomb/StatsBombR")

library(tidyverse)
library(StatsBombR)
library(ggplot2)


#https://github.com/statsbomb/StatsBombR/issues/18
source("createpitch.R")

# Load all free competitions available from StatsBomb data
comp <- FreeCompetitions()

# Load all matches for the competitions
matches <- FreeMatches(comp)

# Filter matches of interest to UEFA Euro only

matches <- matches %>%
  filter(competition.competition_name=="UEFA Euro")

# Load free 360 data for Euro matches (run function in parallel)
data360 <- free_allevents_360(MatchesDF = matches, Parallel = T)

#Next, we're going to pull the standard Euro 2020 event data into a separate dataframe.
# Load standard Euro 2020 event data (run function in parallel)
events <- free_allevents(MatchesDF = matches, Parallel = T)

# Clean the dataset
events <- allclean(events)

# Add opposing team as a variable to dataset
events <- get.opposingteam(events)

# Rename "event_uuid" column to "id"
data360 <- data360 %>% rename(id = event_uuid)

# Join 360 data with standard event data
events <- events %>% left_join(data360, by = c("id" = "id"))

# Rename match_id and remove match_id.y from dataframe (repeat)
events <- events %>% 
  rename(match_id = match_id.x,
         competition_id = competition_id.x,
         season_id = season_id.x) %>% 
  select(-c(match_id.y, competition_id.y, season_id.y))

##################################################################

# Passes under pressure
pup <- events %>%
  # Filter by Pass and "standard passes in the run of play" and under pressure
  filter(type.name == "Pass" & is.na(pass.type.name),
         under_pressure == "TRUE") %>%
  mutate(
    pass.outcome = as.factor(if_else(is.na(pass.outcome.name), "Complete", "Incomplete"))) %>%
  select(c(team.name, player.name, position.name, type.name, under_pressure, pass.end_location.x, pass.end_location.y, pass.outcome, match_id, location.x, location.y, OpposingTeam))

# Rename position.name to Forward, Midfield, Back, Wing
# Determine player's main (most common) position in games
# Add player's main position to pup dataframe
pup <- pup %>%
  mutate(position_name = 
           case_when(str_detect(position.name, "Forward") ~ "Forward",
                     str_detect(position.name, "Midfield") ~ "Midfield",
                     str_detect(position.name, "Back") ~ "Defender",
                     str_detect(position.name, "Wing") ~ "Wing",
                     TRUE ~ position.name)) %>%
  group_by(player.name, position_name) %>%
  mutate(position_count = n()) %>%
  ungroup() %>%
  group_by(player.name) %>%
  mutate(position_total_count = n(), 
         position_percentage = position_count/position_total_count,
         main_position = position_name[which.max(position_percentage)]) %>%
  ungroup()


#The code to find successful and unsuccessful passes:
pup_succ_uns <- pup %>%
  group_by(player.name, OpposingTeam, pass.outcome) %>%
  tally(name="num_passes") %>%
  ungroup()


#####################################################################

## Player Stats
# Passes under pressure

League_data <- events %>%
  # Filter by Pass and "standard passes in the run of play" and under pressure
  filter(type.name == "Pass" & is.na(pass.type.name),
         under_pressure == "TRUE") %>%
  mutate(
    pass.outcome = as.factor(if_else(is.na(pass.outcome.name), "Complete", "Incomplete")),
    pass.body_part.name=ifelse(is.na(pass.body_part.name),"Other",
                               ifelse(pass.body_part.name=="No Touch","Other",
                                      pass.body_part.name      
                               ))) %>%  
  group_by(team.name) %>%
  summarise(Rate=mean(pass.outcome=="Complete")*100,
            n=n()) %>% ungroup() 



# team_data <- events %>%
#   # Filter by Pass and "standard passes in the run of play" and under pressure
#   filter(type.name == "Pass" & is.na(pass.type.name),
#          under_pressure == "TRUE") %>%
#   mutate(
#     pass.outcome = as.factor(if_else(is.na(pass.outcome.name), "Complete", "Incomplete")),
#     pass.body_part.name=ifelse(is.na(pass.body_part.name),"Other",
#                                ifelse(pass.body_part.name=="No Touch","Other",
#                                       pass.body_part.name      
#                                ))) %>%  
#   group_by(team.name,player.name) %>%
#   summarise(Rate=mean(pass.outcome=="Complete")*100,
#             n=n()) %>% ungroup() 

# Team succes.
team_data <- events %>%
  # Filter by Pass and "standard passes in the run of play" and under pressure
  filter(type.name == "Pass" & is.na(pass.type.name),
         under_pressure == "TRUE") %>%
  mutate(
    pass.outcome = as.factor(if_else(is.na(pass.outcome.name), "Complete", "Incomplete")),
    pass.body_part.name=ifelse(is.na(pass.body_part.name),"Other",
                               ifelse(pass.body_part.name=="No Touch","Other",
                                      pass.body_part.name      
                               ))) %>%  
  group_by(team.name,player.name) %>%
  summarise(Rate=mean(pass.outcome=="Complete")*100,
            n=n()) %>% ungroup() 

pla_r <- events %>%
  # Filter by Pass and "standard passes in the run of play" and under pressure
  filter(type.name == "Pass" & is.na(pass.type.name),
         under_pressure == "TRUE") %>%
  mutate(
    pass.outcome = as.factor(if_else(is.na(pass.outcome.name), "Complete", "Incomplete")),
    pass.body_part.name=ifelse(is.na(pass.body_part.name),"Other",
                               ifelse(pass.body_part.name=="No Touch","Other",
                                      pass.body_part.name      
                               ))) %>%  
  group_by(team.name,player.name,pass.body_part.name) %>%
  summarise(Rate=mean(pass.outcome=="Complete")*100,
            n=n()) %>% ungroup() %>%
  pivot_wider(id_cols =1:2,names_from = pass.body_part.name,values_from = Rate)


pla_r <- events %>%
  # Filter by Pass and "standard passes in the run of play" and under pressure
  filter(type.name == "Pass" & is.na(pass.type.name),
         under_pressure == "TRUE") %>%
  mutate(
    pass.outcome = as.factor(if_else(is.na(pass.outcome.name), "Complete", "Incomplete")),
    pass.body_part.name=ifelse(is.na(pass.body_part.name),"Other",
                               ifelse(pass.body_part.name=="No Touch","Other",
                                      pass.body_part.name      
                               ))) %>%  
  group_by(team.name,player.name,pass.body_part.name) %>%
  summarise(Rate=mean(pass.outcome=="Complete")*100,
            n=n()) %>% ungroup() %>%
  pivot_wider(id_cols =1:2,names_from = pass.body_part.name,values_from = Rate)

pla_n <- events %>%
  # Filter by Pass and "standard passes in the run of play" and under pressure
  filter(type.name == "Pass" & is.na(pass.type.name),
         under_pressure == "TRUE") %>%
  mutate(
    pass.outcome = as.factor(if_else(is.na(pass.outcome.name), "Complete", "Incomplete")),
    pass.body_part.name=ifelse(is.na(pass.body_part.name),"Other",
                               ifelse(pass.body_part.name=="No Touch","Other",
                                      pass.body_part.name      
                               )))  %>%  
  group_by(team.name,player.name,pass.body_part.name) %>%
  summarise(Rate=mean(pass.outcome=="Complete")*100,
            n=n()) %>% ungroup() %>%
  pivot_wider(id_cols =1:2,names_from = pass.body_part.name,values_from = n)

pla_r[is.na(pla_r)]=0
pla_n[is.na(pla_n)]=0


############################


# All ball receipts
ball_receipts_all <- events %>%
  filter(type.name == "Ball Receipt*") %>% 
  select(c(id, index, team.name, play_pattern.name, player.name, position.name, ball_receipt.outcome.name, match_id, location.x, location.y, OpposingTeam, freeze_frame, visible_area)) %>%
  mutate(ball_receipt.outcome.name = replace_na(ball_receipt.outcome.name, "Complete"))

# Calculate number of matches for each player
num_matches_player <- ball_receipts_all %>%
  select(match_id, player.name) %>%
  group_by(player.name) %>% 
  distinct(match_id) %>%
  summarise(num_matches = n())

# Join number of matches to ball receipts dataframe
ball_receipts_all <- ball_receipts_all %>%
  left_join(num_matches_player, by="player.name")

# Unnest 360 freeze frame data
ball_receipts_all <- ball_receipts_all %>%
  unnest(freeze_frame) %>%
  mutate(ff_location.x = (map(location, 1)), ff_location.y = (map(location, 2))) %>%
  select(-location) %>%
  mutate(ff_location.x = as.numeric(ifelse(ff_location.x == "NULL", NA, ff_location.x)), ff_location.y = as.numeric(ifelse(ff_location.y == "NULL", NA, ff_location.y))) 

# Location of ball receipt by location.x and location.y does not always match the location of the actor ff_location.x and ff_location.y
# Maybe take the average of their location?
ball_receipts_all <- ball_receipts_all %>%
  mutate(Player_Type_Key = case_when(actor==TRUE & teammate==TRUE ~ "Actor",
                                     teammate==TRUE ~ "Teammate",
                                     teammate==FALSE & keeper==FALSE ~ "Opponent",
                                     keeper==TRUE & teammate==FALSE ~ "Goalkeeper")) %>%
  mutate(actor.location.x = 
           case_when(Player_Type_Key == "Actor" ~ (location.x + ff_location.x)/2),
         actor.location.y = 
           case_when(Player_Type_Key == "Actor" ~ (location.y + ff_location.y)/2))

# id=a3609266-2692-4a12-8d99-5553daa6b677 has 2 actors
# calculate distance between ball receipt location and freeze frame actor location
# choose the one with smaller distance
temp_br_calc <- ball_receipts_all %>%
  filter(id == "a3609266-2692-4a12-8d99-5553daa6b677",
         Player_Type_Key == "Actor") %>%
  mutate(sqrt( (location.x - ff_location.x)^2 + (location.y - ff_location.y)^2 ))

# Remove the entry with larger distance
ball_receipts_all <- ball_receipts_all %>%
  filter(!(id == "a3609266-2692-4a12-8d99-5553daa6b677" & Player_Type_Key == "Actor" & ff_location.x == 9.996438 & ff_location.y == 75.69223))

# Calculate distance (euclidean distance) between average actor/ball receipt and location of other players
ball_receipts_all <- ball_receipts_all %>%
  group_by(id) %>%
  fill(actor.location.x, actor.location.y, .direction = "downup") %>%
  ungroup() %>%
  mutate(distance = if_else(Player_Type_Key == "Actor", NA_real_, sqrt( (actor.location.x - ff_location.x)^2 + (actor.location.y - ff_location.y)^2 )))

# Drop events where there are no Actors and single Actor only
single_actor_ids <- ball_receipts_all %>% 
  group_by(id) %>% 
  filter(all(actor == TRUE)) %>% 
  select(id)

no_actor_ids <- ball_receipts_all %>% 
  group_by(id) %>% 
  filter(all(Player_Type_Key != "Actor")) %>%
  distinct(id)

# Calculate minimum distance between actor/ball receipt and opponent
# Need to filter by "Opponent" in Player_Type_Key
ball_receipts_all_min_dist <- ball_receipts_all %>% 
  filter(!(id %in% single_actor_ids$id),
         !(id %in% no_actor_ids$id)) %>%
  filter(Player_Type_Key == "Opponent") %>%
  group_by(id) %>%
  summarise(min_distance = min(distance, na.rm = TRUE))

# Events where there are no opponents
no_opp_ids <- ball_receipts_all %>% 
  group_by(id) %>% 
  filter(all(Player_Type_Key != "Opponent")) %>% 
  distinct(id) %>%
  ungroup()

# Set min_distance to 120 (something arbitrarily large) for events with no opponents and add to ball_receipts_all_min_dist dataframe
ball_receipts_all_min_dist <- bind_rows(ball_receipts_all_min_dist, tibble(no_opp_ids, min_distance = 120))


# Filter out events with no Actors and events with a single Actor only
# Join the minimum distance with the ball_receipts dataframe
ball_receipts_all <- ball_receipts_all %>% 
  filter(!(id %in% single_actor_ids$id),
         !(id %in% no_actor_ids$id)) %>%
  left_join(ball_receipts_all_min_dist, by = "id") %>%
  # Remove the columns no longer of interest(teammate, actor, keeper, visible area, ff_location.x, ff_location.y)
  select(-c(teammate, actor, keeper, visible_area, ff_location.x, ff_location.y, distance)) %>%
  # Select only "Actor" as unnest(freeze_frame) repeated ids for all locations of players in freeze_frame and we are no longer interested in them
  filter(Player_Type_Key == "Actor")


# Obtain dataframe with number of successful/unsuccessful ball receipts for each player for each match, the total number of successful/unsuccessful ball receipts for each player for the tournament, the average number of successful/unsuccessful ball receipts for each player
ball_receipts_all_player <- ball_receipts_all %>%
  # Want to calculate the number of successful/unsuccessful ball receipts for each player for each match
  group_by(match_id, player.name, ball_receipt.outcome.name) %>%
  mutate(num_receipts = n()) %>%
  ungroup() %>%
  # Want to calculate total number of ball receipts for each player
  group_by(player.name, ball_receipt.outcome.name) %>%
  mutate(total_receipts = n()) %>%
  ungroup() %>%
  # Average number of ball receipts for each player
  mutate(ave_receipts = total_receipts/num_matches) %>%
  select(player.name, match_id, ball_receipt.outcome.name, total_receipts, ave_receipts, num_receipts) %>% 
  distinct()

# Join number of ball receipts dataframe to ball_receipts dataframe
ball_receipts_all <- ball_receipts_all %>%
  left_join(ball_receipts_all_player, by=c("player.name", "match_id", "ball_receipt.outcome.name"))

# Rename position.name to Forward, Midfield, Back, Wing
ball_receipts_all <- ball_receipts_all %>%
  mutate(position_name = 
           case_when(str_detect(position.name, "Forward") ~ "Forward",
                     str_detect(position.name, "Midfield") ~ "Midfield",
                     str_detect(position.name, "Back") ~ "Defender",
                     str_detect(position.name, "Wing") ~ "Wing",
                     TRUE ~ position.name)) %>%
  group_by(player.name, position_name) %>%
  mutate(position_count = n()) %>%
  ungroup() %>%
  group_by(player.name) %>%
  mutate(position_total_count = n(), 
         position_percentage = position_count/position_total_count,
         main_position = position_name[which.max(position_percentage)]) %>%
  ungroup()


# What does player do with the ball after receiving it?

# Determine what the next event after ball receipt is
ball_receipts_next_events_all <- tibble()
for (m_id in unique(most_ball_receipts_all$match_id)) {

  most_ball_receipts_all_sub <- most_ball_receipts_all %>%
    filter(match_id == m_id)

  next_events_sub <- events %>%
    filter(match_id == m_id,
           index %in% (most_ball_receipts_all_sub$index+1)) %>%
    select(type.name)

  ball_receipts_next_events_all <- bind_rows(ball_receipts_next_events_all, bind_cols(most_ball_receipts_all_sub, next_events_sub))

}

# Plot of ball receipts split by opposition for player with most ball receipts for chosen team (team_name), shape of each ball receipt is based on the next event type, colour of each ball receipt is based on distance to closest opponent
ball_receipts_next_events_all %>%
  ggplot() +
  create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
  geom_point(aes(x=location.x, y=location.y, fill=min_distance, shape=type.name, color=min_distance), size=2) +
  scale_fill_gradient(low="red", high="green", na.value="grey", limits=c(NA, 20)) +
  scale_color_gradient(low="red", high="green", na.value="grey", limits=c(NA, 20), guide="none") +
  #scale_shape_manual(values = c(21, 22, 23, 24, 25, 8)) +
  facet_wrap(~ player.name, scales="free") +
  labs(fill = "Distance to closest opponent",
       color = "",
       shape = "Next event type",
       title = paste0("Ball receipts for player with most total ball receipts for ", team_name, " by opposition (", most_ball_receipt_team_name %>% distinct(player.name) %>% pull(player.name), ")"))


# Horizontal bar plot of the number of the next event types, fill by next event type, for player with most ball receipts on each team
ball_receipts_next_events_all %>%
  group_by(player.name, type.name) %>%
  mutate(num_type = n()) %>%
  ungroup() %>%
  select(player.name, team.name, main_position, type.name, num_type) %>%
  distinct() %>%
  group_by(player.name) %>%
  mutate(total_num_type = sum(num_type)) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(x=num_type, y=reorder(player.name, total_num_type), fill=reorder(type.name, num_type)), colour="black") +
  labs(x="Number of event type",
       y="Player",
       fill="Next event type",
       title=paste0("Next event type after ball receipt, split by event type, for player in each team with most overall ball receipts"))


# For all players
# Determine what the next event after ball receipt is
ball_receipts_next_events_all <- tibble()
for (m_id in unique(ball_receipts$match_id)) {

  ball_receipts_sub <- ball_receipts %>%
    filter(match_id == m_id) %>%
    distinct(id, .keep_all = TRUE)

  next_events_sub <- events %>%
    filter(match_id == m_id,
           index %in% (ball_receipts_sub$index+1)) %>%
    select(type.name, carry.end_location.x, carry.end_location.y)

  ball_receipts_next_events_all <- bind_rows(ball_receipts_next_events_all, bind_cols(ball_receipts_sub, next_events_sub))

}


ball_receipts_next_events_all <- ball_receipts_next_events_all %>%
  mutate(carry_distance = sqrt( (location.x - carry.end_location.x)^2 + (location.y - carry.end_location.y)^2 ))


ball_receipts_next_events_all %>%
  filter(type.name == "Carry",
         player.name == "John Stones") %>%
  group_by(OpposingTeam) %>%
  mutate(num_type = n()) %>%
  ungroup() %>%
  select(player.name, team.name, main_position, OpposingTeam, type.name, num_type) %>%
  distinct() %>%
  ggplot() +
  geom_col(aes(x=num_type, y=type.name, fill=OpposingTeam), colour="black") +
  labs(x="Number of event type",
       y="Next event type",
       fill="Opposition",
       title=paste0("Next event type after ball receipt, split by event type and opposition, for ", unique(ball_receipts_next_events$player.name), '(', unique(ball_receipts_next_events$team.name), ')'))




ball_receipts_next_events_all %>%
  filter(type.name == "Carry",
         player.name == "John Stones",
         carry_distance >= 1) %>%
  group_by(OpposingTeam) %>%
  mutate(num_type = n()) %>%
  ungroup() %>%
  select(player.name, team.name, main_position, OpposingTeam, type.name, num_type) %>%
  distinct() %>%
  ggplot() +
  geom_col(aes(x=num_type, y=type.name, fill=OpposingTeam), colour="black") +
  labs(x="Number of event type",
       y="Next event type",
       fill="Opposition",
       title=paste0("Next event type after ball receipt, split by event type and opposition, for ", unique(ball_receipts_next_events$player.name), '(', unique(ball_receipts_next_events$team.name), ')'))







# 
# # #####################################################################
# 
# # Successful ball receipts
# # Want to quantify how many ball receipts are in space as a way to quantify how well a defence is performing?
# ball_receipts <- events %>%
#   filter(type.name == "Ball Receipt*",
#          is.na(ball_receipt.outcome.name)) %>%
#   # select_if(~any(!is.na(.))) %>% # Remove columns with all NAs
#   select(c(id, index, team.name, play_pattern.name, player.name, position.name, ball_receipt.outcome.name, match_id, location.x, location.y, OpposingTeam, freeze_frame, visible_area))
# 
# # Unnest 360 freeze frame data
# ball_receipts <- ball_receipts %>%
#   unnest(freeze_frame) %>%
#   mutate(ff_location.x = (map(location, 1)), ff_location.y = (map(location, 2))) %>%
#   select(-location) %>%
#   mutate(ff_location.x = as.numeric(ifelse(ff_location.x == "NULL", NA, ff_location.x)), ff_location.y = as.numeric(ifelse(ff_location.y == "NULL", NA, ff_location.y)))
# 
# # Location of ball receipt by location.x and location.y does not always match the location of the actor ff_location.x and ff_location.y
# # Maybe take the average of their location?
# ball_receipts <- ball_receipts %>%
#   mutate(Player_Type_Key = case_when(actor==TRUE & teammate==TRUE ~ "Actor",
#                                      teammate==TRUE ~ "Teammate",
#                                      teammate==FALSE & keeper==FALSE ~ "Opponent",
#                                      keeper==TRUE & teammate==FALSE ~ "Goalkeeper")) %>%
#   mutate(actor.location.x =
#            case_when(Player_Type_Key == "Actor" ~ (location.x + ff_location.x)/2),
#          actor.location.y =
#            case_when(Player_Type_Key == "Actor" ~ (location.y + ff_location.y)/2))
# 
# # Calculate distance (euclidean distance) between average actor/ball receipt and location of opponent(s)
# ball_receipts <- ball_receipts %>%
#   group_by(id) %>%
#   fill(actor.location.x, actor.location.y, .direction = "downup") %>%
#   ungroup() %>%
#   mutate(distance = if_else(Player_Type_Key == "Actor", NA_real_, sqrt( (actor.location.x - ff_location.x)^2 + (actor.location.y - ff_location.y)^2 )))
# 
# # Drop events where there are no Actors and single Actor only
# single_actor_ids <- ball_receipts %>%
#   group_by(id) %>%
#   filter(all(actor == TRUE)) %>%
#   select(id)
# 
# no_actor_ids <- ball_receipts %>%
#   group_by(id) %>%
#   filter(all(Player_Type_Key != "Actor")) %>%
#   distinct(id)
# 
# 
# # Calculate minimum distance between actor/ball receipt and opponent
# # Need to filter by "Opponent" in Player_Type_Key
# ball_receipts_min_dist <- ball_receipts %>%
#   filter(!(id %in% single_actor_ids$id),
#          !(id %in% no_actor_ids$id)) %>%
#   filter(Player_Type_Key == "Opponent") %>%
#   group_by(id) %>%
#   summarise(min_distance = min(distance, na.rm = TRUE))
# 
# # Join the minimum distance with the ball_receipts dataframe
# ball_receipts <- ball_receipts %>%
#   left_join(ball_receipts_min_dist, by = "id")
# 
# 
# # Obtain dataframe with number of ball receipts for each player for each match, the total number of ball receipts for each player for the tournament, the average number of ball receipts for each player
# ball_receipt_player <- ball_receipts %>%
#   # Distinct "id" as unnest(freeze_frame) repeated ids for all locations of players in freeze_frame, keep all columns
#   distinct(id, .keep_all = TRUE) %>%
#   # Want to calculate the number of ball receipts for each player for each match
#   group_by(match_id, player.name) %>%
#   mutate(num_receipts = n()) %>%
#   ungroup() %>%
#   # Want to calculate total number of ball receipts for each player
#   group_by(player.name) %>%
#   mutate(total_receipts = n()) %>%
#   # Distinct match_id because we are only interested in the number of ball receipts for each match and num_receipts is duplicated for all events in a match. Only need to know this value once
#   distinct(match_id, .keep_all = TRUE) %>%
#   # Want to calculate the number of matches played for each player and the average number of ball receipts for each player
#   mutate(num_matches = n(),
#          ave_receipts = mean(num_receipts)) %>%
#   select(player.name, match_id, total_receipts, ave_receipts, num_receipts, num_matches) %>%
#   distinct()
# 
# # Join number of ball receipts dataframe to ball_receipts dataframe
# ball_receipts <- ball_receipts %>%
#   left_join(ball_receipt_player, by=c("player.name", "match_id"))
# 
# # Rename position.name to Forward, Midfield, Back, Wing
# ball_receipts <- ball_receipts %>%
#   mutate(position_name =
#            case_when(str_detect(position.name, "Forward") ~ "Forward",
#                      str_detect(position.name, "Midfield") ~ "Midfield",
#                      str_detect(position.name, "Back") ~ "Defender",
#                      str_detect(position.name, "Wing") ~ "Wing",
#                      TRUE ~ position.name)) %>%
#   group_by(player.name, position_name) %>%
#   mutate(position_count = n()) %>%
#   ungroup() %>%
#   group_by(player.name) %>%
#   mutate(position_total_count = n(),
#          position_percentage = position_count/position_total_count,
#          main_position = position_name[which.max(position_percentage)]) %>%
#   ungroup()
# 
# # Plot of ball receipts for all matches for player with most overall ball receipts on each team coloured by distance to closest opponent
# ball_receipts %>%
#   group_by(team.name) %>%
#   slice_max(total_receipts) %>%
#   ungroup() %>%
#   distinct(id, .keep_all = TRUE) %>%
#   unite(player_team, c("player.name", "team.name"), sep = "=", remove = FALSE) %>%
#   unite(player_team, c("player_team", "main_position"), sep = "\n") %>%
#   select(player_team, location.x, location.y, min_distance) %>%
#   ggplot() +
#   create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
#   geom_point(aes(x=location.x, y=location.y, fill=min_distance), shape=21, size=2) +
#   scale_fill_gradient(low="red", high="green", na.value="grey", limits=c(NA, 20)) +
#   facet_wrap(~ player_team, scales="free") +
#   labs(fill = "Distance to closest opponent",
#        title = "Location of all ball receipts for player with most ball receipts for the tournament for all teams")
# 
# 
# # Plot of ball receipts for all matches for player with highest average ball receipts having played at least 3 matches on each team coloured by distance to closest opponent
# ball_receipts %>%
#   filter(num_matches >= 3) %>%
#   group_by(team.name) %>%
#   slice_max(ave_receipts) %>%
#   ungroup() %>%
#   distinct(id, .keep_all = TRUE) %>%
#   unite(player_team, c("player.name", "team.name"), sep = "=", remove = FALSE) %>%
#   unite(player_team, c("player_team", "main_position"), sep = "\n") %>%
#   select(player_team, location.x, location.y, min_distance) %>%
#   ggplot() +
#   create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
#   geom_point(aes(x=location.x, y=location.y, fill=min_distance), shape=21, size=2) +
#   scale_fill_gradient(low="red", high="green", na.value="grey", limits=c(NA, 20)) +
#   facet_wrap(~ player_team, scales="free") +
#   labs(fill = "Distance to closest opponent",
#        title = "Location of all ball receipts for player with highest average ball receipts having played at least 3 matches for all teams")
# 
# 
# # Plot of ball receipts for player with most ball receipts on a chosen team, how does it compare between matches
# most_overall_receipts_players <- ball_receipts %>%
#   group_by(team.name) %>%
#   slice_max(total_receipts) %>%
#   distinct(player.name)
# 
# team_name <- "England"
# 
# most_ball_receipt_team_name <- ball_receipts %>%
#   group_by(team.name) %>%
#   filter(team.name == team_name,
#          player.name == most_overall_receipts_players %>% filter(team.name == team_name) %>% pull(player.name)) %>%
#   distinct(id, .keep_all = TRUE)
# 
# most_ball_receipt_team_name %>%
#   ggplot() +
#   create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
#   geom_point(data = transform(most_ball_receipt_team_name, OpposingTeam=NULL), aes(x=location.x, y=location.y), shape=21, size=2, fill = "grey85", colour = "grey85") +
#   geom_point(aes(x=location.x, y=location.y, fill=min_distance), shape=21, size=2) +
#   scale_fill_gradient(low="red", high="green", na.value="grey", limits=c(NA, 20)) +
#   facet_wrap(~ OpposingTeam, scales="free") +
#   labs(fill = "Distance to closest opponent",
#        title = paste0("Ball receipts for player with most total ball receipts for ", team_name, " by opposition (", most_ball_receipt_team_name %>% distinct(player.name) %>% pull(player.name), ")"))
# 
# # Horizontal bar chart of highest total ball receipts for players on each team
# ball_receipts %>%
#   filter(player.name %in% most_overall_receipts_players$player.name) %>%
#   select(player.name, team.name, main_position, total_receipts) %>%
#   distinct() %>%
#   unite(player_team, c("player.name", "team.name"), sep = "=", remove = FALSE) %>%
#   unite(player_team, c("player_team", "main_position"), sep = "\n") %>%
#   ggplot() +
#   geom_col(aes(x=total_receipts, y=reorder(player_team, total_receipts))) +
#   labs(x="Total receipts",
#        y="Player, Country, Position",
#        title="Player on each team with highest total ball receipts")
# 
# 
# # Top 10 players with highest average ball receipts
# top10_most_ave_ball_receipts <- ball_receipts %>%
#   filter(num_matches >= 3) %>%
#   select(player.name, ave_receipts) %>%
#   distinct() %>%
#   slice_max(ave_receipts, n=10)
# 
# 
# # Plot highest average ball receipts for players who played at least 3 matches
# ball_receipts %>%
#   filter(player.name %in% top10_most_ave_ball_receipts$player.name) %>%
#   select(player.name, team.name, main_position, ave_receipts) %>%
#   distinct() %>%
#   unite(player_team, c("player.name", "team.name"), sep = "=", remove = FALSE) %>%
#   unite(player_team, c("player_team", "main_position"), sep = "\n") %>%
#   ggplot() +
#   geom_col(aes(x=ave_receipts, y=reorder(player_team, ave_receipts))) +
#   labs(x="Average receipts",
#        y="Player, Country, Position",
#        title="Top 10 players with highest average ball receipts who have played at least 3 matches")
# 
# 
# # Top 10 players with highest total ball receipts
# top10_most_total_ball_receipts <- ball_receipts %>%
#   select(player.name, total_receipts) %>%
#   distinct() %>%
#   slice_max(total_receipts, n=10)
# 
# # Plot highest total ball receipts for players
# ball_receipts %>%
#   filter(player.name %in% top10_most_total_ball_receipts$player.name) %>%
#   select(player.name, team.name, main_position, total_receipts, OpposingTeam, num_receipts) %>%
#   distinct() %>%
#   unite(player_team, c("player.name", "team.name"), sep = "=", remove = FALSE) %>%
#   unite(player_team, c("player_team", "main_position"), sep = "\n") %>%
#   ggplot() +
#   geom_col(aes(x=num_receipts, y=reorder(player_team, total_receipts), fill=OpposingTeam), colour="black") +
#   labs(x="Total receipts",
#        y="Player, Country, Position",
#        title="Top 10 players with most total ball receipts, split by match")
# 
# 
# # What does player with most ball receipts do with the ball?
# # Pass? Dribble? Clearance?
# 
# # Choose a team
# team_name <- "England"
# 
# # Obtain subset of ball_receipts dataframe for player with most overall receipts on chosen team (team_name)
# most_ball_receipt_team_name <- ball_receipts %>%
#   filter(team.name == team_name,
#          player.name == (most_overall_receipts_players %>% filter(team.name == team_name) %>% pull(player.name))) %>%
#   distinct(id, .keep_all = TRUE)
# 
# # Determine what the next event after ball receipt is
# ball_receipts_next_events <- tibble()
# for (m_id in unique(most_ball_receipt_team_name$match_id)) {
#   
#   most_ball_receipt_team_name_sub <- most_ball_receipt_team_name %>%
#     filter(match_id == m_id)
#   
#   next_events_sub <- events %>%
#     filter(match_id == m_id,
#            index %in% (most_ball_receipt_team_name_sub$index+1)) %>%
#     select(type.name)
#   
#   ball_receipts_next_events <- bind_rows(ball_receipts_next_events, bind_cols(most_ball_receipt_team_name_sub, next_events_sub))
#   
# }
# 
# # Plot of ball receipts split by opposition for player with most ball receipts for chosen team (team_name), shape of each ball receipt is based on the next event type, colour of each ball receipt is based on distance to closest opponent
# ball_receipts_next_events %>%
#   ggplot() +
#   create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
#   geom_point(aes(x=location.x, y=location.y, fill=min_distance, shape=type.name, color=min_distance), size=2) +
#   scale_fill_gradient(low="red", high="green", na.value="grey", limits=c(NA, 20)) +
#   scale_color_gradient(low="red", high="green", na.value="grey", limits=c(NA, 20), guide="none") +
#   scale_shape_manual(values = c(21, 22, 23, 24, 25, 8)) +
#   facet_wrap(~ OpposingTeam, scales="free") +
#   labs(fill = "Distance to closest opponent",
#        color = "",
#        shape = "Next event type",
#        title = paste0("Ball receipts for player with most total ball receipts for ", team_name, " by opposition (", most_ball_receipt_team_name %>% distinct(player.name) %>% pull(player.name), ")"))
# 
# 
# # Horizontal bar plot of the number of the next event types, split by next event type on yaxis and fill by opposition, for player with most ball receipts on chosen team
# ball_receipts_next_events %>%
#   group_by(OpposingTeam, type.name) %>%
#   mutate(num_type = n()) %>%
#   ungroup() %>%
#   select(player.name, team.name, main_position, OpposingTeam, type.name, num_type) %>%
#   distinct() %>%
#   group_by(type.name) %>%
#   mutate(total_num_type = sum(num_type)) %>%
#   ungroup() %>%
#   ggplot() +
#   geom_col(aes(x=num_type, y=reorder(type.name, total_num_type), fill=OpposingTeam), colour="black") +
#   labs(x="Number of event type",
#        y="Next event type",
#        fill="Opposition",
#        title=paste0("Next event type after ball receipt, split by event type and opposition, for ", unique(ball_receipts_next_events$player.name), '(', unique(ball_receipts_next_events$team.name), ')'))
# 
# # Horizontal bar plot of the number of the next event types, split by opposition on yaxis and fill by next event type, for player with most ball receipts on chosen team
# ball_receipts_next_events %>%
#   group_by(OpposingTeam, type.name) %>%
#   mutate(num_type = n()) %>%
#   ungroup() %>%
#   select(player.name, team.name, main_position, OpposingTeam, type.name, num_type) %>%
#   distinct() %>%
#   group_by(OpposingTeam) %>%
#   mutate(total_num_type = sum(num_type)) %>%
#   ungroup() %>%
#   ggplot() +
#   geom_col(aes(x=num_type, y=reorder(OpposingTeam, total_num_type), fill=reorder(type.name, num_type)), colour="black") +
#   labs(x="Number of event type",
#        y="Opposition",
#        fill="Next event type",
#        title=paste0("Next event type after ball receipt, split by match and event type, for ", unique(ball_receipts_next_events$player.name), '(', unique(ball_receipts_next_events$team.name), ')'))
# 
# 
# #######################
# 
# 
# # Obtain subset of ball_receipts dataframe for players with most overall receipts on each team
# most_ball_receipts_all <- ball_receipts %>%
#   filter(player.name %in% (most_overall_receipts_players %>% pull(player.name))) %>%
#   distinct(id, .keep_all = TRUE)
# 
# # Determine what the next event after ball receipt is
# ball_receipts_next_events_all <- tibble()
# for (m_id in unique(most_ball_receipts_all$match_id)) {
#   
#   most_ball_receipts_all_sub <- most_ball_receipts_all %>%
#     filter(match_id == m_id)
#   
#   next_events_sub <- events %>%
#     filter(match_id == m_id,
#            index %in% (most_ball_receipts_all_sub$index+1)) %>%
#     select(type.name)
#   
#   ball_receipts_next_events_all <- bind_rows(ball_receipts_next_events_all, bind_cols(most_ball_receipts_all_sub, next_events_sub))
#   
# }
# 
# # Plot of ball receipts split by opposition for player with most ball receipts for chosen team (team_name), shape of each ball receipt is based on the next event type, colour of each ball receipt is based on distance to closest opponent
# ball_receipts_next_events_all %>%
#   ggplot() +
#   create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
#   geom_point(aes(x=location.x, y=location.y, fill=min_distance, shape=type.name, color=min_distance), size=2) +
#   scale_fill_gradient(low="red", high="green", na.value="grey", limits=c(NA, 20)) +
#   scale_color_gradient(low="red", high="green", na.value="grey", limits=c(NA, 20), guide="none") +
#   #scale_shape_manual(values = c(21, 22, 23, 24, 25, 8)) +
#   facet_wrap(~ player.name, scales="free") +
#   labs(fill = "Distance to closest opponent",
#        color = "",
#        shape = "Next event type",
#        title = paste0("Ball receipts for player with most total ball receipts for ", team_name, " by opposition (", most_ball_receipt_team_name %>% distinct(player.name) %>% pull(player.name), ")"))
# 
# 
# # Horizontal bar plot of the number of the next event types, fill by next event type, for player with most ball receipts on each team
# ball_receipts_next_events_all %>%
#   group_by(player.name, type.name) %>%
#   mutate(num_type = n()) %>%
#   ungroup() %>%
#   select(player.name, team.name, main_position, type.name, num_type) %>%
#   distinct() %>%
#   group_by(player.name) %>%
#   mutate(total_num_type = sum(num_type)) %>%
#   ungroup() %>%
#   ggplot() +
#   geom_col(aes(x=num_type, y=reorder(player.name, total_num_type), fill=reorder(type.name, num_type)), colour="black") +
#   labs(x="Number of event type",
#        y="Player",
#        fill="Next event type",
#        title=paste0("Next event type after ball receipt, split by event type, for player in each team with most overall ball receipts"))
# 
# 
# # For all players
# # Determine what the next event after ball receipt is
# ball_receipts_next_events_all <- tibble()
# for (m_id in unique(ball_receipts$match_id)) {
#   
#   ball_receipts_sub <- ball_receipts %>%
#     filter(match_id == m_id) %>%
#     distinct(id, .keep_all = TRUE)
#   
#   next_events_sub <- events %>%
#     filter(match_id == m_id,
#            index %in% (ball_receipts_sub$index+1)) %>%
#     select(type.name, carry.end_location.x, carry.end_location.y)
#   
#   ball_receipts_next_events_all <- bind_rows(ball_receipts_next_events_all, bind_cols(ball_receipts_sub, next_events_sub))
#   
# }
# 
# 
# ball_receipts_next_events_all <- ball_receipts_next_events_all %>%
#   mutate(carry_distance = sqrt( (location.x - carry.end_location.x)^2 + (location.y - carry.end_location.y)^2 ))
# 
# 
# ball_receipts_next_events_all %>%
#   filter(type.name == "Carry",
#          player.name == "John Stones") %>%
#   group_by(OpposingTeam) %>%
#   mutate(num_type = n()) %>%
#   ungroup() %>%
#   select(player.name, team.name, main_position, OpposingTeam, type.name, num_type) %>%
#   distinct() %>%
#   ggplot() +
#   geom_col(aes(x=num_type, y=type.name, fill=OpposingTeam), colour="black") +
#   labs(x="Number of event type",
#        y="Next event type",
#        fill="Opposition",
#        title=paste0("Next event type after ball receipt, split by event type and opposition, for ", unique(ball_receipts_next_events$player.name), '(', unique(ball_receipts_next_events$team.name), ')'))
# 
# 
# 
# 
# ball_receipts_next_events_all %>%
#   filter(type.name == "Carry",
#          player.name == "John Stones",
#          carry_distance >= 1) %>%
#   group_by(OpposingTeam) %>%
#   mutate(num_type = n()) %>%
#   ungroup() %>%
#   select(player.name, team.name, main_position, OpposingTeam, type.name, num_type) %>%
#   distinct() %>%
#   ggplot() +
#   geom_col(aes(x=num_type, y=type.name, fill=OpposingTeam), colour="black") +
#   labs(x="Number of event type",
#        y="Next event type",
#        fill="Opposition",
#        title=paste0("Next event type after ball receipt, split by event type and opposition, for ", unique(ball_receipts_next_events$player.name), '(', unique(ball_receipts_next_events$team.name), ')'))
# 


# Is it possible to calculate time on field?
# Can we calculate ball receipts by amount of time on field?
