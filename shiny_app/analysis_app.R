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
# Depending on version of StatsBombR package.
# The second line runs with the latest install from github as per https://github.com/statsbomb/StatsBombR/issues/18
# data360 <- StatsBombFree360Events(MatchesDF = matches, Parallel = T)
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

ball_receipts_all_player <- ball_receipts_all_player %>%
  left_join(ball_receipts_all_player %>%
              select(player.name, ball_receipt.outcome.name, total_receipts) %>%
              distinct(across()) %>%
              pivot_wider(
                names_from = "ball_receipt.outcome.name",
                values_from  = "total_receipts",
              ) %>%
              mutate(
                Complete = replace_na(Complete, 0),
                Incomplete = replace_na(Incomplete, 0),
                Total = Complete + Incomplete,
                # Replace Complete and Incomplete with success/unsuccess rate
                Complete = Complete/Total,
                Incomplete = Incomplete/Total
              ) %>%
              select(-Total) %>%
              pivot_longer(
                cols = c("Complete", "Incomplete"),
                names_to = "ball_receipt.outcome.name",
                values_to = "success_rate"
              ),
            by = c("player.name", "ball_receipt.outcome.name")
  )

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

# Add distance to closest opponent grouping bins
ball_receipts_all <- ball_receipts_all %>%
  mutate(min_distance_bins = 
           case_when(min_distance <= 5 ~ "0-5yards",
                     min_distance > 5 & min_distance <= 10 ~ "5-10yards",
                     min_distance > 10 & min_distance <= 20 ~ "10-20yards",
                     min_distance > 20 ~ "20+yards")) %>%
  mutate(min_distance_bins = factor(min_distance_bins, levels = c("0-5yards", "5-10yards", "10-20yards", "20+yards")))


############################


# What does a player do with the ball after a successful ball receipt?

# Successful ball receipts
ball_receipts_succ <- ball_receipts_all %>%
  filter(ball_receipt.outcome.name == "Complete")

# Determine the next event after successful ball receipt
ball_receipts_succ_next_events_all <- tibble()
for (m_id in unique(ball_receipts_succ$match_id)) {
  
  # Filter successful ball receipts dataframe by match_id
  ball_receipts_succ_sub <- ball_receipts_succ %>%
    filter(match_id == m_id) %>%
    arrange(index)
  
  # Obtain next event after successful ball receipt and also carry end_location details
  next_events_sub <- events %>%
    filter(match_id == m_id,
           index %in% (ball_receipts_succ_sub$index+1)) %>%
    arrange(index) %>%
    select(type.name, carry.end_location.x, carry.end_location.y)
  
  # Add next event column details to ball_receipts_succ_sub dataframe
  # Then combine with ball_receipts_succ_next_events_all
  ball_receipts_succ_next_events_all <- bind_rows(ball_receipts_succ_next_events_all, bind_cols(ball_receipts_succ_sub, next_events_sub))

}

# Calculate distance between location and carry end_location
ball_receipts_succ_next_events_all <- ball_receipts_succ_next_events_all %>%
  mutate(carry_distance = sqrt( (location.x - carry.end_location.x)^2 + (location.y - carry.end_location.y)^2 ))


# If carry_distance < 1 yard, we say that person was standing still during the carry event
# Find the next event
for (m_id in unique(ball_receipts_succ_next_events_all$match_id)) {
  
  # Filter ball_receipts_succ_next_events_all dataframe by match_id
  # Filter by carry_distance < 1
  ball_receipts_succ_sub_dist <- ball_receipts_succ_next_events_all %>%
    filter(match_id == m_id,
           carry_distance < 1) %>%
    arrange(index)
  
  # Obtain second event after successful ball receipt as the first event had carry_distance less than 1 yard
  next_events_sub <- events %>%
    filter(match_id == m_id,
           index %in% (ball_receipts_succ_sub_dist$index+2)) %>%
    arrange(index) %>%
    select(type.name)
  
  # Replace the next event type
  ball_receipts_succ_next_events_all[ball_receipts_succ_next_events_all$match_id == m_id & ball_receipts_succ_next_events_all$type.name == "Carry" & ball_receipts_succ_next_events_all$carry_distance < 1,"type.name"] = next_events_sub
  
}

ball_receipts_succ_next_events_all %>%
  filter(player.name == "John Stones") %>%
group_by(type.name, min_distance_bins) %>%
  mutate(num_type = n()) %>%
  ungroup() %>%
  select(player.name, team.name, main_position, type.name, min_distance_bins, num_type) %>%
  distinct() %>%
  ggplot() +
  geom_col(aes(x=num_type, y=min_distance_bins, fill=reorder(type.name, num_type)), colour="black") +
  scale_fill_discrete(breaks = unique(ball_receipts_succ_next_events_all$type.name)) +
  labs(x="Number of event type",
       y="Distance to closest opponent \n(successful ball receipt)",
       fill="Next event type") +
  theme(legend.position = "bottom")


