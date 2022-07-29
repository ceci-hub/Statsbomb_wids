library(tidyverse)
library(StatsBombR)
library(SBpitch)
library(ggplot2)

source("statsbombfunctionstoreaddata.R")
source("createpitch.R")

# Load all free competitions available from StatsBomb data
comp <- FreeCompetitions()

# Load all matches for the competitions
matches <- FreeMatches(comp)

# Filter matches of interest to UEFA Euro only
matches <- matches %>%
  filter(competition.competition_name=="UEFA Euro")

# Load free 360 data for Euro matches (run function in parallel)
data360 <- StatsBombFree360Events(MatchesDF = matches, Parallel = T)

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


# Successful ball receipts
# Want to quantify how many ball receipts are in space as a way to quantify how well a defence is performing?
ball_receipts <- events %>%
  filter(type.name == "Ball Receipt*",
         is.na(ball_receipt.outcome.name)) %>% 
  # select_if(~any(!is.na(.))) %>% # Remove columns with all NAs
  select(c(id, index, team.name, play_pattern.name, player.name, position.name, ball_receipt.outcome.name, match_id, location.x, location.y, OpposingTeam, freeze_frame, visible_area))

# Plot of ball receipts for Kalvin Phillips in match between England and Italy
ball_receipts %>%
  filter(match_id == 3795506,
         player.name == "Kalvin Phillips") %>%
  ggplot() +
  create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
  geom_point(aes(x=location.x, y=location.y)) +
  labs(title = paste0("Ball receipts by Kalvin Phillips"))


ball_receipts <- ball_receipts %>%
  unnest(freeze_frame) %>%
  mutate(ff_location.x = (map(location, 1)), ff_location.y = (map(location, 2))) %>%
  select(-location) %>%
  mutate(ff_location.x = as.numeric(ifelse(ff_location.x == "NULL", NA, ff_location.x)), ff_location.y = as.numeric(ifelse(ff_location.y == "NULL", NA, ff_location.y))) 


ball_receipts %>%
  filter(id=="0d2c8266-ba81-46ee-acf8-a9eebd86d672") %>%
  mutate(Player_Type_Key = case_when(actor==TRUE & teammate==TRUE ~ "Actor",
                                     teammate==TRUE ~ "Teammate",
                                     teammate==FALSE & keeper==FALSE ~ "Opponent",
                                     keeper==TRUE & teammate==FALSE ~ "Goalkeeper")) %>%
  ggplot() +
  create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
  geom_point(aes(x = ff_location.x, y = ff_location.y, fill=Player_Type_Key),
             size = 3, alpha = 0.5, shape=21) +
  geom_point(aes(x=location.x, y=location.y))

# Check if successful ball receipts correspond to end location of successful passes?
ball_receipts_comparison <- events %>%
  filter(type.name == "Ball Receipt*",
         is.na(ball_receipt.outcome.name)) %>%
  unnest(related_events)

completed_passes_comparison <- events %>%
  filter(type.name == "Pass",
         is.na(pass.outcome.name)) %>%
  unnest(related_events)

if (all(ball_receipts_comparison$id %in% completed_passes_comparison$related_events) & all(completed_passes_comparison$id %in% ball_receipts_comparison$related_events)) {
  print("All ball receipt ids are in completed passes related events and all completed passes ids are in ball receipt related events")
}

# Location of ball receipt by location.x and location.y does not always match the location of the actor ff_location.x and ff_location.y
# Maybe take the average of their location?
ball_receipts <- ball_receipts %>%
  mutate(Player_Type_Key = case_when(actor==TRUE & teammate==TRUE ~ "Actor",
                                     teammate==TRUE ~ "Teammate",
                                     teammate==FALSE & keeper==FALSE ~ "Opponent",
                                     keeper==TRUE & teammate==FALSE ~ "Goalkeeper")) %>%
  mutate(actor.location.x = 
           case_when(Player_Type_Key == "Actor" ~ (location.x + ff_location.x)/2),
         actor.location.y = 
           case_when(Player_Type_Key == "Actor" ~ (location.y + ff_location.y)/2))

# Filter by single event and plot to see if average location calculation works
ball_receipts %>%
  filter(id=="0d2c8266-ba81-46ee-acf8-a9eebd86d672") %>%
  ggplot() +
  create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
  geom_point(aes(x = ff_location.x, y = ff_location.y, fill=Player_Type_Key),
             size = 3, alpha = 0.5, shape=21) +
  geom_point(aes(x=location.x, y=location.y)) +
  geom_point(aes(x=actor.location.x, y=actor.location.y), shape=2)

# Calculate distance (euclidean distance) between average actor/ball receipt and location of opponent(s)
ball_receipts <- ball_receipts %>%
  group_by(id) %>%
  fill(actor.location.x, actor.location.y, .direction = "downup") %>%
  ungroup() %>%
  mutate(distance = if_else(Player_Type_Key == "Actor", NA_real_, sqrt( (actor.location.x - ff_location.x)^2 + (actor.location.y - ff_location.y)^2 )))

ball_receipts %>%
  filter(id=="0d2c8266-ba81-46ee-acf8-a9eebd86d672") %>%
  ggplot() +
  create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
  geom_point(aes(x = ff_location.x, y = ff_location.y, fill=distance),
             size = 3, alpha = 0.75, shape=21) +
  geom_point(aes(x=location.x, y=location.y)) +
  geom_point(aes(x=actor.location.x, y=actor.location.y), shape=2) +
  scale_fill_gradient(low="red", high="green", na.value="grey")


# Drop events where there are no Actors and single Actor only
single_actor_ids <- ball_receipts %>% 
  group_by(id) %>% 
  filter(all(actor == TRUE)) %>% 
  select(id)

no_actor_ids <- ball_receipts %>% 
  group_by(id) %>% 
  filter(all(Player_Type_Key != "Actor")) %>%
  distinct(id)


# Calculate minimum distance between actor/ball receipt and opponent
# Need to filter by "Opponent" in Player_Type_Key
ball_receipts_min_dist <- ball_receipts %>% 
  filter(!(id %in% single_actor_ids$id),
         !(id %in% no_actor_ids$id)) %>%
  filter(Player_Type_Key == "Opponent") %>%
  group_by(id) %>%
  summarise(min_distance = min(distance, na.rm = TRUE))

# Join the minimum distance with the ball_receipts dataframe
ball_receipts <- ball_receipts %>% 
  left_join(ball_receipts_min_dist, by = "id")

# Plot ball receipts coloured by distance to closest opponent for specific match
m_id <- 3788741
ball_receipts %>%
  filter(match_id == m_id) %>%
  ggplot() +
  create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
  geom_point(aes(x=location.x, y=location.y, fill=min_distance), shape=21, size=2) +
  scale_fill_gradient(low="red", high="green", na.value="grey") + 
  facet_wrap(~ team.name, scales="free") +
  labs(fill = "Distance to closest opponent")


# Obtain dataframe with number of ball receipts for each player for each match, the total number of ball receipts for each player for the tournament, the average number of ball receipts for each player
ball_receipt_player <- ball_receipts %>%
  # Distinct "id" as unnest(freeze_frame) repeated ids for all locations of players in freeze_frame, keep all columns
  distinct(id, .keep_all = TRUE) %>%
  # Want to calculate the number of ball receipts for each player for each match
  group_by(match_id, player.name) %>%
  mutate(num_receipts = n()) %>%
  ungroup() %>%
  # Want to calculate total number of ball receipts for each player
  group_by(player.name) %>%
  mutate(total_receipts = n()) %>%
  # Distinct match_id because we are only interested in the number of ball receipts for each match and num_receipts is duplicated for all events in a match. Only need to know this value once
  distinct(match_id, .keep_all = TRUE) %>%
  # Want to calculate the number of matches played for each player and the average number of ball receipts for each player
  mutate(num_matches = n(),
         ave_receipts = mean(num_receipts)) %>%
  select(player.name, match_id, total_receipts, ave_receipts, num_receipts, num_matches) %>% 
  distinct()

# Join number of ball receipts dataframe to ball_receipts dataframe
ball_receipts <- ball_receipts %>%
  left_join(ball_receipt_player, by=c("player.name", "match_id"))

# Rename position.name to Forward, Midfield, Back, Wing
ball_receipts <- ball_receipts %>%
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

# Plot of ball receipts for player with most ball receipts on each team for given match coloured by distance to closest opponent
ball_receipts %>%
  filter(match_id == m_id) %>%
  group_by(team.name) %>%
  slice_max(num_receipts) %>%
  ungroup() %>%
  distinct(id, .keep_all = TRUE) %>%
  unite(player_team, c("player.name", "team.name", "main_position"), sep = "\n", remove = FALSE) %>%
  select(player_team, location.x, location.y, min_distance) %>%
  ggplot() +
  create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
  geom_point(aes(x=location.x, y=location.y, fill=min_distance), shape=21, size=2) +
  scale_fill_gradient(low="red", high="green", na.value="grey") + 
  facet_wrap(~ player_team, scales="free") +
  labs(fill = "Distance to closest opponent",
       title = "Location of ball receipts for player on each team with most ball receipts \nfor the match (Turkey vs Italy)")


# Plot of ball receipts for all matches for player with most overall ball receipts on each team coloured by distance to closest opponent
ball_receipts %>%
  group_by(team.name) %>%
  slice_max(total_receipts) %>%
  ungroup() %>%
  distinct(id, .keep_all = TRUE) %>%
  unite(player_team, c("player.name", "team.name"), sep = "=", remove = FALSE) %>%
  unite(player_team, c("player_team", "main_position"), sep = "\n") %>%
  select(player_team, location.x, location.y, min_distance) %>%
  ggplot() +
  create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
  geom_point(aes(x=location.x, y=location.y, fill=min_distance), shape=21, size=2) +
  scale_fill_gradient(low="red", high="green", na.value="grey", limits=c(NA, 20)) + 
  facet_wrap(~ player_team, scales="free") +
  labs(fill = "Distance to closest opponent",
       title = "Location of all ball receipts for player with most ball receipts for the tournament for all teams")


# Plot of ball receipts for all matches for player with highest average ball receipts having played at least 3 matches on each team coloured by distance to closest opponent
ball_receipts %>%
  filter(num_matches >= 3) %>%
  group_by(team.name) %>%
  slice_max(ave_receipts) %>%
  ungroup() %>%
  distinct(id, .keep_all = TRUE) %>%
  unite(player_team, c("player.name", "team.name"), sep = "=", remove = FALSE) %>%
  unite(player_team, c("player_team", "main_position"), sep = "\n") %>%
  select(player_team, location.x, location.y, min_distance) %>%
  ggplot() +
  create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
  geom_point(aes(x=location.x, y=location.y, fill=min_distance), shape=21, size=2) +
  scale_fill_gradient(low="red", high="green", na.value="grey", limits=c(NA, 20)) + 
  facet_wrap(~ player_team, scales="free") +
  labs(fill = "Distance to closest opponent",
       title = "Location of all ball receipts for player with highest average ball receipts having played at least 3 matches for all teams")


# Plot of ball receipts for player with most ball receipts on a chosen team, how does it compare between matches
most_overall_receipts_players <- ball_receipts %>%
  group_by(team.name) %>%
  slice_max(total_receipts) %>%
  distinct(player.name)

team_name <- "England"

most_ball_receipt_team_name <- ball_receipts %>%
  group_by(team.name) %>%
  filter(team.name == team_name,
         player.name == most_overall_receipts_players %>% filter(team.name == team_name) %>% pull(player.name)) %>%
  distinct(id, .keep_all = TRUE)
  
most_ball_receipt_team_name %>%
  ggplot() +
  create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
  geom_point(data = transform(most_ball_receipt_team_name, OpposingTeam=NULL), aes(x=location.x, y=location.y), shape=21, size=2, fill = "grey85", colour = "grey85") +
  geom_point(aes(x=location.x, y=location.y, fill=min_distance), shape=21, size=2) +
  scale_fill_gradient(low="red", high="green", na.value="grey", limits=c(NA, 20)) + 
  facet_wrap(~ OpposingTeam, scales="free") +
  labs(fill = "Distance to closest opponent",
       title = paste0("Ball receipts for player with most total ball receipts for ", team_name, " by opposition (", most_ball_receipt_team_name %>% distinct(player.name) %>% pull(player.name), ")"))

# Horizontal bar chart of highest total ball receipts for players on each team
ball_receipts %>%
  filter(player.name %in% most_overall_receipts_players$player.name) %>%
  select(player.name, team.name, main_position, total_receipts) %>%
  distinct() %>%
  unite(player_team, c("player.name", "team.name"), sep = "=", remove = FALSE) %>%
  unite(player_team, c("player_team", "main_position"), sep = "\n") %>%
  ggplot() +
  geom_col(aes(x=total_receipts, y=reorder(player_team, total_receipts))) +
  labs(x="Total receipts",
       y="Player, Country, Position",
       title="Player on each team with highest total ball receipts")


# Top 10 players with highest average ball receipts
top10_most_ave_ball_receipts <- ball_receipts %>%
  filter(num_matches >= 3) %>%
  select(player.name, ave_receipts) %>%
  distinct() %>%
  slice_max(ave_receipts, n=10)


# Plot highest average ball receipts for players who played at least 3 matches
ball_receipts %>%
  filter(player.name %in% top10_most_ave_ball_receipts$player.name) %>%
  select(player.name, team.name, main_position, ave_receipts) %>%
  distinct() %>%
  unite(player_team, c("player.name", "team.name"), sep = "=", remove = FALSE) %>%
  unite(player_team, c("player_team", "main_position"), sep = "\n") %>%
  ggplot() +
  geom_col(aes(x=ave_receipts, y=reorder(player_team, ave_receipts))) +
  labs(x="Average receipts",
       y="Player, Country, Position",
       title="Top 10 players with highest average ball receipts who have played at least 3 matches")


# Top 10 players with highest total ball receipts
top10_most_total_ball_receipts <- ball_receipts %>%
  select(player.name, total_receipts) %>%
  distinct() %>%
  slice_max(total_receipts, n=10)

# Plot highest total ball receipts for players
ball_receipts %>%
  filter(player.name %in% top10_most_total_ball_receipts$player.name) %>%
  select(player.name, team.name, main_position, total_receipts, OpposingTeam, num_receipts) %>%
  distinct() %>%
  unite(player_team, c("player.name", "team.name"), sep = "=", remove = FALSE) %>%
  unite(player_team, c("player_team", "main_position"), sep = "\n") %>%
  ggplot() +
  geom_col(aes(x=num_receipts, y=reorder(player_team, total_receipts), fill=OpposingTeam), colour="black") +
  labs(x="Total receipts",
       y="Player, Country, Position",
       title="Top 10 players with most total ball receipts, split by match")


# What does player with most ball receipts do with the ball?
# Pass? Dribble? Clearance?

# Choose a team
team_name <- "England"

# Obtain subset of ball_receipts dataframe for player with most overall receipts on chosen team (team_name)
most_ball_receipt_team_name <- ball_receipts %>%
  filter(team.name == team_name,
         player.name == (most_overall_receipts_players %>% filter(team.name == team_name) %>% pull(player.name))) %>%
  distinct(id, .keep_all = TRUE) 

# Determine what the next event after ball receipt is
ball_receipts_next_events <- tibble()
for (m_id in unique(most_ball_receipt_team_name$match_id)) {
  
  most_ball_receipt_team_name_sub <- most_ball_receipt_team_name %>%
    filter(match_id == m_id)
  
  next_events_sub <- events %>% 
    filter(match_id == m_id,
           index %in% (most_ball_receipt_team_name_sub$index+1)) %>%
    select(type.name)
  
  ball_receipts_next_events <- bind_rows(ball_receipts_next_events, bind_cols(most_ball_receipt_team_name_sub, next_events_sub))
  
}

# Plot of ball receipts split by opposition for player with most ball receipts for chosen team (team_name), shape of each ball receipt is based on the next event type, colour of each ball receipt is based on distance to closest opponent
ball_receipts_next_events %>%
  ggplot() +
  create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
  geom_point(aes(x=location.x, y=location.y, fill=min_distance, shape=type.name, color=min_distance), size=2) +
  scale_fill_gradient(low="red", high="green", na.value="grey", limits=c(NA, 20)) +
  scale_color_gradient(low="red", high="green", na.value="grey", limits=c(NA, 20), guide="none") +
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 8)) + 
  facet_wrap(~ OpposingTeam, scales="free") +
  labs(fill = "Distance to closest opponent",
       color = "",
       shape = "Next event type",
       title = paste0("Ball receipts for player with most total ball receipts for ", team_name, " by opposition (", most_ball_receipt_team_name %>% distinct(player.name) %>% pull(player.name), ")"))


# Horizontal bar plot of the number of the next event types, split by next event type on yaxis and fill by opposition, for player with most ball receipts on chosen team 
ball_receipts_next_events %>%
  group_by(OpposingTeam, type.name) %>%
  mutate(num_type = n()) %>%
  ungroup() %>%
  select(player.name, team.name, main_position, OpposingTeam, type.name, num_type) %>%
  distinct() %>%
  group_by(type.name) %>%
  mutate(total_num_type = sum(num_type)) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(x=num_type, y=reorder(type.name, total_num_type), fill=OpposingTeam), colour="black") +
  labs(x="Number of event type",
       y="Next event type",
       fill="Opposition",
       title=paste0("Next event type after ball receipt, split by event type and opposition, for ", unique(ball_receipts_next_events$player.name), '(', unique(ball_receipts_next_events$team.name), ')'))

# Horizontal bar plot of the number of the next event types, split by opposition on yaxis and fill by next event type, for player with most ball receipts on chosen team 
ball_receipts_next_events %>%
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
  labs(x="Number of event type",
       y="Opposition",
       fill="Next event type",
       title=paste0("Next event type after ball receipt, split by match and event type, for ", unique(ball_receipts_next_events$player.name), '(', unique(ball_receipts_next_events$team.name), ')'))


#######################


# Obtain subset of ball_receipts dataframe for players with most overall receipts on each team
most_ball_receipts_all <- ball_receipts %>%
  filter(player.name %in% (most_overall_receipts_players %>% pull(player.name))) %>%
  distinct(id, .keep_all = TRUE)

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



# Is it possible to calculate time on field?
# Can we calculate ball receipts by amount of time on field?


#### Sidetracked/tangent as there are events with only a single player as Actor and events with no Actor. Decided to filter these events out for now. ####

# Why are there events with only a single player as the Actor? No other players in the freeze frame. 
View(ball_receipts %>% group_by(id) %>% filter(all(actor == TRUE)))

# For id = "0e7941e1-9290-49d2-9d6c-4c3b21d575ad", seems like genuinely no one around?
# Everyone could be up the pitch due to corner, no one in visible area
id_interest <- "0e7941e1-9290-49d2-9d6c-4c3b21d575ad"

# Visible area from 360 data
visible_area <- ball_receipts %>%
  filter(id==id_interest) %>% select(visible_area) %>% pull(visible_area) %>%.[[1]]

visible_area <- bind_cols(visible_area_x=visible_area[seq(1,length(visible_area),2)], 
                          visible_area_y=visible_area[seq(2,length(visible_area),2)])

ball_receipts %>%
  filter(id==id_interest) %>%
  ggplot() +
  create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
  geom_point(aes(x = ff_location.x, y = ff_location.y, fill=Player_Type_Key),
             size = 3, alpha = 0.5, shape=21) +
  geom_point(aes(x=location.x, y=location.y)) +
  geom_point(aes(x=actor.location.x, y=actor.location.y), shape=2) +
  geom_path(data=visible_area, aes(x=visible_area_x, y=visible_area_y))


# Have a look at the related events. Everything related to the same possession number.
# Note that the locations here don't all make sense, most likely because location is from the perspective of the team, so some events here are clearances attributable to the opposing team (England), hence would need to calculate x location to be 120-x
possession_id <- 198
m_id <- 3795221
team_name <- "England"
events %>% 
  filter(possession==possession_id, 
         match_id == m_id) %>% 
  select(id, index, location.x, location.y, type.name, team.name, player.name, pass.length, pass.angle, pass.end_location.x, pass.end_location.y, pass.type.name, pass.outcome.name, pass.technique.name, carry.end_location.x, carry.end_location.y, duel.type.name, ball_receipt.outcome.name, OpposingTeam, visible_area, freeze_frame) %>%
  mutate(location.x = case_when(team.name == team_name ~ 120-location.x,
                                TRUE ~ location.x)) %>%
  ggplot() +
  create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
  geom_segment(aes(x=location.x, y=location.y, xend=lead(location.x), yend=lead(location.y), colour=index), arrow = arrow(length = unit(0.06,"inches"))) +
  scale_colour_gradient(low="red", high="green", na.value="grey")

# Freeze frames
events %>% 
  select(-location) %>%
  filter(possession==possession_id, 
         match_id == m_id) %>%
  unnest(freeze_frame) %>%
  mutate(ff_location.x = (map(location, 1)), ff_location.y = (map(location, 2))) %>%
  select(-location) %>%
  mutate(ff_location.x = as.numeric(ifelse(ff_location.x == "NULL", NA, ff_location.x)), ff_location.y = as.numeric(ifelse(ff_location.y == "NULL", NA, ff_location.y))) %>%
  mutate(Player_Type_Key = case_when(actor==TRUE & teammate==TRUE ~ "Actor",
                                     teammate==TRUE ~ "Teammate",
                                     teammate==FALSE & keeper==FALSE ~ "Opponent",
                                     keeper==TRUE & teammate==FALSE ~ "Goalkeeper")) %>%
  mutate(location.x = case_when(team.name == team_name ~ 120-location.x,
                                TRUE ~ location.x),
         ff_location.x = case_when(team.name == team_name ~ 120-ff_location.x,
                                   TRUE ~ ff_location.x)) %>%
  ggplot() +
  create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
  geom_point(aes(x = ff_location.x, y = ff_location.y, fill=Player_Type_Key),
             size = 3, alpha = 0.5, shape=21) +
  geom_point(aes(x=location.x, y=location.y)) +
  facet_wrap(~ index, scales = "free")


# For id = "2dbbd45c-0d04-4ae3-a79a-b3653b23db27", unsure why no opponents or teammates in the area. Also distance between location of ball and location of actor is pretty far...
id_interest <- "2dbbd45c-0d04-4ae3-a79a-b3653b23db27"
# Visible area from 360 data
visible_area <- ball_receipts %>%
  filter(id==id_interest) %>% select(visible_area) %>% pull(visible_area) %>%.[[1]]

visible_area <- bind_cols(visible_area_x=visible_area[seq(1,length(visible_area),2)], 
                          visible_area_y=visible_area[seq(2,length(visible_area),2)])

ball_receipts %>%
  filter(id==id_interest) %>%
  ggplot() +
  create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
  geom_point(aes(x = ff_location.x, y = ff_location.y, fill=Player_Type_Key),
             size = 3, alpha = 0.5, shape=21) +
  geom_point(aes(x=location.x, y=location.y)) +
  geom_point(aes(x=actor.location.x, y=actor.location.y), shape=2) +
  geom_path(data=visible_area, aes(x=visible_area_x, y=visible_area_y))


# Have a look at the related events. Everything related to the same possession number.
# Note that the locations here don't all make sense, most likely because location is from the perspective of the team, so some events here are clearances attributable to the opposing team (Czech Republic), hence would need to calculate x location to be 120-x
possession_id <- 51
m_id <- 3788760
team_name <- "Czech Republic"
events %>%
  filter(match_id == m_id,
         possession == possession_id) %>% 
  select(id, index, location.x, location.y, type.name, team.name, player.name, pass.length, pass.angle, pass.end_location.x, pass.end_location.y, pass.type.name, pass.outcome.name, pass.technique.name, carry.end_location.x, carry.end_location.y, duel.type.name, ball_receipt.outcome.name, OpposingTeam, visible_area, freeze_frame) %>%
  mutate(location.x = case_when(team.name == team_name ~ 120-location.x,
                                TRUE ~ location.x)) %>%
  ggplot() +
  create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
  geom_segment(aes(x=location.x, y=location.y, xend=lead(location.x), yend=lead(location.y), colour=index), arrow = arrow(length = unit(0.06,"inches"))) +
  scale_colour_gradient(low="red", high="green", na.value="grey")


visible_area <- events %>%
  filter(match_id == m_id,
         possession == possession_id) %>% 
  select(visible_area) %>% 
  pull(visible_area)

visible_area_expanded <- tibble()
for (i in 1:length(visible_area)) {
  area_i <- visible_area %>% .[[i]]
  
  visible_area_expanded <- bind_rows(visible_area_expanded, bind_cols(id = i,
                                                                      visible_area_x=area_i[seq(1,length(area_i),2)],
                                                                      visible_area_y=area_i[seq(2,length(area_i),2)]))
}

visible_area_expanded <- visible_area_expanded %>% 
  left_join(events %>% 
              filter(match_id == m_id, possession == possession_id) %>% 
              select(index) %>% 
              mutate(id=1:21)) %>%
  select(-id)


# Freeze frames with visible areas
events %>% 
  select(-location) %>%
  filter(match_id == m_id,
         possession == possession_id) %>%
  unnest(freeze_frame) %>%
  mutate(ff_location.x = (map(location, 1)), ff_location.y = (map(location, 2))) %>%
  select(-location) %>%
  mutate(ff_location.x = as.numeric(ifelse(ff_location.x == "NULL", NA, ff_location.x)), ff_location.y = as.numeric(ifelse(ff_location.y == "NULL", NA, ff_location.y))) %>%
  mutate(Player_Type_Key = case_when(actor==TRUE & teammate==TRUE ~ "Actor",
                                     teammate==TRUE ~ "Teammate",
                                     teammate==FALSE & keeper==FALSE ~ "Opponent",
                                     keeper==TRUE & teammate==FALSE ~ "Goalkeeper")) %>%
  mutate(location.x = case_when(team.name == team_name ~ 120-location.x,
                                TRUE ~ location.x),
         ff_location.x = case_when(team.name == team_name ~ 120-ff_location.x,
                                   TRUE ~ ff_location.x)) %>%
  ggplot() +
  create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
  geom_point(aes(x = ff_location.x, y = ff_location.y, fill=Player_Type_Key),
             size = 3, alpha = 0.5, shape=21) +
  geom_point(aes(x=location.x, y=location.y)) +
  geom_path(data=visible_area_expanded, aes(x=visible_area_x, y=visible_area_y)) +
  facet_wrap(~ index, scales = "free")



# Why are there events with no Actor? Think about what to do with these
View(ball_receipts %>% group_by(id) %>% filter(all(actor == FALSE)))

# 7 such events:
ball_receipts %>% group_by(id) %>% filter(all(actor == FALSE)) %>% distinct(id)

# Have a look at the related events. Everything related to the same possession number.
# Note that the locations here don't all make sense, most likely because location is from the perspective of the team, so some events here are clearances attributable to the opposing team (Czech Republic), hence would need to calculate x location to be 120-x
m_id <- 3788748
possession_id <- 42
team_name <- "Czech Republic"
events %>%
  filter(match_id == m_id,
         possession == possession_id) %>% 
  select(id, index, location.x, location.y, type.name, team.name, player.name, pass.length, pass.angle, pass.end_location.x, pass.end_location.y, pass.type.name, pass.outcome.name, pass.technique.name, carry.end_location.x, carry.end_location.y, duel.type.name, ball_receipt.outcome.name, OpposingTeam, visible_area, freeze_frame) %>%
  mutate(location.x = case_when(team.name == team_name ~ 120-location.x,
                                TRUE ~ location.x)) %>%
  ggplot() +
  create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
  geom_segment(aes(x=location.x, y=location.y, xend=lead(location.x), yend=lead(location.y), colour=index), arrow = arrow(length = unit(0.06,"inches"))) +
  scale_colour_gradient(low="red", high="green", na.value="grey")


visible_area <- events %>%
  filter(match_id == m_id,
         possession == possession_id) %>% 
  select(visible_area) %>% 
  pull(visible_area)

visible_area_expanded <- tibble()
for (i in 1:length(visible_area)) {
  area_i <- visible_area %>% .[[i]]
  
  if (is_null(area_i)) {
    visible_area_expanded <- bind_rows(visible_area_expanded, bind_cols(id = i,
                                                                        visible_area_x=NA,
                                                                        visible_area_y=NA))
  } else {
    visible_area_expanded <- bind_rows(visible_area_expanded, 
                                       bind_cols(id = i,
                                                 visible_area_x=area_i[seq(1,length(area_i),2)], 
                                                 visible_area_y=area_i[seq(2,length(area_i),2)]))
  }
  
}

visible_area_expanded <- visible_area_expanded %>% 
  left_join(events %>% 
              filter(match_id == m_id, possession == possession_id) %>% 
              select(index) %>% 
              mutate(id=1:46)) %>%
  select(-id)


# Freeze frames with visible areas
events %>% 
  select(-location) %>%
  filter(match_id == m_id,
         possession == possession_id) %>%
  unnest(freeze_frame) %>%
  mutate(ff_location.x = (map(location, 1)), ff_location.y = (map(location, 2))) %>%
  select(-location) %>%
  mutate(ff_location.x = as.numeric(ifelse(ff_location.x == "NULL", NA, ff_location.x)), ff_location.y = as.numeric(ifelse(ff_location.y == "NULL", NA, ff_location.y))) %>%
  mutate(Player_Type_Key = case_when(actor==TRUE & teammate==TRUE ~ "Actor",
                                     teammate==TRUE ~ "Teammate",
                                     teammate==FALSE & keeper==FALSE ~ "Opponent",
                                     keeper==TRUE & teammate==FALSE ~ "Goalkeeper")) %>%
  mutate(location.x = case_when(team.name == "Czech Republic" ~ 120-location.x,
                                TRUE ~ location.x),
         ff_location.x = case_when(team.name == "Czech Republic" ~ 120-ff_location.x,
                                   TRUE ~ ff_location.x)) %>%
  ggplot() +
  create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures = TRUE) +
  geom_point(aes(x = ff_location.x, y = ff_location.y, fill=Player_Type_Key),
             size = 3, alpha = 0.5, shape=21) +
  geom_point(aes(x=location.x, y=location.y)) +
  geom_path(data=visible_area_expanded, aes(x=visible_area_x, y=visible_area_y), na.rm = TRUE) +
  facet_wrap(~ index, scales = "free")



