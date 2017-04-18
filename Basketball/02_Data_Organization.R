library(dplyr)

# load working data

load("LOCATION AND FILE NAME HERE")

game_logs <- game_logs_raw
players <- players_raw
teams <- teams_raw
injuries <- injuries_raw
games <- games_raw


# correct column names - maybe change to grepl eventually
colnames(teams)[1] <- 'team_id'
colnames(players)[1] <- 'player_id'
colnames(injuries)[1] <- 'injury_id'
colnames(games)[1] <- 'game_id'
colnames(game_logs)[1] <- 'log_id'
colnames(injuries)[1] <- 'injury_id'

colnames(teams)[4] <- 'team_color'
colnames(teams)[5] <- 'team_color_vec'
colnames(teams)[9] <- 'team_name'
colnames(teams)[10] <- 'team_nickname'
colnames(teams)[13] <- 'team_slug'
colnames(players)[21] <- 'player_name'
colnames(players)[22] <- 'player_nickname'
colnames(players)[29] <- 'player_slug'
colnames(players)[26] <- 'real_world_salary'
colnames(injuries)[4] <- 'injury_location'
colnames(injuries)[5] <- 'injury_start_date'
colnames(injuries)[6] <- 'injury_update'
colnames(injuries)[7] <- 'injury_note'
colnames(injuries)[8] <- 'injury_status'
colnames(injuries)[9] <- 'injury_status_label'
colnames(games)[20] <- 'game_label'
colnames(games)[21] <- 'game_name'
colnames(games)[28] <- 'game_slug'
colnames(games)[30] <- 'game_status'
colnames(games)[37] <- 'game_title'


# suppress columns 2 & 3 in all
shed <- c('created_at', 'updated_at')
teams <- teams[, (!names(teams) %in% shed)]
players <- players[, (!names(players) %in% shed)]
games <- games[, (!names(games) %in% shed)]
game_logs <- game_logs[, (!names(game_logs) %in% shed)]
injuries <- injuries[, (!names(injuries) %in% shed)]

# suppress specific columns for each dataframe
shed <- c('hashtags')
teams <- teams[, (!names(teams) %in% shed)]
shed <- c('bats','captain','handedness','high_school','humanized_salary','pro_debut','mlbam_id')
players <- players[, (!names(players) %in% shed)]
shed <- c('game_name', 'interval', 'at_neutral_site','broadcast','clock','clock_secs','daytime','duration','humidity','interval_number',
          'period','period_label','status','internet_coverage','satellite_coverage','television_coverage',
          'temperature','temperature_unit','weather_conditions','wind_direction','wind_speed','wind_speed_unit')
games <- games[, (!names(games) %in% shed)]



# correct game played status (total_time_played <= 0, the player didn't play)
game_logs$game_played <- ifelse(game_logs$time_played_total > 0, TRUE, FALSE)
game_logs <- game_logs[game_logs$game_played == TRUE,]

# add column for fantasty score
game_logs$dk_score <- with(game_logs, (points + 0.5 * three_pointers_made + 1.25 * rebounds_total + 1.5 * assists + 2 * steals
                                       + 2 * blocks - 0.5 * turnovers + 1.5 * double_double + 3 * triple_double))

# merge, check for mismatch, supress redundant columns, rename for workability

players <- merge(players, teams, by = 'team_id')
# check for league_id mistmatch
if(!(all(players$league_id.x == players$league_id.y))){
  stop('Mismatch in League IDs')
  mismatch <- players %>%
    filter(!(players$league_id.x == players$league_id.y)) %>%
    group_by(player_id)
}
players <- players[, !names(players) %in% c('league_id.y')]

game_logs <- merge(game_logs, games, by = 'game_id')  
# check for home_team_outcome.x, home_team_score.x, away_team_outcome.x, away_team_score.y
if(!(all(game_logs$home_team_outcome.x == game_logs$home_team_outcome.y &&
         game_logs$home_team_score.x == game_logs$home_team_score.y &&
         game_logs$away_team_outcome.x == game_logs$away_team_outcome.y &&
         game_logs$away_team_score.x == game_logs$away_team_score.y))){
  mismatch <- game_logs %>%
    filter(!(game_logs$home_team_outcome.x == game_logs$home_team_outcome.y ||
                                 game_logs$home_team_score.x == game_logs$home_team_outcome.y ||
                                 game_logs$away_team_outcome.x == game_logs$away_team_outcome.y ||
                                 game_logs$away_team_score.x == game_logs$away_team_score.y)) %>%
    group_by(player_id)
  stop('Mistmach in scores or outcomes')
}
shed <- c('home_team_outcome.y', 'home_team_score.y', 'away_team_outcome.y', 'away_team_score.y')
game_logs <- game_logs[, !names(game_logs) %in% shed]

# remove all star game (game_id = 4a6935bd-fd89-4c28-9316-6e8fb72a8bae)
asg <- '4a6935bd-fd89-4c28-9316-6e8fb72a8bae'
game_logs <- game_logs[!(game_logs$game_id == asg),]

game_logs <- merge(players, game_logs, by = 'player_id')

# check for mistmatch in team_id
# everyone on this list should be a traded player
if(!all(game_logs$team_id.x == game_logs$team_id.y)){
  mismatch <- game_logs %>%
    filter(!(game_logs$team_id.x == game_logs$team_id.y)) %>%
    group_by(player_id) %>%
    summarise(player = first(player_name), team_x = first(team_id.x), team_y = first(team_id.y))
  View(mismatch)
  stop('Mismatch in Team IDs')
}
game_logs <- game_logs[, !(names(game_logs) %in% c('team_id.y', 'on', 'started_at'))]
game_logs$ended_at <- as.Date(game_logs$ended_at)

colnames(game_logs)[grepl('team_id', names(game_logs))] <- 'team_id'
colnames(game_logs)[grepl('league_id', names(game_logs))] <- 'league_id'
colnames(game_logs)[grepl('home_team_outcome', names(game_logs))] <- 'home_team_outcome'
colnames(game_logs)[grepl('home_team_score', names(game_logs))] <- 'home_team_score'
colnames(game_logs)[grepl('away_team_outcome', names(game_logs))] <- 'away_team_outcome'
colnames(game_logs)[grepl('away_team_score', names(game_logs))] <- 'away_team_score'
