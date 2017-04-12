# uncomment this section if you need to install or update packages
# install.packages("devtools")
# library(devtools)
# devtools::install_github("stattleship/stattleship-r")
# install.packages("dplyr")

# POI: THIS WHOLE THING TAKES A WHILE

# get all of the 2014 - 2016 data and store it as CSV on machine / upload to google drive 

library(stattleshipR)

set_token('b53532dbcec8625311565ef38125b4ba')

sport <- 'baseball'  
league <- 'mlb'  

ep <- 'teams'
q_body <- list(season_id = 'mlb-2016')

tms <- ss_get_result(sport = sport, league = league, ep = ep, query = q_body, walk = TRUE)  
baseball_teams <- do.call('rbind', lapply(tms, function(x) x$teams)) 
baseball_teams$hashtags <- as.character(baseball_teams$hashtags)
baseball_teams$colors <- as.character(baseball_teams$hashtags)
tms <- unique(baseball_teams$slug)

write.csv(baseball_teams, 'mlb_teams.csv')
  
# historical game logs
ep = 'game_logs'

# get 2015 game logs
game_logs_2015 <- data.frame()

for (i in 1:length(tms)){
  qbody <- list(status = 'ended', season_id = 'mlb-2015', team_id = tms[i], interval_type = 'regularseason')
  gls <- ss_get_result(sport = sport, league = league, ep = ep, query = qbody, walk = TRUE)
  game_logs_2015 <- rbind(game_logs_2015, do.call('rbind', lapply(gls, function(x) x$game_logs)))
  Sys.sleep(0.8)
}

write.csv(game_logs_2015, 'mlb_game_logs_2015.csv')

# get 2016 game logs
game_logs_2016 <- data.frame()
for (i in 1:length(tms)){
  qbody <- list(status = 'ended', season_id = 'mlb-2016', team_id = tms[i], interval_type = 'regularseason')
  gls <- ss_get_result(sport = sport, league = league, ep = ep, query = qbody, walk = TRUE)
  game_logs_2016 <- rbind(game_logs_2016, do.call('rbind', lapply(gls, function(x) x$game_logs)))
  Sys.sleep(0.8)
}

write.csv(game_logs_2016, 'mlb_game_logs_2016.csv')


ep <- 'players'  
q_body <- list()
pls <- ss_get_result(sport = sport, league = league, ep = ep, query = q_body, walk = TRUE)  
baseball_players <- do.call('rbind', lapply(pls, function(x) x$players)) 

write.csv(baseball_players, 'mlb_players.csv')

ep <- 'games'
games_2015 <- data.frame()
games_2016 <- data.frame()
for (i in 1:length(tms)){
  qbody <- list(status = 'ended', season_id = 'mlb-2015', team_id = tms[i], interval_type = 'regularseason')
  gms15 <- ss_get_result(sport = sport, league = league, ep = ep, query = qbody, walk = TRUE)
  games_2015 <- rbind(games_2015, do.call('rbind', lapply(gms15, function(x) x$games)))
  Sys.sleep(0.8)
  qbody <- list(status = 'ended', season_id = 'mlb-2016', team_id = tms[i], interval_type = 'regularseason')
  gms16 <- ss_get_result(sport = sport, league = league, ep = ep, query = qbody, walk = TRUE)
  games_2016 <- rbind(games_2015, do.call('rbind', lapply(gms16, function(x) x$games)))
  Sys.sleep(0.8)
}

games_2015$official_ids <- as.character(games_2015$official_ids)
games_2016$official_ids <- as.character(games_2016$official_ids)

write.csv(games_2015, 'mlb_games_2015.csv')
write.csv(games_2016, 'mlb_games_2016.csv')

ep <- 'pitches'
pitches_2015 <- data.frame()
pitches_2016 <- data.frame()
for (i in 1:length(tms)){
  qbody <- list(season_id = 'mlb-2015', team_id = tms[i], interval_type = 'regularseason')
  p15 <- ss_get_result(sport = sport, league = league, ep = ep, query = qbody, walk = TRUE)
  pitches_2015 = rbind(pitches_2015, do.call('rbind', lapply(p15, function(x) x$pitches)))
  Sys.sleep(0.8)
}

for (i in 1:length(tms)){
  qbody <- list(season_id = 'mlb-2016', team_id = tms[i], interval_type = 'regularseason')
  p16 <- ss_get_result(sport = sport, league = league, ep = ep ,query = qbody, walk = TRUE)
  pitches_2016 <- rbind(pitches_2016, do.call('rbind', lapply(p16, function(x) x$pitches)))
  Sys.sleep(0.8)
}

write.csv(pitches_2015, 'mlb_pitches_2015.csv')
write.csv(pitches_2016, 'mlb_pitches_2016.csv')

p <- ss_get_result(sport = sport, league = league, ep = ep, query = qbody, walk = FALSE)
p <- do.call('rbind', lapply(p, function(x) x$pitches))
# ep <- 'injuries'
# q_body <- list()

# inj <- ss_get_result(sport = sport, league = league, ep = ep, query = q_body, walk = TRUE)
# baseball_injuries <- do.call('rbind', lapply(inj, function(x) x$injuries))

ep <- 'games'
q_body <- list(status = "ended", season_id = 'mlb-2016', interval_type = "regularseason")

gms <- ss_get_result(sport = sport, league = league, ep = ep, query = q_body, walk = TRUE)
baseball_games <- do.call('rbind', lapply(gms, function(x) x$games))

# store raw data
baseball_game_logs_raw_2016 <- baseball_game_logs
baseball_players_raw_2016 <- baseball_players
baseball_teams_raw_2016 <- baseball_teams
# baseball_injuries_raw <- baseball_injuries
baseball_games_raw_2016 <- baseball_games

# save.image(paste0('Baseball/data_MLB_2016.RData'))
