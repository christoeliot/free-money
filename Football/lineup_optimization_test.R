# the model

library(stattleshipR)
library(genalg)
library(plyr)
library(dplyr)

teams <- read.csv('Football/nfl_teams.csv')
tms <- unique(teams$slug)

set_token('b53532dbcec8625311565ef38125b4ba')
sport <- 'football'
league <- 'nfl'

# add way to simply update games list rather than requery everything 
# since max(timestamp)...?

games_2017 <- read.csv('Football/nfl_games_2017.csv')
ep <- 'games'
qbody <- list(status = 'ended', season_id = 'nfl-2017-2018', since = max(games_2017$timestamp))
gms <- ss_get_result(sport = sport, league = league, ep = ep, query = qbody, walk = TRUE)
temp <- do.call('rbind', lapply(gms, function(x) x$games))
drop <- c('X', 'official_ids')
temp <- temp[, !(names(temp) %in% drop)]
games_2017 <- games_2017[, !(names(games_2017) %in% drop)]
colnames(temp)[1] <- 'game_id'
colnames(games_2017)[1] <- 'game_id'
games_2017 <- rbind(games_2017, temp)
write.csv(games_2017, 'Football/nfl_games_2017.csv')

game_logs_2017 <- data.frame()
ep <- 'game_logs'
for (i in 1:length(tms)){
  qbody <- list(status = 'ended', season_id = 'nfl-2017-2018', team_id = tms[i])
  gls <- ss_get_result(sport = sport, league = league, ep = ep, query = qbody, walk = TRUE)
  game_logs_2017 <- rbind(game_logs_2017, do.call('rbind', lapply(gls, function(x) x$game_logs)))
  Sys.sleep(0.5)
}
colnames(game_logs_2017)[1] <- 'game_log_id'
write.csv(game_logs_2017, 'Football/nfl_game_logs_2017.csv')

team_game_logs_2017 <- data.frame()
ep <- 'team_game_logs'
for (i in 1:length(tms)){
  qbody <- list(status = 'ended', season_id = 'nfl-2017-2018', team_id = tms[i])
  tgls <- ss_get_result(sport = sport, league = league, ep = ep, query = qbody, walk = TRUE)
  team_game_logs_2017 <- rbind(team_game_logs_2017, do.call('rbind', lapply(tgls, function(x) x$team_game_logs)))
  Sys.sleep(0.5)
}
colnames(team_game_logs_2017)[1] <- 'team_game_log_id'
write.csv(team_game_logs_2017, 'Football/nfl_team_game_logs_2017.csv')
