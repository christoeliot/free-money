# update current 2017 data and store it as CSV 
library(stattleshipR)

set_token('b53532dbcec8625311565ef38125b4ba')

sport <- 'baseball'  
league <- 'mlb'  

ep <- 'teams'

tms <- ss_get_result(sport = sport, league = league, ep = ep, query = q_body, walk = TRUE)  
baseball_teams <- do.call('rbind', lapply(tms, function(x) x$teams)) 
baseball_teams$hashtags <- as.character(baseball_teams$hashtags)
baseball_teams$colors <- as.character(baseball_teams$hashtags)
tms <- unique(baseball_teams$slug)

# get 2015 game logs
game_logs_2017 <- data.frame()
ep <- 'game_logs'

for (i in 1:length(tms)){
  qbody <- list(status = 'ended', team_id = tms[i], interval_type = 'regularseason')
  gls <- ss_get_result(sport = sport, league = league, ep = ep, query = qbody, walk = TRUE)
  game_logs_2017 <- rbind(game_logs_2017, do.call('rbind', lapply(gls, function(x) x$game_logs)))
  Sys.sleep(0.8)
}

write.csv(game_logs_2017, 'Baseball/mlb_game_logs_2017.csv')

games_2017 <- data.frame()
ep <- 'games'
for (i in 1:length(tms)){
  qbody <- list(status = 'ended', team_id = tms[i], interval_type = 'regularseason')
  gms <- ss_get_result(sport = sport, league = league, ep = ep, query = qbody, walk = TRUE)
  games_2017 <- rbind(games_2017, do.call('rbind', lapply(gms, function(x) x$games)))
  Sys.sleep(0.8)
}

write.csv(game_logs_2017, 'Baseball/mlb_games_2017.csv')

##### ADD WHEN YOU LAST DID IT AND THEN SOME SHIT TO MAKE YOU UPDATE IT SINCE YOU LAST DID IT
