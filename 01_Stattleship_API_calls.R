# uncomment this section if you need to install or update packages
# install.packages("devtools")
# library(devtools)
# devtools::install_github("stattleship/stattleship-r")
# install.packages("dplyr")

# POI: THIS WHOLE THING TAKES A WHILE

library(stattleshipR)

set_token("YOUR API TOKEN")

sport <- 'basketball'  
league <- 'nba'  
ep <- 'game_logs'  
q_body <- list(status='ended', interval_type='regularseason')

gls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE)  
game_logs <- do.call('rbind', lapply(gls, function(x) x$game_logs)) 

sport <- 'basketball'  
league <- 'nba'  
ep <- 'players'  
q_body <- list()

pls <- ss_get_result(sport = sport, league = league, ep = ep, query = q_body, walk = TRUE)  
players <- do.call('rbind', lapply(pls, function(x) x$players)) 

sport <- 'basketball'  
league <- 'nba'  
ep <- 'teams'
q_body <- list()

tms <- ss_get_result(sport = sport, league = league, ep = ep, query = q_body, walk = TRUE)  
teams <- do.call('rbind', lapply(tms, function(x) x$teams)) 

sport <- 'basketball'  
league <- 'nba'  
ep <- 'injuries'
q_body <- list()

inj <- ss_get_result(sport = sport, league = league, ep = ep, query = q_body, walk = TRUE)
injuries <- do.call('rbind', lapply(inj, function(x) x$injuries))

sport <- 'basketball'  
league <- 'nba'  
ep <- 'games'
q_body <- list(status = "ended", interval_type = "regularseason")

gms <- ss_get_result(sport = sport, league = league, ep = ep, query = q_body, walk = TRUE)
games <- do.call('rbind', lapply(gms, function(x) x$games))

# store raw data
game_logs_raw <- game_logs
players_raw <- players
teams_raw <- teams
injuries_raw <- injuries
games_raw <- games

save.image("ADD LOCATION AND FILE NAME HERE")
