source("00_Helpers.R")

#import DK Salaries and assign to contest

contest <-  read.csv('DKSalaries.csv')
colnames(contest)[2] <- 'player_name'
contest$AwayTeam <- lapply(strsplit(as.character(contest$GameInfo), '\\@'), '[', 1)
contest$GameTime <- lapply(strsplit(as.character(contest$GameInfo), '\\@'), '[', 2)
contest$HomeTeam <- lapply(strsplit(as.character(contest$GameTime), '\\ '), '[', 1)
contest$GameTime <- lapply(strsplit(as.character(contest$GameTime), '\\ '), '[', 2)

# merging with the player data frame
n_playing = unique(contest$player_name)
temp <- players[players$player_name %in% n_playing,]
contest <- merge(contest, temp, by = 'player_name')
keep <- c('player_name','Position','Salary','GameInfo','AvgPointsPerGame','teamAbbrev',
          'AwayTeam','GameTime','HomeTeam','player_id')
contest <- contest[,names(contest) %in% keep]

# do any player names exist in the DK file that don't exist in the Stattleship output?

# get logs for all players in the contest
keeper_game_logs <- game_logs[game_logs$player_id %in% contest$player_id,]

# subset logs by player and store in list 
contest_game_logs <- split(keeper_game_logs, keeper_game_logs$player_id)

# get minutes for tonight's contest
minutes <- bball_minutes_forecast(contest_game_logs, '2017-04-01') 










#############################

library(prophet)

pt_projections <- as.data.frame(matrix(ncol = 5, nrow = length(contest_game_logs)))
colnames(pt_projections) <- c('player_id', 'player_name', 'yhat', 'yhat_lower', 'yhat_upper')

hold <- c('ended_at', 'time_played_total')  

temp <- as.data.frame(contest_game_logs[1])
m <- temp[, grepl(paste(hold, collapse = '|'), names(temp))]
colnames(m)[1] <- 'y'
colnames(m)[2] <- 'ds'
m <- prophet(m)
future <- make_future_dataframe(m, periods = 10)
forecast <- predict(m, future)

# you got dat forecast - now how do you get the numbers that match today's contest and player_id? put that shit in the list
pid <- temp[1, grepl('player_id', names(temp))]
pn <- temp[1, grepl('player_id', names(temp))]
yh <- forecast[forecast$ds %in% as.Date('2017-04-01'), names(forecast) %in% 'yhat']
yhl <- forecast[forecast$ds %in% as.Date('2017-04-01'), names(forecast) %in% 'yhat_lower']
yhu <- forecast[forecast$ds %in% as.Date('2017-04-01'), names(forecast) %in% 'yhat_upper']
to_add <- c(pid, pn, yh, yhl, yhu)
pt_projections[1,] <- to_add


for (i in 1:length(contest_game_logs)) {
  temp <- as.data.frame(contest_game_logs[i])
  m <- temp[, grepl(paste(hold, collapse = '|'), names(temp))]
  colnames(m)[1] <- 'y'
  colnames(m)[2] <- 'ds'
  m <- prophet(m)
  future <- make_future_dataframe(m, periods = 14)
  forecast <- predict(m, future)
  
  # you got dat forecast - now how do you get the numbers that match today's contest and player_id? put that shit in the list
  pid <- temp[1, grepl('player_id', names(temp))]
  pn <- temp[1, grepl('player_id', names(temp))]
  yh <- forecast[forecast$ds %in% as.Date('2017-04-01'), names(forecast) %in% 'yhat']
  yhl <- forecast[forecast$ds %in% as.Date('2017-04-01'), names(forecast) %in% 'yhat_lower']
  yhu <- forecast[forecast$ds %in% as.Date('2017-04-01'), names(forecast) %in% 'yhat_upper']
  to_add <- c(pid, pn, yh, yhl, yhu)
  pt_projections[i,] <- to_add
}

# get fantasy points for tonight's contest (two ways to get stats: one using confidence / prediction intervals on ppm basis; the other using prophet)

# add that shit to the contest df

# optimize lineup






###################################################################################################

library(dplyr)
library(ggplot2)
library(plotly)
# still to load / install random forest, prophet, some other shit fa'sho




# get list of game logs data frames for active players in contest
# subset data frames by player
n_playing <- unique(contest$player_id)

# not working yet - IN A PERFECT WORLD THIS WOULD TRIGGER THE API CALLS!!!!
for (i in 1:length(n_playing)){
  temp <- game_logs[game_logs$player_id == n_playing[i],]
  contest_game_logs[i] <- temp 
  }

# put the game_log dataframes into a list of dataframes

# this works, so we can subset by recency 
# game_logs_week <- game_logs[(Sys.Date() - game_logs$ended_at <= 7),]

# curve fitting and shit for the active players
# forecast minutes / projected score
# generate average / window of score
# determine value somehow based on projected points, ownership, salary, floor

# lasso and ridge regression
# training data vs. testing data
# sensitivity analysis on robustness of model

# MAKE A BUNCH OF VARIATIONS OF EACH MODEL AND DOE TO DETERMINE THE BEST ONE - ITERATION, ITERATION, ITERATION