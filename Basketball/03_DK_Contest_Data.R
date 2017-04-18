source('00_Helpers.R')

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

# forecast minutes for tonight's contest and infer dfs score
minutes <- bball_minutes_forecast(contest_game_logs, '2017-04-01')
to_summ <- keeper_game_logs[, !(names(keeper_game_logs) %in% 'team_id')]
ppm_stats_month <- ppm_stats(to_summ, 30)
colnames(ppm_stats_month) <- paste0('30day_', colnames(ppm_stats_month))
ppm_stats_fortnight <- ppm_stats(to_summ, 14)
colnames(ppm_stats_fortnight) <- paste0('14day_', colnames(ppm_stats_fortnight))

# forecast fantasy points for tonight's contest
dkpoints <- bball_dkscore_forecast(contest_game_logs, '2017-04-01')

# add that shit to the contest df
model_1 <- merge(contest, minutes, by = 'player_id', all = TRUE)
model_1 <- merge(model_1, ppm_stats_month, by = 'player_id')
model_2 <- merge(contest, dkpoints, by = 'player_id', all = TRUE)

# optimize lineup
