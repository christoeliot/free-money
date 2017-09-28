library(genalg)
library(stattleshipR)
library(dplyr)
library(xlsx)

# before doing this, make sure you have put the DK csv into the right folder and deprecated the old one

contest <- read.csv('Baseball/DKSalaries.csv')
colnames(contest)[2] <- 'DK_player_name'
colnames(contest)[6] <- 'team_abbrev'
contest$team_abbrev <- tolower(contest$team_abbrev)

keys <- read.xlsx('Baseball/mlb_dk_match_key.xlsx', sheetName = 'key')

contest <- merge(contest, keys, by = 'DK_player_name', all.x = TRUE)

# add check for missing players

# tonight's games, injuries, expected pitchers
set_token('b53532dbcec8625311565ef38125b4ba')

# this query returns super messy results  
sport <- 'baseball'
league <- 'mlb'
ep <- 'probable_pitchers'
q_body <- list(on = 'today')
pp <- ss_get_result(sport = sport, league = league, ep = ep, query = q_body, walk = TRUE)
probable_pitchers <- do.call('rbind', lapply(pp, function(x) x$probable_pitchers))

keep <- c('id', 'updated_at', 'game_id', 'player_id', 'team_id')
probable_pitchers <- probable_pitchers[, names(probable_pitchers) %in% keep]

probable_pitchers <- merge(probable_pitchers, contest, by = 'player_id')

# still need to add error checking ^^^^

# remove pitchers not in probable pitchers from contest list
pp_id <- unique(probable_pitchers$player_id)
scrub <- contest[contest$Position %in% c('SP', 'RP'),]
scrub <- scrub[!(scrub$player_id %in% pp_id),]
scrub <- unique(scrub$player_id)
contest <- contest[!(contest$player_id %in% scrub),]

# scrub injured players
ep <- 'injuries'
q_body <- list()
inj <- ss_get_result(sport = sport, league = league, ep = ep, query = q_body, walk = TRUE)
injuries <- do.call('rbind', lapply(inj, function(x) x$injuries))
colnames(injuries)[1] <- 'injury_id'
keep <- c('injury_id', 'started_on', 'status_updated_at', 'note',
          'status', 'status_label', 'player_id')
injuries <- injuries[, names(injuries) %in% keep]
injuries <- merge(injuries, players, by = 'player_id')
scrub <- injuries[injuries$status %in% c('D7', 'D10', 'D60'),]
scrub <- unique(scrub$player_id)
contest <- contest[!(contest$player_id %in% scrub),]


# merge w/ 2017 game logs, add dk_score, summarise with avg and std dev
game_logs_2017 <- read.csv('Baseball/mlb_game_logs_2017.csv')
keep <- c('player_id', 'game_id', 'game_played', 'doubles', 'hit_by_pitch', 'hits', 'home_runs', 'innings_pitched', 'runs', 'runs_batted_in', 'singles', 
          'stolen_bases', 'triples', 'walks', 'complete_games', 'pitcher_hit_by_pitch', 'pitcher_earned_runs', 'pitcher_hits', 
          'pitcher_strikeouts', 'pitcher_walks', 'shutouts', 'wins')
game_logs_2017 <- game_logs_2017[, names(game_logs_2017) %in% keep]

game_logs_2017 <- merge(game_logs_2017, contest, by = 'player_id')

game_logs_2017$DK_Score <- with(game_logs_2017, ifelse(game_logs_2017$Position %in% c('SP', 'RP'),
                           2.25 * innings_pitched + 2 * pitcher_strikeouts + 4 * wins - 2 * pitcher_earned_runs
                           - 0.6 * pitcher_hits - 0.6 * pitcher_walks - 0.6 * pitcher_hit_by_pitch + 2.5 * complete_games
                           + 2.5  * complete_games * shutouts + 5 * complete_games * ifelse(pitcher_hits < 1, 1, 0), 
                           3 * singles + 5 * doubles + 8 * triples + 10 * home_runs + 2 * runs_batted_in  
                           + 2 * runs + 2 * walks + 2 * hit_by_pitch + 5 * stolen_bases))

averages <- aggregate(DK_Score ~ player_id, data = game_logs_2017, FUN = mean)
colnames(averages)[2] <- 'avg_DK' 
stdevs <- aggregate(DK_Score ~ player_id, data = game_logs_2017, FUN = sd)
colnames(stdevs)[2] <- 'sd_DK'
stdevs[is.na(stdevs$sd_DK), names(stdevs) %in% 'sd_DK'] <- 0

summ <- merge(averages, stdevs, by = 'player_id')   
contest <- merge(contest, summ, by = 'player_id')

ind_1b <- which(contest$Position %in% c('1B', '1B/2B', '1B/3B', '1B/OF')) 
ind_2b <- which(contest$Position %in% c('2B', '1B/2B', '2B/OF', '2B/SS', '2B/3B')) 
ind_3b <- which(contest$Position %in% c('3B','1B/3B', '2B/3B', '3B/SS', '3B/OF'))
ind_ss <- which(contest$Position %in% c('SS', '1B/SS', '2B/SS', '3B/SS', 'OF/SS'))
ind_c <- which(contest$Position %in% c('C', '1B/C', '2B/C', '3B/C', 'C/OF'))
ind_of <- which(contest$Position %in% c('OF', '1B/OF', '2B/OF', '3B/OF', 'C/OF'))
ind_p <- which(contest$Position %in% c('SP','RP'))

#update eval function to check for total number of players and types of players

evalFunc <- function(x) {
  team_ind <- which(x == 1)
  solution_goal <- sum(contest[team_ind, 6])
  solution_cost <- sum(contest[team_ind, 4])
  
  if(solution_cost > salary_cap) return(abs(50000 - solution_cost) / solution_goal)
  if(sum(x) != 10)  return(200 * (abs(sum(x) - 10)) / solution_goal)
  if(sum(x[ind_p]) > 2) return(sum(x[ind_p]) * 1000 / solution_goal)
  if(sum(x[ind_1b]) > 1) return(sum(x[ind_1b]) * 50 / solution_goal)
  if(sum(x[ind_2b]) > 1) return(sum(x[ind_2b]) * 50 / solution_goal)
  if(sum(x[ind_3b]) > 1) return(sum(x[ind_3b]) * 50 / solution_goal)
  if(sum(x[ind_ss]) > 1) return(sum(x[ind_ss]) * 50 / solution_goal)
  if(sum(x[ind_c]) > 1) return(sum(x[ind_c]) * 50 / solution_goal)
  if(sum(x[ind_of]) > 3) return(sum(x[ind_of]) * 150 / solution_goal)
  if(sum(x[ind_p]) > 2) return(sum(x[ind_p]) * 50 / solution_goal)
  return(-solution_goal)
}



#Model building
iter = 500
player_size <- length(contest[,1])
#Salary Limit from DK
salary_cap <- 50000

GAmodel <- rbga.bin(size = player_size, popSize = 500, iters = iter, mutationChance = 0.01,  evalFunc = evalFunc)

cat(genalg:::summary.rbga(GAmodel,echo=T))

best_solution <- GAmodel$population[which.min(GAmodel$evaluations), ]
best_ind <- which(best_solution == 1)

contest[best_ind,]




# do 10000 simulations of these lineups (still neeed to add that hitters cannot score negative)
monte <- matrix(0, nrow = dim(solution)[1], ncol = 11)
for (i in 1:dim(solution)[1]){
  monte[i,] <- c(solution$number[i], rnorm(10, mean = solution$avg_DK[i], sd = solution$sd_DK[i]))
}

# aggregate
monte <- as.data.frame(monte)
colnames(monte)[1] <- 'lineup_num'
monte <- lapply(monte, function(x) type.convert(as.character((x))))
monte_agg <- aggregate(.~lineup_num, monte, sum)

################################## PICKLIST TEST GROUND ######################################

# are there new players in this contest? (maybe add team_abbrev as merge condition?)
# get all of them without matches
missing <- contest[is.na(contest$player_id),]
# only do if missing has shit in it
reset <- read.csv('Baseball/DKSalaries.csv')
colnames(reset)[2] <- 'DK_player_name'
reset$last_name <- lapply(strsplit(as.character(reset$DK_player_name), '\\ '), '[', 2)
reset$last_name <- tolower(reset$last_name)
colnames(reset)[6] <- 'team_abbrev'
reset$team_abbrev <- tolower(reset$team_abbrev)
missing <- merge(missing, reset, by = c('DK_player_name', 'team_abbrev'))
missing <- merge(missing, players, by = c('last_name', 'team_abbrev'))

wb <- loadWorkbook('Baseball/mlb_dk_match_key.xlsx')
sheets <- getSheets(wb)
removeSheet(wb, sheetName = 'import')
m <- createSheet(wb, sheetName = 'import')
addDataFrame(missing, m)
saveWorkbook(wb, 'Baseball/mlb_dk_match_key.xlsx')

####################################################################################################################

### HERE IS THE DUDE WHO FIGURED OUT HOW TO FRAME THE PROBLEM FIRST, LET US BEAT THAT
## https://github.com/MattBrown88/lpsolve---Daily-Fantasy-Sports-Optimization/blob/master/LPSolve%20-%20DFS%20Lineup%20Optimization.R


