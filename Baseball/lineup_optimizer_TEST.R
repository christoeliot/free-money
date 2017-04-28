library(lpSolve)
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

### TAKE OUT DUDES ON THE DL 


# generate lineups matrix or whatever 

# Prepare constraint matrix of zeros
A <- matrix(0, nrow = 8, ncol = nrow(contest))

#Set 1B parameters
j<-1
for (i in 1:nrow(contest)){
  if (contest$Position[i]=='1B'    || 
      contest$Position[i]=='1B/2B' || 
      contest$Position[i]=='1B/3B' ||
      contest$Position[i]=='1B/OF')
    A[j,i]<-1
}
#2B
j<-2
for (i in 1:nrow(contest)){
  if (contest$Position[i]=='2B'    || 
      contest$Position[i]=='1B/2B' || 
      contest$Position[i]== '2B/OF'||
      contest$Position[i]=='2B/SS' ||
      contest$Position[i]=='2B/3B')
    A[j,i]<-1
}
#3B
j<-3
for (i in 1:nrow(contest)){
  if (contest$Position[i]=='3B'    || 
      contest$Position[i]=='1B/3B' || 
      contest$Position[i]== '2B/3B'||
      contest$Position[i]=='3B/SS' ||
      contest$Position[i]=='3B/OF')
    A[j,i]<-1
}
#SS
j<-4
for (i in 1:nrow(contest)){
  if (contest$Position[i]=='SS'    || 
      contest$Position[i]=='1B/SS' || 
      contest$Position[i]== '2B/SS'||
      contest$Position[i]=='3B/SS' ||
      contest$Position[i]=='OF/SS')
    A[j,i]<-1
}
#C
j<-5
for (i in 1:nrow(contest)){
  if (contest$Position[i]=='C'    || 
      contest$Position[i]=='1B/C' || 
      contest$Position[i]== '2B/C'||
      contest$Position[i]=='3B/C' ||
      contest$Position[i]=='C/OF')
    A[j,i]<-1
}
#SP
j<-6
for (i in 1:nrow(contest)){
  if (contest$Position[i] %in% c('SP','RP')) 
    A[j,i]<-1
}

#OF
j<-7
for (i in 1:nrow(contest)){
  if (contest$Position[i]=='OF'    || 
      contest$Position[i]=='1B/OF' || 
      contest$Position[i]== '2B/OF'||
      contest$Position[i]=='3B/OF' ||
      contest$Position[i]=='C/OF')
    A[j,i]<-1
}
i<-1

A[8, ] <- contest$Salary                # salary <= 50000

# Prepare input for LP solver
contest <- contest %>% mutate(rand_points = rnorm(n(), mean = avg_DK, sd = sd_DK))

require(lpSolve)

objective.in <- contest$avg_DK
const.mat <- A
const.dir <- c('==', '==', '==', '==','==','==','==', '<=')
const.rhs <- c(1, 1, 1, 1,1,2,3, 50000)
sol <- lp(direction = 'max', objective.in, const.mat, const.dir, const.rhs, all.bin = TRUE)   
solution <- contest[which(sol$solution == 1), names(contest) %in% 
                  c('DK_player_name', 'Position', 'Salary', 
                    'GameInfo', 'team_abbrev.x', 'AvgPointsPerGame',
                    'avg_DK', 'sd_DK', 'player_id')]
solution$number <- with(solution, paste0('lineup_',1))

# Generate optimal lineup with lp solve
for(i in 2:25){
  objective.in <- contest$rand_points
  sol <- lp(direction = 'max', objective.in, const.mat, const.dir, const.rhs, all.bin = TRUE)   
  temp <- contest[which(sol$solution == 1), names(contest) %in% 
                        c('DK_player_name', 'Position', 'Salary', 
                          'GameInfo', 'team_abbrev.x', 'AvgPointsPerGame',
                          'avg_DK', 'sd_DK', 'player_id')]
  temp$number <- with(solution, paste0('lineup_',i))
  solution <- rbind(solution, temp)
  contest <- contest %>% mutate(rand_points = rnorm(n(), mean = avg_DK, sd = sd_DK))
}

# do 10000 simulations of these lineups (still neeed to add that hitters cannot score negative)
monte <- matrix(0, nrow = dim(solution)[1], ncol = 10001)
for (i in 1:dim(solution)[1]){
  monte[i,] <- c(solution$number[i], rnorm(10000, mean = solution$avg_DK[i], sd = solution$sd_DK[i]))
}

# aggregate
monte <- as.data.frame(monte)
colnames(monte)[1] <- 'lineup_num'
monte_agg <-
  monte %>%
  group_by(lineup_num) %>%
  summarise_each(funs(sum))

temp <- monte_agg$lineup_num

monte_agg <- as.data.frame(t(monte_agg[,-1]))
colnames(monte_agg) <- temp



##############################################################################################
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

#######################################################################################

###################################################################################
########################## INJURIES ######################################


#####################################################################################



#####################################################################################
########### GET A BUNCH OF TABLES AT VARYING TIME INTERVALS #########################

# blend into aggregate?
# get a bunch of lineups
# monte carlo
# group by scoring ranges
# mass enter lineups with strategic variations 


######################################################################################

# first pass - just optimize based on FFPG, then iterate what to optimize on

### HERE IS THE DUDE WHO FIGURED OUT HOW TO FRAME THE PROBLEM FIRST, LET US BEAT THAT
## https://github.com/MattBrown88/lpsolve---Daily-Fantasy-Sports-Optimization/blob/master/LPSolve%20-%20DFS%20Lineup%20Optimization.R

#####################################################################################################
# CREATE SOLUTIONS LIST OR WHATEVER FOR MONTE CARLO SIM
#Print players in optimal lineup

# LIST OF 'OPTIMAL LINEUPS'
for (i in 1:length(sol)){
  solutions <- as.data.frame(contest[inds, which(sol$solution == i]))
}

### BUILD YOUR POSITION MATRIX FASTER
for (j in 1:7){
  if (j == 1 || j == 2 || j == 3){
    
  }
  else (j > 3){
    
    
  }
  j <- j + 1
  }
  
}

#Write csv file of the optimal lineup
write.table(solution1, 'mydata.txt', sep='\t')
