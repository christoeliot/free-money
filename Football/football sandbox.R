# download rosters for football season

# STILL TO DO:
  # remove all columns you do not need from each df to make dataset smaller (players & teams = done)
      # what do you not need? (remember you can always add stuff back in once your model gets sophisticated enough)
      # games and game logs 
  # figure out how to calculate DK score for D/ST
  # create dataset(s) to optimize on
  # link to DKSalaries sheet
  # generate matchup favorability
  # generate optimal lineups
  # monte carlo sim on lineups to generate CI
  # incorporate into Shiny app
  # make money

library(stattleshipR)
library(genalg)
library(plyr)
library(dplyr)


set_token('b53532dbcec8625311565ef38125b4ba')

sport <- 'football'
league <- 'nfl'

ep <- 'teams'
q_body = list()
tms <- ss_get_result(sport = sport, league = league, ep = ep, query = q_body, walk = TRUE)
teams <- do.call('rbind', lapply(tms, function(x) x$teams))
drop <- c('created_at', 'updated_at', 'color', 'colors', 'hashtag', 'hashtags', 'name', 
          'longitude', 'latitude', 'league_id', 'division_id')
teams <- teams[, !(names(teams) %in% drop)]
colnames(teams)[1] <- 'team_id'
write.csv(teams, 'Football/nfl_teams.csv')

 
# creating dataset(s) for exploration
# need to create a DK match key too!!!!

# i will need at least one dataset for individual players and one for teams (i.e. opponents) 
tms <- unique(teams$slug)
ep <- 'players'
players <- data.frame()
for (i in 1:length(tms)){
  q_body <- list(team_id = tms[i])
  plyrs <- ss_get_result(sport = sport, league = league, ep = ep, query = q_body, walk = TRUE)
  players <- rbind(players, do.call('rbind', lapply(plyrs, function(x) x$players)))
}
colnames(players)[1] <- 'player_id'
write.csv(players, 'Football/nfl_players.csv')


drop <- c('X', 'created_at', 'updated_at', 'active', 'bats', 'captain', 'city', 'country', 'draft_overall_pick',
          'draft_round', 'draft_season', 'draft_team_name', 'handedness', 'high_school', 'humanized_salary',
          'mlbam_id', 'nickname', 'position_name', 'pro_debut', 'salary_currency', 'school', 'sport', 'state',
          'uniform_number', 'unit_of_height','unit_of_weight', 'weight', 'years_of_experience', 'league_id')
players <- players[, !(names(players) %in% drop)]

ep <- 'injuries'
q_body <- list()
inj <- ss_get_result(sport = sport, league = league, ep = ep, query = q_body, walk = TRUE)
injuries <- do.call('rbind', lapply(inj, function(x) x$injuries))

games_2016 <- data.frame()
ep <- 'games'
qbody <- list(status = 'ended', season_id = 'nfl-2016-2017')
gms <- ss_get_result(sport = sport, league = league, ep = ep, query = qbody, walk = TRUE)
games_2016 <- do.call('rbind', lapply(gms, function(x) x$games))
drop <- c('official_ids')
games_2016 <- games_2016[, !(names(games_2016) %in% drop)]
write.csv(games_2016, 'Football/nfl_games_2016.csv')

drop <- c('X', 'created_at', 'updated_at', 'at_neutral_site', 'attendance', 'broadcast', 'clock', 'clock_secs',
          'daytime', 'duration', 'ended_at', 'humidity', 'interval', 'interval_number', 'interval_type', 'label',
          'name', 'on', 'period', 'period_label', 'score', 'started_at', 'status', 'internet_coverage', 'satellite_coverage',
          'television_coverage', 'temperature', 'temperature_unit', 'title', 'weather_conditions', 'wind_direction',
          'wind_speed', 'wind_speed_unit', 'official_ids')
colnames(games_2016)[1] <- 'game_id'
games_2016 <- games_2016[, !(names(games_2016) %in% drop)]

game_logs_2016 <- data.frame()
ep <- 'game_logs'
for (i in 1:length(tms)){
  qbody <- list(status = 'ended', season_id = 'nfl-2016-2017', team_id = tms[i])
  gls <- ss_get_result(sport = sport, league = league, ep = ep, query = qbody, walk = TRUE)
  game_logs_2016 <- rbind(game_logs_2016, do.call('rbind', lapply(gls, function(x) x$game_logs)))
  Sys.sleep(0.5)
}
colnames(game_logs_2016)[1] <- 'game_log_id'
write.csv(game_logs_2016, 'Football/nfl_game_logs_2016.csv')

# maybe add some of these columns back when you have a working model
drop <- c('X', 'created_at', 'updated_at', 'home_team_outcome', 'home_team_score', 'away_team_outcome', 'away_team_score',
          'is_away_team', 'average_yards_per_pass_attempt', 'average_yards_per_pass_completion', 'combined_tackles', 
          'defense_fumble_recoveries', 'defense_misc_assisted_tackles', 'defense_misc_solo_tackles',
          'defense_miscellaneous_fumble_recoveries', 'defense_special_teams_assisted_tackles', 'defense_special_teams_fumble_recoveries',
          'defense_special_teams_kicks_blocked', 'defense_special_teams_solo_tackles', 'defense_special_teams_tackles', 'defense_tackles',
          'defense_tackles_for_loss', 'defense_tackles_for_loss_yards', 'extra_points_attempts', 'extra_points_made', 'field_goal_attempts',
          'field_goal_attempts_19', 'field_goal_attempts_29', 'field_goal_attempts_39', 'field_goal_attempts_49', 'field_goal_attempts_50',
          'field_goals_attempted_against', 'field_goals_blocked', 'field_goals_made', 'field_goals_made_19', 'field_goals_made_29',
          'field_goals_made_39', 'field_goals_made_49', 'field_goals_made_50', 'field_goals_missed', 'field_goals_succeeded_against',
          'field_goals_succeeded_longest_yards', 'field_goals_succeeded_yards', 'fumbles_committed', 'fumbles_forced', 'fumbles_opposing_recovered',
          'fumbles_opposing_touchdowns', 'fumbles_own_recovered', 'fumbles_recovered_touchdowns_longest_yards', 'interception_pct',
          'interceptions_longest', 'interceptions_returned_for_touchdown', 'interceptions_returned_touchdowns_longest_yards',
          'interceptions_touchdown', 'interceptions_yards', 'kicking_singles', 'kickoff_longest_yards', 'kickoff_return_faircatches',
          'kickoff_return_longest_yards', 'kickoff_return_touchdowns_longest_yards', 'kickoff_return_yards', 'kickoff_returns', 
          'kickoff_returns_10_yards_plus', 'kickoff_returns_20_yards_plus', 'kickoff_returns_30_yards_plus', 'kickoff_returns_40_yards_plus',
          'kickoff_returns_50_yards_plus', 'kickoff_singles', 'kickoff_yards', 'kickoffs', 'misc_assisted_tackles', 'misc_combined_tackles',
          'misc_tackles', 'passer_rating', 'passes_10_yards_plus', 'passes_20_yards_plus', 'passes_30_yards_plus', 'passes_40_yards_plus',
          'passes_50_yards_plus', 'passes_completions', 'passes_defensed', 'passes_first_downs', 'passes_longest', 'passes_percentage',
          'passing_touchdowns_longest_yards', 'penalty_yards', 'points', 'punt_return_faircatches', 'punt_return_longest_yards',
          'punt_return_touchdowns', 'punt_return_touchdowns_longest_yards', 'punt_return_yards', 'punt_returns', 'punt_returns_10_yards_plus',
          'punt_returns_20_yards_plus', 'punt_returns_30_yards_plus', 'punt_returns_40_yards_plus', 'punt_returns_50_yards_plus', 
          'punting_net_yards', 'punting_singles', 'punts_blocked', 'punts_inside_twenty', 'punts_longest', 'punts_total', 'punts_yards_gross',
          'quarterback_hits', 'receiving_touchdowns_longest_yards', 'receptions_10_yards_plus', 'receptions_20_yards_plus', 'receptions_30_yards_plus',
          'receptions_40_yards_plus', 'receptions_50_yards_plus', 'receptions_average_yards_per', 'receptions_first_downs', 'receptions_fumbles',
          'receptions_longest', 'receptions_looks', 'rushes_10_yards_plus', 'rushes_20_yards_plus', 'rushes_30_yards_plus', 'rushes_40_yards_plus',
          'rushes_50_yards_plus', 'rushes_fumbles', 'rushes_longest', 'rushing_lost_yards', 'rushing_touchdowns_longest_yards', 'sacks_against_total',
          'sacks_against_yards', 'sacks_total', 'sacks_yards', 'safeties', 'tackles_assists', 'tackles_solo', 'total_touchdowns', 'touchbacks_total',
          'touchdown_pct')
game_logs_2016 <- game_logs_2016[, !(names(game_logs_2016) %in% drop)]
colnames(game_logs_2016)[1] <- 'game_log_id'

# scoring rules
# 4 pts / passing td, 0.04 pts / passing yd, 3 pts / 300 yd passing game, -1 pt / int, 6 pts / rushing td, 0.1 pts / rush yd,
# 3 pts / 100 yd rushing game, 6 pts / receiving td, 0.1 pts / receiving yd, 3 pts / 100 yd receiving game, 1 pt / reception,
# -1 pts / fumble


# fix this to be correct 
game_logs_2016$DK_Score <- with(game_logs_2016, 4 * passes_touchdowns + 0.04 * passes_yards_gross + 
                                  3 * ifelse(passes_yards_gross >= 300, 1, 0) - 1 * passes_interceptions +
                                  6 * rushes_touchdowns + 0.1 * rushes_yards + 3 * ifelse(rushes_yards >= 100, 1, 0) +
                                  6 * receptions_touchdowns + 0.1 * receptions_yards + 3 * ifelse(receptions_yards >= 100, 1, 0) +
                                1 * receptions_total - 1 * fumbles_lost)
                                

ep <- 'team_game_logs'
qbody <- list(status = 'ended', season_id = 'nfl-2016-2017')
tgls <- ss_get_result(sport = sport, league = league, ep = ep, query = qbody, walk = TRUE)
team_game_logs_2016 <- do.call('rbind', lapply(tgls, function(x) x$team_game_logs))
colnames(team_game_logs_2016)[1] <- 'team_game_log_id'

# 1 pts / sack, 2 pts / interception, 2 pts / fumble recovery, 6 pts / return td, 2 pts / safety, 2 pts / blocked kick,
# 2 pts / PAT return, 10 pts / 0 PA, 7 pts / 1-6 PA, 4 pts / 7-13 PA, 1 pts / 14-20 PA, 0 pts / 21-27 PA, -1 pts / 28-34 PA,
# -4 pts / 35+ PA
# only points scored while D/ST is on field count as PA

write.csv(team_game_logs_2016, 'Football/nfl_team_game_logs_2016.csv')

###################################################################
###################################################################
### START MODEL HERE ###

contest <- read.csv('Football/DKSalaries.csv')

# roster requirements
# 1 qb, 2 rb, 3 wr, 1 te, 1 flex, 1 dst
# salary of 50k

TE_ind <- which(contest$Position == 'TE') 
QB_ind <- which(contest$Position == 'QB') 
RB_ind <- which(contest$Position == 'RB')
WR_ind <- which(contest$Position == 'WR')
DST_ind <- which(contest$Position == 'DST')


evalFunc <- function(x) {
  team_ind <- which(x==1)
  current_solution_points <- sum(contest[team_ind, 5])
  current_solution_weight <- sum(contest[team_ind, 3])
  #   print(current_solution_weight)
  #   print(current_solution_points)
  
  if(current_solution_weight > sal.limit) return(abs(50000 - current_solution_weight))
  if(sum(x) != 9 )  return(200*(abs(sum(x)-9)))
  if(sum(x[QB_ind]) >1|| sum(x[DST_ind]) >1 )   return(500)
  if(sum(x[TE_ind]) == 2 && (sum(x[RB_ind]) >2 || sum(x[WR_ind]) >3)) {return(sum(x[TE_ind])*50)}
  if(sum(x[WR_ind]) == 4 && (sum(x[RB_ind]) >2 || sum(x[TE_ind]) >1)) {return(sum(x[WR_ind])*50)}
  if(sum(x[WR_ind]) > 4  || (sum(x[RB_ind]) >3 || sum(x[TE_ind]) >2)) {return(sum(x[WR_ind])*50)}
  return(-current_solution_points)
}

#Model building
iter = 500
player_size <- length(contest[,1])
#Salary Limit from DK
sal.limit <- 50000

GAmodel <- rbga.bin(size = player_size, popSize = 500, iters = iter, mutationChance = 0.01,  evalFunc = evalFunc)

cat(genalg:::summary.rbga(GAmodel,echo=T))

best_solution <- GAmodel$population[which.min(GAmodel$evaluations), ]
best_ind <- which(best_solution == 1)

contest[best_ind,]
