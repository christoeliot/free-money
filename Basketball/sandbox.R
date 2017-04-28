
# to subset by date 
game_logs_week <- game_logs[(Sys.Date() - game_logs$ended_at <= 7),]


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




# curve fitting and shit for the active players
# forecast minutes / projected score
# generate average / window of score
# determine value somehow based on projected points, ownership, salary, floor

# lasso and ridge regression
# training data vs. testing data
# sensitivity analysis on robustness of model

# MAKE A BUNCH OF VARIATIONS OF EACH MODEL AND DOE TO DETERMINE THE BEST ONE - ITERATION, ITERATION, ITERATION

# generate ppg stats with a data frame or list of data frame inputs 
player_stats_pg <-
  game_logs %>%
  filter(game_played == TRUE) %>%
  group_by(player_id) %>%
  summarise(name = first(player_name), team = first(team_name), pos = first(position_abbreviation), games = length(game_played), 
            minutes_tot = sum(time_played_total) / 60, mpg_avg = minutes_tot / games, mpg_sd = sd(time_played_total) / 60, 
            points_tot = sum(points), ppg_avg = mean(points), ppg_sd = sd(points), 
            assists_tot = sum(assists), apg_avg = mean(assists), apg_sd = sd(assists), 
            rebounds_tot = sum(rebounds_total), rebounds_off = sum(rebounds_offensive), rebounds_def = sum(rebounds_defensive),
            trpg_avg = mean(rebounds_total), trpg_sd = sd(rebounds_total), 
            orpg_avg = mean(rebounds_offensive), orpg_sd = sd(rebounds_offensive),
            drpg_avg = mean(rebounds_defensive), drpg_sd = sd(rebounds_defensive),
            fgm_tot = sum(field_goals_made), fga_tot = sum(field_goals_attempted), fgpct_tot = fgm_tot / fga_tot, game_fg = mean(field_goals_pct),
            fgmpg_ave = mean(field_goals_made), fgmpg_sd = sd(field_goals_made), fgapg_ave = mean(field_goals_attempted), fgapg_sd = sd(field_goals_attempted),
            ftm_tot = sum(free_throws_made), fta_tot = sum(free_throws_attempted), ftp_tot = ftm_tot / fta_tot, ftp_game = mean(free_throws_pct),
            ftmpg_ave = mean(free_throws_made), ftmpg_sd = sd(free_throws_made),
            ftapg_ave = mean(free_throws_attempted), ftapg_sd = sd(free_throws_attempted),
            three_pm_tot = sum(three_pointers_made), three_pa_tot = sum(three_pointers_attempted),
            three_pct_tot = three_pm_tot / three_pa_tot, three_pct_game = mean(three_pointers_pct),
            three_pm_pg_ave = mean(three_pointers_made), three_pm_pg_sd = sd(three_pointers_made),
            three_pa_pg_ave = mean(three_pointers_attempted), three_pa_pg_sd = sd(three_pointers_attempted),
            turnovers_tot = sum(turnovers), turnovers_pg_ave = mean(turnovers), turnovers_pg_sd = sd(turnovers),
            steals_tot = sum(steals), steals_pg_ave = mean(steals), steals_pg_sd = sd(steals), 
            blocks_tot = sum(blocks), blocks_pg_ave = mean(blocks), blocks_pg_sd = sd(blocks),
            pfouls_tot = sum(personal_fouls), pfouls_pg_ave = mean(personal_fouls), pfouls_pg_sd = sd(personal_fouls), 
            tfouls_tot = sum(technical_fouls), tfouls_pg_ave = mean(technical_fouls), tfouls_pg_sd = sd(technical_fouls),
            plusminus_ave = mean(plus_minus), plusminus_sd = sd(plus_minus), real_salary = max(real_world_salary))

# generate ppm stats
player_stats_pm <-
  game_logs %>%
  filter(game_played == TRUE) %>%
  group_by(player_id) %>%
  summarise(name = first(player_name), team = first(team_name), pos = first(position_abbreviation), games = length(game_played), 
            minutes_tot = sum(time_played_total) / 60, mpg_avg = minutes_tot / games, mpg_sd = sd(time_played_total) / 60, 
            points_tot = sum(points), ppm_avg = mean(points / (time_played_total / 60)), ppm_sd = sd(points / (time_played_total / 60)), 
            assists_tot = sum(assists), apm_avg = mean(assists / (time_played_total / 60)), apm_sd = sd(assists / (time_played_total / 60)), 
            rebounds_tot = sum(rebounds_total), rebounds_off = sum(rebounds_offensive), rebounds_def = sum(rebounds_defensive),
            trpm_avg = mean(rebounds_total / (time_played_total / 60)), trpm_sd = sd(rebounds_total / (time_played_total / 60)), 
            orpm_avg = mean(rebounds_offensive / (time_played_total / 60)), orpm_sd = sd(rebounds_offensive / (time_played_total / 60)),
            drpm_avg = mean(rebounds_defensive / (time_played_total / 60)), drpm_sd = sd(rebounds_defensive / (time_played_total / 60)),
            fgm_tot = sum(field_goals_made), fga_tot = sum(field_goals_attempted), fgpct_tot = fgm_tot / fga_tot, game_fg = mean(field_goals_pct),
            fgmpm_ave = mean(field_goals_made / (time_played_total / 60)), fgmpm_sd = sd(field_goals_made / (time_played_total / 60)), 
            fgapm_ave = mean(field_goals_attempted / (time_played_total / 60)), fgapm_sd = sd(field_goals_attempted / (time_played_total / 60)),
            ftm_tot = sum(free_throws_made), fta_tot = sum(free_throws_attempted), ftp_tot = ftm_tot / fta_tot, ftp_game = mean(free_throws_pct),
            ftmpm_ave = mean(free_throws_made / (time_played_total / 60)), ftmpm_sd = sd(free_throws_made / (time_played_total / 60)),
            ftapm_ave = mean(free_throws_attempted / (time_played_total / 60)), ftapm_sd = sd(free_throws_attempted / (time_played_total / 60)),
            three_pm_tot = sum(three_pointers_made), three_pa_tot = sum(three_pointers_attempted),
            three_pm_pm_ave = mean(three_pointers_made / (time_played_total / 60)), three_pm_pm_sd = sd(three_pointers_made / (time_played_total / 60)),
            three_pa_pm_ave = mean(three_pointers_attempted / (time_played_total / 60)), three_pa_pm_sd = sd(three_pointers_attempted / (time_played_total / 60)),
            three_pct_tot = three_pm_tot / three_pa_tot, three_pct_game = mean(three_pointers_pct), 
            turnovers_tot = sum(turnovers), turnovers_pm_ave = mean(turnovers / (time_played_total / 60)), turnovers_pm_sd = sd(turnovers / (time_played_total / 60)),
            steals_tot = sum(steals), steals_pm_ave = mean(steals / (time_played_total / 60)), steals_pm_sd = sd(steals / (time_played_total / 60)), 
            blocks_tot = sum(blocks), blocks_pm_ave = mean(blocks / (time_played_total / 60)), blocks_pm_sd = sd(blocks / (time_played_total / 60)),
            pfouls_tot = sum(personal_fouls), pfouls_pm_ave = mean(personal_fouls / (time_played_total / 60)), pfouls_pm_sd = sd(personal_fouls / (time_played_total / 60)), 
            tfouls_tot = sum(technical_fouls), tfouls_pm_ave = mean(technical_fouls / (time_played_total / 60)), tfouls_pm_sd = sd(technical_fouls / (time_played_total / 60)),
            plusminus_ave = mean(plus_minus), plusminus_sd = sd(plus_minus))
