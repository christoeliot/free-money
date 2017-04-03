# generate time played in tonight's game forecast using prophet and return range 
# take the player game log for the season (maybe change time horizon eventually), parse into the columns needed, 
# generate time interval ranges, store that shit somewhere, then multiply it by the confidence interval 
# shit that you build and generate a DK points floor and ceiling 
bball_minutes_forecast <- function(df_list, contest_date) {
  library(prophet)
  
  pt_projections <- as.data.frame(matrix(ncol = 5, nrow = length(df_list)))
  colnames(pt_projections) <- c('player_id', 'player_name', 'yhat', 'yhat_lower', 'yhat_upper')
    
  hold <- c('ended_at', 'time_played_total')
  
  # i don't think this works right now - look into the progress bar 
  n <- length(df_list)
  pb <- txtProgressBar(min = 0, max = n, style=3)
  
  for (i in 1:length(df_list)) {
    temp <- as.data.frame(df_list[i])
    m <- temp[, grepl(paste(hold, collapse = '|'), names(temp))]
    colnames(m)[1] <- 'y'
    colnames(m)[2] <- 'ds'
    m$y[is.na(m$y)] <- 0
    m <- prophet(m)
    future <- make_future_dataframe(m, periods = 14)
    forecast <- predict(m, future)
    
    # you got dat forecast - now how do you get the numbers that match today's contest and player_id? put that shit in the list
    pid <- temp[1, grepl('player_id', names(temp))]
    pn <- temp[1, grepl('player_name', names(temp))]
    yh <- ifelse(!is.na(forecast$ds %in% as.Date(contest_date)), forecast[forecast$ds %in% as.Date(contest_date), names(forecast) %in% 'yhat'], 'NO PROJECTION FOR CONTEST DATE') 
    yhl <- ifelse(!is.na(forecast$ds %in% as.Date(contest_date)), forecast[forecast$ds %in% as.Date(contest_date), names(forecast) %in% 'yhat_lower'], 'NO PROJECTION FOR CONTEST DATE')
    yhu <- ifelse(!is.na(forecast$ds %in% as.Date(contest_date)), forecast[forecast$ds %in% as.Date(contest_date), names(forecast) %in% 'yhat_upper'], 'NO PROJECTION FOR CONTEST DATE')
    to_add <- c(pid, pn, yh, yhl, yhu)
    
    pt_projections[i,] <- to_add
  }
  return(pt_projections)
}

# use prophet for dkscore (not minutes driven)
bball_dkscore_forecast <- function(df_list, contest_date) {
  library(prophet)
  dks_projections <- as.data.frame(matrix(ncol = 5, nrow = length(df_list)))
  colnames(dks_projections) <- c('player_id', 'player_name', 'yhat', 'yhat_lower', 'yhat_upper')
  
  hold <- c('ended_at', 'dk_score')  
  for (i in 1:length(df_list)) {
    temp <- as.data.frame(df_list[i])
    m <- temp[, grepl(paste(hold, collapse = '|'), names(temp))]
    colnames(m)[1] <- 'y'
    colnames(m)[2] <- 'ds'
    m$y[is.na(m$y)] <- 0
    m <- prophet(m)
    future <- make_future_dataframe(m, periods = 14)
    forecast <- predict(m, future)
    
    # you got dat forecast - now how do you get the numbers that match today's contest and player_id? put that shit in the list
    pid <- temp[1, grepl('player_id', names(temp))]
    pn <- temp[1, grepl('player_name', names(temp))]
    yh <- forecast[forecast$ds %in% as.Date(contest_date), names(forecast) %in% 'yhat'] # CAN I FORCE PREDICTION TO OCCUR ON A CERTAIN DAY
    yhl <- forecast[forecast$ds %in% as.Date(contest_date), names(forecast) %in% 'yhat_lower']
    yhu <- forecast[forecast$ds %in% as.Date(contest_date), names(forecast) %in% 'yhat_upper']
    to_add <- c(pid, pn, yh, yhl, yhu)
    dks_projections[i,] <- to_add
  }
  return(dks_projections)
}

ppm_stats <- function(gls, days_back) {
  library(dplyr)
  ppm_stats <-
    gls %>%
    filter(Sys.Date() - gls$ended_at <= days_back ) %>%
    group_by(player_id) %>%
    summarise(games = length(game_played), 
              ppm_avg = mean(points / time_played_total), ppm_sd = sd(points / time_played_total), 
              apm_avg = mean(assists / time_played_total), apm_sd = sd(assists / time_played_total), 
              trpm_avg = mean(rebounds_total / time_played_total), trpm_sd = sd(rebounds_total / time_played_total), 
              orpm_avg = mean(rebounds_offensive / time_played_total), orpm_sd = sd(rebounds_offensive / time_played_total),
              drpm_avg = mean(rebounds_defensive / time_played_total), drpm_sd = sd(rebounds_defensive / time_played_total),
              fgmpm_ave = mean(field_goals_made / time_played_total), fgmpm_sd = sd(field_goals_made / time_played_total), 
              fgapm_ave = mean(field_goals_attempted / time_played_total), fgapm_sd = sd(field_goals_attempted / time_played_total),
              ftm_tot = sum(free_throws_made), fta_tot = sum(free_throws_attempted), ftp_tot = ftm_tot / fta_tot, ftp_game = mean(free_throws_pct),
              ftmpm_ave = mean(free_throws_made / time_played_total), ftmpm_sd = sd(free_throws_made / time_played_total),
              ftapm_ave = mean(free_throws_attempted / time_played_total), ftapm_sd = sd(free_throws_attempted / time_played_total),
              three_pm_pm_ave = mean(three_pointers_made / time_played_total), three_pm_pm_sd = sd(three_pointers_made / time_played_total),
              three_pa_pm_ave = mean(three_pointers_attempted / time_played_total), three_pa_pm_sd = sd(three_pointers_attempted / time_played_total),
              turnovers_pm_ave = mean(turnovers / time_played_total), turnovers_pm_sd = sd(turnovers / time_played_total),
              steals_pm_ave = mean(steals / time_played_total), steals_pm_sd = sd(steals / time_played_total), 
              blocks_pm_ave = mean(blocks / time_played_total), blocks_pm_sd = sd(blocks / time_played_total),
              pfouls_pm_ave = mean(personal_fouls / time_played_total), pfouls_pm_sd = sd(personal_fouls / time_played_total), 
              tfouls_pm_ave = mean(technical_fouls / time_played_total), tfouls_pm_sd = sd(technical_fouls / time_played_total),
              plusminus_ave = mean(plus_minus), plusminus_sd = sd(plus_minus))
}