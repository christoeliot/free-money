

# to subset by date 
game_logs_week <- game_logs[(Sys.Date() - game_logs$ended_at <= 7),]

