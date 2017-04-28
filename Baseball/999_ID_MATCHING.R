## updating the matching key

contest <- read.csv('Baseball/DKSalaries.csv')
colnames(contest)[2] <- 'DK_player_name'
contest$last_name <- lapply(strsplit(as.character(contest$DK_player_name), '\\ '), '[', 2)
contest$last_name <- tolower(contest$last_name)
colnames(contest)[6] <- 'team_abbrev'
contest$player_name <- iconv(contest$DK_player_name, to = 'ASCII//TRANSLIT')
contest$player_name <- gsub("[^[:alnum:] ]", "", contest$DK_player_name)
contest$player_name <- tolower(contest$player_name)
contest$team_abbrev <- tolower(contest$team_abbrev)

players <- read.csv('Baseball/mlb_players.csv')
colnames(players)[2] <- 'player_id'
keep <- c('player_id', 'active', 'bats', 'birth_date', 'first_name', 'last_name', 
          'position_abbreviation', 'name', 'position_name', 'team_id', 'playing_position_id')
players <- players[, names(players) %in% keep]
players <- players[players[,'active'] == TRUE,]
players$first_name <- iconv(players$first_name, to = 'ASCII//TRANSLIT')
players$last_name <- iconv(players$last_name, to = 'ASCII//TRANSLIT')
players$player_name <- with(players, paste(players$first_name, players$last_name, sep = ' '))
players$player_name <- gsub('[^[:alnum:] ]', '', players$player_name)
players$player_name <- tolower(players$player_name)
players$last_name <- gsub('[^[:alnum:] ]', '', players$last_name)
players$last_name <- tolower(players$last_name)

teams <- read.csv('Baseball/mlb_teams.csv')
colnames(teams)[2] <- 'team_id'
keep <- c('team_id', 'location', 'nickname', 'slug')
teams <- teams[, names(teams) %in% keep]
colnames(teams)[2] <- 'team_location'
colnames(teams)[3] <- 'team_name'
colnames(teams)[4] <- 'team_abbrev'
teams$team_abbrev <- gsub('mlb-','', teams$team_abbrev)
teams$team_abbrev[teams$team_abbrev == 'la'] <- 'lad'

players <- merge(players, teams, by = 'team_id')

contest <- merge(contest, players, by = c('last_name', 'team_abbrev'), all.x = TRUE)