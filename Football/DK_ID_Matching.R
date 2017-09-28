contest <- read.csv('Football/DKSalaries.csv')
colnames(contest)[2] <- 'DK_player_name'
contest$DK_player_name <- tolower(contest$DK_player_name)
contest$DK_player_name <- iconv(contest$DK_player_name, to = 'ASCII//TRANSLIT')
contest$DK_player_name <- gsub("[^[:alnum:] ]", "", contest$DK_player_name)
contest$first_name <- lapply(strsplit(as.character(contest$DK_player_name), '\\ '), '[', 1)
contest$last_name <- lapply(strsplit(as.character(contest$DK_player_name), '\\ '), '[', 2)
colnames(contest)[6] <- 'team_abbrev'
contest$team_abbrev <- tolower(contest$team_abbrev)

players <- read.csv('Football/nfl_players.csv')
drop <- c('X')
players <- players[, !(names(players) %in% drop)]
players$first_name <- iconv(players$first_name, to = 'ASCII//TRANSLIT')
players$last_name <- iconv(players$last_name, to = 'ASCII//TRANSLIT')
players$player_name <- with(players, paste(players$first_name, players$last_name, sep = ' '))
players$player_name <- gsub('[^[:alnum:] ]', '', players$player_name)
players$player_name <- tolower(players$player_name)
players$last_name <- gsub('[^[:alnum:] ]', '', players$last_name)
players$last_name <- tolower(players$last_name)

teams <- read.csv('Football/nfl_teams.csv')
keep <- c('team_id', 'location', 'nickname', 'slug')
teams <- teams[, names(teams) %in% keep]
colnames(teams)[2] <- 'team_location'
colnames(teams)[3] <- 'team_name'
colnames(teams)[4] <- 'team_abbrev'
teams$team_abbrev <- gsub('nfl-','', teams$team_abbrev)
teams$team_abbrev <- ifelse(teams$team_abbrev == 'stl', 'lar', teams$team_abbrev)
teams$team_abbrev <- ifelse(teams$team_abbrev == 'jac', 'jax', teams$team_abbrev)


# match DST to get team_id

players <- merge(players, teams, by = 'team_id')

# need to add first name too!!
contest <- merge(contest, players, by = c('last_name', 'team_abbrev'), all.x = TRUE)

