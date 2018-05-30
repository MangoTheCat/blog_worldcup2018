


## This data frame contains information about the teams.
## You are free to add information here that you can use when determining winners
team_data <- tibble(
  number = 1:32,
  name = c("Egypt","Russia","Saudi Arabia","Uruguay",
           "Iran","Morocco","Portugal","Spain",
           "Australia","Denmark","France","Peru",
           "Argentina","Croatia","Iceland","Nigeria",
           "Brazil","Costa Rica","Switzerland","Serbia",
           "Germany","South Korea","Mexico","Sweden",
           "Belgium","England","Panama","Tunisia",
           "Colombia","Japan","Poland","Senegal"),
  group = rep(LETTERS[1:8], each=4),
  rating = c(151, 41, 1001, 34, # From a betting website
             501, 501, 26, 7,
             301, 101, 7.5, 201,
             10, 34, 201, 201,
             5, 501, 101, 201,
             5.5, 751, 101, 151,
             12, 19, 1001, 751,
             41, 301, 51, 201),
  elo = c(1646, 1685, 1582, 1890, # From https://www.eloratings.net/, May 12th
          1793, 1711, 1975, 2048,
          1714, 1843, 1984, 1906,
          1985, 1853, 1787, 1699,
          2131, 1745, 1879, 1770,
          2092, 1746, 1859, 1796,
          1931, 1941, 1669, 1649,
          1935, 1693, 1831, 1747)
)

group_match_data <- read.csv(text=
                               "team1,team2,date,goals1,goals2
                             Russia,Saudi Arabia,14/06/2018,,
                             Egypt,Uruguay,15/06/2018,,
                             Morocco,Iran,15/06/2018,,
                             Portugal,Spain,15/06/2018,,
                             France,Australia,16/06/2018,,
                             Argentina,Iceland,16/06/2018,,
                             Peru,Denmark,16/06/2018,,
                             Croatia,Nigeria,16/06/2018,,
                             Costa Rica,Serbia,17/06/2018,,
                             Germany,Mexico,17/06/2018,,
                             Brazil,Switzerland,17/06/2018,,
                             Sweden,South Korea,18/06/2018,,
                             Belgium,Panama,18/06/2018,,
                             Tunisia,England,18/06/2018,,
                             Colombia,Japan,19/06/2018,,
                             Poland,Senegal,19/06/2018,,
                             Russia,Egypt,19/06/2018,,
                             Portugal,Morocco,20/06/2018,,
                             Uruguay,Saudi Arabia,20/06/2018,,
                             Iran,Spain,20/06/2018,,
                             Denmark,Australia,21/06/2018,,
                             France,Peru,21/06/2018,,
                             Argentina,Croatia,21/06/2018,,
                             Brazil,Costa Rica,22/06/2018,,
                             Nigeria,Iceland,22/06/2018,,
                             Serbia,Switzerland,22/06/2018,,
                             Belgium,Tunisia,23/06/2018,,
                             South Korea,Mexico,23/06/2018,,
                             Germany,Sweden,23/06/2018,,
                             England,Panama,24/06/2018,,
                             Japan,Senegal,24/06/2018,,
                             Poland,Colombia,24/06/2018,,
                             Saudi Arabia,Egypt,25/06/2018,,
                             Uruguay,Russia,25/06/2018,,
                             Iran,Portugal,25/06/2018,,
                             Spain,Morocco,25/06/2018,,
                             Australia,Peru,26/06/2018,,
                             Denmark,France,26/06/2018,,
                             Nigeria,Argentina,26/06/2018,,
                             Iceland,Croatia,26/06/2018,,
                             Mexico,Sweden,27/06/2018,,
                             South Korea,Germany,27/06/2018,,
                             Serbia,Brazil,27/06/2018,,
                             Switzerland,Costa Rica,27/06/2018,,
                             England,Belgium,28/06/2018,,
                             Senegal,Colombia,28/06/2018,,
                             Panama,Tunisia,28/06/2018,,
                             Japan,Poland,28/06/2018,,
                             ",header=TRUE, stringsAsFactors = FALSE)

group_match_data$team1 = trimws(group_match_data$team1, which = "left")

# team_data_2014
team_data_2014 <- wcmatches %>% 
  filter(Year == 2014, str_detect(Stage, 'Group')) %>% 
  select(`Home Team Name`, Stage) %>% 
  distinct(.keep_all = TRUE)

team_data_2014$`Home Team Name` %in% as.character(elo_2014_start$team)
# TRUE, ... FALSE, ...
# There are a few teams with messed name

team_data_2014$`Home Team Name`[8] <- "Ivory Coast"
team_data_2014$`Home Team Name`[29] <- "Bosnia and Herzegovina"




team_data_2014$`Home Team Name`[!team_data_2014$`Home Team Name` %in% as.character(elo_2014_start$team)]

elo_2014_start <- elo_2014_start %>% 
  select(team, elo = rating) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(elo = as.numeric(elo))

elo_2014_final <- elo_2014_final %>% 
  select(team, elo = rating) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(elo = as.numeric(elo))

elo_2014 <- elo_2014_start %>% left_join(elo_2014_final, by = "team")

# edit names for iran, usa, korea
elo_2014 <- editData:::editData(elo_2014)

elo_2014 <- elo_2014 %>% 
  mutate(elo.x = as.numeric(elo.x)) %>% 
  mutate(elo.y = as.numeric(elo.y))

team_data_2014 <- team_data_2014 %>% 
  mutate(number = 1:n()) %>% 
  rename(name = `Home Team Name`,
         group = Stage) %>% 
  left_join(select(elo_2014, team, elo = elo.x), by = c("name" = "team")) 
  

group_match_data_2014 <- wcmatches %>% 
  filter(Year == 2014, str_detect(Stage, 'Group')) %>% 
  select(team1 = `Home Team Name`, team2 = `Away Team Name`) %>% 
  mutate(goals1 = as.numeric(NA)) %>% 
  mutate(goals2 = as.numeric(NA))

group_match_data_2014 <- editData:::editData(group_match_data_2014)

# fifa
fifa_2014_start <- fifa_2014_start %>% separate(`Total Points`, into = c("now", "last"), '\\(') %>% 
  mutate(last = str_remove(last, "\\)")) %>% 
  mutate(now = as.numeric(now)) %>% 
  mutate(last = as.numeric(last))#not sure why decimal is gone

fifa_2018_start <- fifa_2018_start %>% 
  separate(`Total Points`, into = c("fifa_start", "last"), '\\(') %>% 
  #mutate(last = str_remove(last, "\\)")) %>% 
  mutate(fifa_start = as.numeric(fifa_start)) %>% 
  select(-Rank, -last, team = Team) %>% 
  mutate(team = str_replace(team, "Korea Republic", "South Korea")) %>% 
  mutate(team = str_replace(team, "IR Iran", "Iran"))
  #mutate(last = as.numeric(last))#not sure why decimal is gone

# Append to team_data
team_data <- team_data %>% 
  left_join(fifa_2018_start, by = c("name" = "team"))


wcmatches_train <- tg %>% 
  select(-last, -now_log, -now_scale, -elo.x_log, -elo.x_scale,
         -Rank, fifa_start = now) %>% 
  mutate(team = str_replace(team, "Korea Republic", "South Korea")) %>% 
  mutate(team = str_replace(team, "IR Iran", "Iran"))

use_data(pkg = "worldcup", wcmatches_train, overwrite = T)
use_data(pkg = "worldcup", fifa_2018_start, overwrite = T)

