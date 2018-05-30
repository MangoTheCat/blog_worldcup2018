library(readr)
library(tidyverse)

wc2018 = read.csv("fifa-worldcup-2018-dataset\\World Cup 2018 Dataset.csv"
                  ,check.names = F)
wc2018 = wc2018[1:32, 1:19]

# wcmatches <- read.csv("fifa-world-cup/WorldCupMatches.csv",
#                 check.names = F)
# wcmatches <- wcmatches[1:852,]


wcmatches <- read_csv("fifa-world-cup/WorldCupMatches.csv", n_max = 852)

wcmatches = wcmatches [!duplicated(wcmatches),] # remove duplicates

wcplayers <- read_csv("fifa-world-cup/WorldCupPlayers.csv")

# join matches and players? 

team <- wcmatches %>% 
  select(Year, MatchID, `Home Team Name`, `Away Team Name`) %>% 
  gather(key = is_home, value = team, -Year, -MatchID) %>% 
  mutate(is_home = ifelse(str_detect(is_home, 'Home'), 1, 0))

#team %>% mutate(is_home = ifelse(str_detect(is_home, 'Home'), 1, 0)) %>% View()

goals <- wcmatches %>% 
  select(Year, MatchID, `Home Team Goals`, `Away Team Goals`) %>% 
  gather(key = is_home, value = goals, -Year, -MatchID) %>% 
  mutate(is_home = ifelse(str_detect(is_home, 'Home'), 1, 0))

tg <- left_join(team, goals) %>% 
  mutate(is_home = factor(is_home))

# Modelling
mod = glm(goals ~ team + is_home, family = poisson(link = log), data = tg)

# Goals per match
ggplot(tg) + 
  geom_histogram(aes(y=factor(is_home),fill=goals),stat="identity",position="dodge")




# Does home VS. guest makes any difference?

ggplot(tg, aes(x=goals, fill=factor(is_home))) + 
  geom_histogram(alpha=1, position="dodge") 
  

  
ggplot(tg, aes(goals, color=factor(is_home))) + 
  geom_freqpoly(alpha=1)
  


tg %>% filter(is_home == 1) %>% 
  group_by(team, Year) %>% summarise(mean = mean(goals))




#-------------------------
load_all("bivpois")
data(ex4.ita91)

# formula for modeling of lambda1 and lambda2
form1 <- ~c(team1,team2)+c(team2,team1)
#
# Model 1: Double Poisson
ex4.m1 <- lm.bp(g1~1, g2~1, l1l2=form1, zeroL3=TRUE, data=ex4.ita91)
#
# Models 2-5: bivariate Poisson models
ex4.m2 <- lm.bp(g1~1,g2~1, l1l2=form1, data=ex4.ita91)
ex4.m3 <- lm.bp(g1~1,g2~1, l1l2=form1, l3=~team1, data=ex4.ita91)
ex4.m4 <- lm.bp(g1~1,g2~1, l1l2=form1, l3=~team2, data=ex4.ita91)
ex4.m5 <- lm.bp(g1~1,g2~1, l1l2=form1, l3=~team1+team2, data=ex4.ita91)
#
# Model 6: Zero Inflated Model
ex4.m6 <- lm.dibp(g1~1,g2~1, l1l2=form1, data=ex4.ita91, jmax=0)
#
# Models 7-11: Diagonal Inflated Bivariate Poisson Models
ex4.m7 <- lm.dibp(g1~1,g2~1, l1l2=form1, data=ex4.ita91, distribution="geometric")
ex4.m8 <- lm.dibp(g1~1,g2~1, l1l2=form1, data=ex4.ita91, jmax=1)
ex4.m9 <- lm.dibp(g1~1,g2~1, l1l2=form1, data=ex4.ita91, jmax=2)
ex4.m10 <- lm.dibp(g1~1,g2~1, l1l2=form1, data=ex4.ita91, jmax=3)
ex4.m11 <- lm.dibp(g1~1,g2~1, l1l2=form1, data=ex4.ita91, distribution="poisson")
#
# Models 12: Diagonal Inflated Double Poisson Model
ex4.m12 <- lm.dibp(g1~1,g2~1, l1l2=form1, data=ex4.ita91, distribution="poisson",
                   zeroL3=TRUE)


# monitoring parameters for model 1: Dbl Poisson
ex4.m2$beta1 # model parameters for lambda1
ex4.m2$beta2 # model parameters for lambda2.
# All are the same as in beta1 except the intercept
ex4.m2$beta3 # model parameters for lambda3 (Here only the intercept)
ex4.m2$beta2[1] # Intercpept for lambda2.
ex4.m2$beta2[1]-ex4.m2$beta2[2] # estimated home effect


dat = wcmatches %>% 
  select(Year, MatchID, 
         g1 = `Home Team Goals`, g2 = `Away Team Goals`, 
         team1 = `Home Team Name`, team2 = `Away Team Name`) %>% 
  filter(Year > 1992)

# ex4.m11 <- lm.bp(g1~1, g2~1, l1l2=form1, zeroL3=TRUE, data=dat)
# ex4.m22 <- lm.bp(g1~1,g2~1, l1l2=form1, data=dat)




tg = tg %>% filter(Year > 1992) %>% 
  left_join(elo_2014, by = "team") %>% 
  left_join(fifa_2014_start, by  = c("team" = "Team")) %>% 
  mutate(now_log = log(now)) %>% # fifa and elo ratings on original scale can't distinguish the teams enough
  mutate(elo.x_log = log(elo.x)) %>% 
  mutate(now_scale = scale(now)) %>% 
  mutate(elo.x_scale = scale(elo.x))

mod_is_home = glm(goals ~ is_home, family = poisson(link = log), data = tg)

mod = glm(goals ~ is_home + team, family = poisson(link = log), data = tg)

mod_elo = glm(goals ~ is_home + team + elo.x, family = poisson(link = log), data = tg)

mod_elo_fifa = glm(goals ~ is_home + team + elo.x + now, family = poisson(link = log), data = tg)

mod_elo_fifa_log = glm(goals ~ team + elo.x_log + now_log, family = poisson(link = log), data = tg)

mod_elo_fifa_scale = glm(goals ~ is_home + team + elo.x_scale + now_scale, family = poisson(link = log), data = tg)


mod_elo = glm(goals ~ team + elo.x, family = poisson(link = log), data = tg)



#---------
predict(mod, 
        data.frame(is_home="1", team="Brazil"), type="response")
# 2.350695

predict(mod, 
        data.frame(is_home="1", team="Germany"), type="response")
# 2.36092 

predict(mod, 
        data.frame(is_home="1", team="Netherlands"), type="response")
# 2.050794

#---------
predict(mod_elo, 
        data.frame(is_home="1", team="Brazil", elo.x=2131), type="response") # 2018
# 1.944785

predict(mod_elo, 
        data.frame(is_home="1", team="Brazil", elo.x=2138), type="response") # 2014
# 1.944785

predict(mod_elo, 
        data.frame(is_home="1", team="Germany", elo.x=2092), type="response") # 2018
# 2.104631 

predict(mod_elo, 
        data.frame(is_home="1", team="Germany", elo.x=2073), type="response") # 2014
# 2.104631


#---------- 
predict(mod_elo_fifa, 
        data.frame(is_home="1", team="Brazil", elo.x=2131, now=1384), type="response") # 2018
# 1.944785


predict(mod_elo_fifa, 
        data.frame(is_home="1", team="Germany", elo.x=2092, now=1544), type="response") # 2018
# 2.104631

#---------- 
# no difference!!!
predict(mod_elo_fifa_log, 
        data.frame(is_home="1", team="Brazil", elo.x_log=log(2131), now_log=log(1384)), type="response") # 2018
# 1.944785

#----------
predict(mod_elo_fifa_log, 
        data.frame(team="Brazil", elo.x_log=log(2131), now_log=log(1384)), type="response") # 2018
# 1.921053

predict(mod_elo_fifa_log, 
        data.frame(team="Germany", elo.x_log=log(2092), now_log=log(1544)), type="response") # 2018
# 2.078947

predict(mod_elo_fifa_log, 
        data.frame(team="Brazil", elo.x_log=log(2138), now_log=log(1242)), type="response") # 2014
# 1.921053
# actual is 11/7 = 1.571429

predict(mod_elo_fifa_log, 
        data.frame(team="Germany", elo.x_log=log(2073), now_log=log(1300)), type="response") # 2014
# 2.078947
# actual is 18/7 = 2.571429





#---------- rvest elo
#library(rvest)
#library(V8)
#
#world_cup <- read_html("https://www.eloratings.net/2018_World_Cup")
#
#world_cup %>%
#  html_nodes("script") 
#-------------------


play_game <- function(team_data, team1, team2, ...) {
  ## Simplest version. All teams are equal in skill
  result <- cbind(rpois(length(team1), lambda=2.75/2), 
                  rpois(length(team1), lambda=2.75/2))
}

play_game(team_data, "Russia", "Saudi Arabia")

play_game(team_data, "Germany", "Brazil")

play_game(team_data, "Belgium", "England")

#--------------------
# probability of draw between home and away team
skellam::dskellam(0,mean(wcmatches$`Home Team Goals`),mean(wcmatches$`Away Team Goals`))

# probability of home team winning by one goal
skellam::dskellam(1,mean(wcmatches$`Home Team Goals`),mean(wcmatches$`Away Team Goals`))

poisson_model <- 
  rbind(
    data.frame(goals=epl_data$homeGoals,
               team=epl_data$home,
               opponent=epl_data$away,
               home=1),
    data.frame(goals=epl_data$awayGoals,
               team=epl_data$away,
               opponent=epl_data$home,
               home=0)) %>%
  glm(goals ~ home + team +opponent, family=poisson(link=log),data=.)
summary(poisson_model)

# calculate expected number of goals for each team in match
predict(poisson_model, 
        data.frame(home=1, team="Chelsea", 
                   opponent="Sunderland"), type="response")


# skelam distribution/goal difference plot
mutate(epl_data, goal_diff=homeGoals-awayGoals) %>% group_by(goal_diff) %>%
  summarize(actual=n()/nrow(.)) %>% 
  inner_join(data.frame(goal_diff=-5:5,
                        pred=skellam::dskellam(-5:5,mean(epl_data$homeGoals),
                                               mean(epl_data$awayGoals))),
             by=c("goal_diff")) %>%
  ggplot(aes(x=as.factor(goal_diff))) + 
  geom_bar(aes(y = actual,fill="d2"), stat="identity") +
  geom_line(aes(y = pred, group = 1, color = "d1"), size=1.25) +
  scale_colour_manual(name="Skellam",labels="", values=c("d1" = "blue", "d2" = "red"))+
  scale_fill_manual(name="Actual",labels="",values="red")  +
  ggtitle("Difference in Goals Scored (Home Team vs Away Team)")  + xlab("Home Goals - Away Goals") + ylab("Proportion of Matches") +
  my_post_theme

# skelam distribution/goal difference plot
wcmatches %>% 
  filter(Year > 1992) %>% 
  mutate(goal_diff=`Home Team Goals` - `Away Team Goals`) %>% group_by(goal_diff) %>%
  summarize(actual=n()/nrow(.)) %>% 
  inner_join(data.frame(goal_diff=-5:5,
                        pred=skellam::dskellam(-5:5,mean(wcmatches$`Home Team Goals`),
                                               mean(wcmatches$`Away Team Goals`))),
             by=c("goal_diff")) %>%
  ggplot(aes(x=as.factor(goal_diff))) + 
  geom_bar(aes(y = actual,fill="d2"), stat="identity") +
  geom_line(aes(y = pred, group = 1, color = "d1"), size=1.25) +
  scale_colour_manual(name="Skellam",labels="", values=c("d1" = "blue", "d2" = "red"))+
  scale_fill_manual(name="Actual",labels="",values="red")  +
  ggtitle("Difference in Goals Scored (Home Team vs Away Team)")  + xlab("Home Goals - Away Goals") + ylab("Proportion of Matches")


# Year 2014
scores_2014 <- filter(tg, Year == 2014) %>% 
  group_by(team) %>% 
  summarise(goals = sum(goals)) %>% 
  arrange(goals)

scores_2014 %>% summary()

ggplot(scores_2014, aes(x=reorder(team, goals), y=goals)) +
  geom_bar(stat="identity", fill="lightblue") +
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=goals), hjust=-0.2, size=4) +
  ggtitle("Total Scores (Goals) by Country") + ylab("Total Scores") + xlab(NULL)

## manual check 
#wcmatches %>% 
#  select(Year, `Home Team Name`, `Away Team Name`, `Home Team Goals`, `Away Team Goals`) %>%
#  filter(Year == 2014) %>% View()