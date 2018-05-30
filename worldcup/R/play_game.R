#' Play game functions
#' @param team_data team data 
#' @param team1 team1 name
#' @param team2 team2 name
#' @param musthavewinner whether to include draw or not
#' 
#' @return Predicted goals scored for team1 and team2. A matrix with length(team1) rows and 2 columns with goals
#' @export

play_game <- function(team_data, play_fun, team1, team2, 
                      musthavewinner=FALSE, normalgoals = 2.75, ...) {
  # Sanity checks
  if (length(team1) != length(team2))
    stop("Lengths of team should be the same")
  
  if (any(team1==team2))
    stop("A team cannot play against itself")
  
  play_fun <- match.fun(play_fun)
  
  result <- play_fun(team_data, team1, team2, normalgoals, ...)
  
  # If we MUST have a winner then one simple trick is to add a random goal 
  # to one of the two teams that have the same score. Penalty goals seem rather 
  # random anyway
  if (musthavewinner) {
    result[result[,1]==result[,2],1] + 2*rbinom(sum(result[,1]==result[,2]), size=1, prob=.5) - 1
    
  }
  result
}

#' @export
play_fun_simplest <- function(team_data, team1, team2, normalgoals, ...) {
  ## Simplest version.
  ## All teams are equal - two independet poisson regressions
  Agoals <- rpois(length(team1), lambda = normalgoals / 2)
  Bgoals <- rpois(length(team2), lambda = normalgoals / 2)
  result <- cbind(Agoals, Bgoals)
  return(result)
}

# train_double_poisson(team_data = team_data, train_data = wcmatches_train)
#' @export
train_double_poisson <- function(team_data, train_data) {

#  mod_elo <- glm(goals ~ team + elo.x, family = poisson(link = log), data = train_data)
#  mod_fifa <- glm(goals ~ team + fifa_start, family = poisson(link = log), data = train_data)
  mod <- glm(goals ~ elo.x + fifa_start, family = poisson(link = log), data = train_data)
  
#  #team_data_backup <- team_data
#  
#  for (i in team_data$name[team_data$name %in% train_data$team]) {
#    # Some teams are new, not in train_data, I just manipulated lambda
#    if (!(i %in% c(
#      "Saudi Arabia", "Iran", "Morocco", "Denmark", "Serbia",
#      "Sweden", "Tunisia", "Poland", "Senegal"
#    ))) {
#      # Some teams have no elo data in train
#      team_data$lambda[match(i, team_data$name)] <-
#        predict(mod_elo,
#          data.frame(
#            team = team_data$name[match(i, team_data$name)],
#            elo.x = team_data$elo[match(i, team_data$name)]
#          ),
#          type = "response"
#        )
#    } else {
#      team_data$lambda[match(i, team_data$name)] <- 1.35
#    }
#  }
#  
#  #-------------
#  for (i in team_data$name[team_data$name %in% train_data$team]) {
#    # Some teams are new, not in train_data, I just manipulated lambda
#    #if (!(i %in% c(
#      #"Saudi Arabia", 
#      #"Iran", 
#      #"Morocco", "Denmark", "Serbia",
#      "Sweden", "Tunisia"#, 
#      #"Poland", "Senegal"
#    #))) {
#      # Some teams have no fifa_start data in train
#      team_data$lambda3[match(i, team_data$name)] <-
#        predict(mod,
#                data.frame(
#                  #team = team_data$name[match(i, team_data$name)],
#                  elo.x = team_data$elo[match(i, team_data$name)],
#                  fifa_start = team_data$fifa_start[match(i, team_data$name)]
#                ),
#                type = "response"
#        )
#    #} #else {
#      #team_data$lambda[match(i, team_data$name)] <- 1.35
#    #}
#  }
  
  #-----------
  team_data$lambda <- NA
  
  for (i in team_data$name) {
    team_data$lambda[match(i, team_data$name)] <-
      predict(mod,
              data.frame(
                #team = team_data$name[match(i, team_data$name)],
                elo.x = team_data$elo[match(i, team_data$name)],
                fifa_start = team_data$fifa_start[match(i, team_data$name)]
              ),
              type = "response"
      )    
  }  

  
  return(team_data)
  
  #team_data <- editData::editData(team_data)
}

#' @export
play_fun_double_poisson <- function(team_data, team1, team2, ...) {
  # Double poisson regressions in a match
  team_data <- train_double_poisson(team_data = team_data, train_data = wcmatches_train)
  
  # lambda1 and 2 are vector of mu for team1 and 2, 
  # all trained from train_double_poisson() 
  lambda1 <- team_data$lambda[team1]
  lambda2 <- team_data$lambda[team2]
  
  Agoals <- rpois(length(team1), lambda = lambda1) 
  Bgoals <- rpois(length(team2), lambda = lambda2)
  result <- cbind(Agoals, Bgoals)
  return(result)
}

#' @export
play_fun_skellam <- function(team_data, team1, team2, normalgoals, ...){
  ## Skellam distribution
  ## Parameters based on ratings from betting website 
  p1 <- .91/team_data$rating[team1]
  p2 <- .91/team_data$rating[team2]
  
  prob <- p1 / (p1 + p2)
  lambdaA <- FindParameter(prob)
  Agoals <- rpois(length(prob), lambdaA)
  Bgoals <- rpois(length(prob), normalgoals-lambdaA)
  result <- cbind(Agoals, Bgoals)
  return(result)
}

#' @export
play_fun_elo <- function(team_data, team1, team2, ...){
  ## ELO version (no update here). Using sapply here instead of
  ## vectorization in case the elo ranking should be updated after each match.
  #result <- t(sapply(seq_len(length(team1)), function(i) {
  #  AWinProb <- 1 / (1 + 10 ^ ((team_data$elo[team_data$name==team2[i]] - team_data$elo[team_data$name==team1[i]]) / 400))
  #  ## Agoals and Bgoals actually are ranking, not goals
  #  #Agoals <- rbinom(1, size = 1, prob = AWinProb)
  #  #Bgoals <- 1 - Agoals
  #  #cbind(Agoals, Bgoals)
  #  
  #  myres <- rbinom(1, size=1, prob=AWinProb)
  #  #fakegoals <- c(1, 0)
  #  if (myres == 0) fakegoals <- c(0, 1)
  #  #fakegoals
  #  # 
  #}))
  
  result <- t(sapply(seq_len(length(team1)), function(i) {
                        AWinProb <- 1/(1 + 10^((team_data$elo[team2[i]] - team_data$elo[team1[i]])/400))
                        myres <- rbinom(1, size=1, prob=AWinProb)
                            fakegoals <- c(1,0)  
                            if (myres==0)
                              fakegoals <- c(0,1)
                            fakegoals
                           })) 
  
  colnames(result) <- c("Agoals", "Bgoals")
  return(result)
  
  #--------------------------
  ## ELO version (no update here). Using sapply here instead of
  ## vectorization in case the elo ranking should be updated after each match.
}