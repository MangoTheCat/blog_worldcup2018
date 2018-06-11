#' @title Play game functions
#' 
#' @name play_game
#' @param teams team data 
#' @param team1 team1 name
#' @param team2 team2 name
#' @param play_fun play game function
#' @param musthavewinner whether to include draw or not
#' 
#' @rdname 
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
    result[result[,1]==result[,2],1] + 2*rbinom(sum(result[,1]==result[,2]), 
                                                size=1, prob=.5) - 1
  }
  result
}

#' @rdname
play_fun_simplest <- function(team_data = NULL, team1, team2, normalgoals = 2.75, train_data = NULL) {

  ## Simplest version.
  ## All teams are equal - two independet poisson regressions
  Agoals <- rpois(length(team1), lambda = normalgoals / 2)
  Bgoals <- rpois(length(team2), lambda = normalgoals / 2)
  result <- cbind(Agoals, Bgoals)
  return(result)
}

#' @rdname
play_fun_skellam <- function(team_data, team1, team2, normalgoals = 2.75, ...){

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

#' @rdname
play_fun_elo <- function(team_data, team1, team2, ...){

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

# train_double_poisson(team_data = team_data, train_data = wcmatches_train)
train_double_poisson <- function(team_data, train_data) {

  # mod_elo <- glm(goals ~ team + elo.x, family = poisson(link = log), data = train_data)
  # mod_fifa <- glm(goals ~ team + fifa_start, family = poisson(link = log), data = train_data)
  mod <- glm(goals ~ elo + fifa_start, family = poisson(link = log), data = train_data)
  
  team_data$lambda <- NA
  team_data$lambda <- predict(mod, 
                          newdata = select(team_data, elo, fifa_start), 
                          type = "response")
  
  return(team_data)

}

#' @rdname
play_fun_double_poisson <- function(team_data, team1, team2, train_data = wcmatches_train, ...) {

  mod <- glm(goals ~ elo + fifa_start, family = poisson(link = log), data = train_data)
  
  team_data$lambda <- NA
  team_data$lambda <- predict(mod, 
                              newdata = select(team_data, elo, fifa_start), 
                              type = "response")
  
  # Double poisson regressions in a match
  #team_data <- train_double_poisson(team_data = team_data, train_data = train_data) #wcmatches_train
  
  # lambda1 and 2 are mean of team1 and 2
  # selected by number i.e. row id
  # trained from train_double_poisson() 
  lambda1 <- team_data$lambda[team1]
  lambda2 <- team_data$lambda[team2]
  
  Agoals <- rpois(length(team1), lambda = lambda1) 
  Bgoals <- rpois(length(team2), lambda = lambda2)
  result <- cbind(Agoals, Bgoals)
  #goals1 <- rpois(length(team1), lambda = lambda1) 
  #goals2 <- rpois(length(team2), lambda = lambda2)
  #data.frame(goals1, goals2)
  return(result)
}
