#' Find winners
#' 
#' Find the winners at various stages, round of 16, quarter-finals, semi-finals and final.
#' 
#' @export
find_group_winners <- function(team_data, group_match_data, play_fun) {
  
  ## Create a copy of the the matches that we can fill out
  group_match_results <- group_match_data
  
  ## Simulate each match that hasn't already been played  
  #pick <- (!complete.cases(group_match_results[c("goals1", "goals2")]))
  group_results <- play_game(
    team_data,
    team_data$number[match(group_match_data$team1, team_data$name)], # map from name to number
    team_data$number[match(group_match_data$team2, team_data$name)],
    play_fun = play_fun,
    musthavewinner = FALSE
  )
  
  ## Now add the results (the goals) to the match resuls
  group_match_results[, c("goals1", "goals2")] <- group_results
  ## Compute points earned per team for each match
  group_match_results$pointsForA <- with(group_match_results, 3*(goals1>goals2)+1*(goals1==goals2))
  group_match_results$pointsForB <- with(group_match_results, 3*(goals1<goals2)+1*(goals1==goals2))
  
  
  ## Okay the casing is a bit of a mess here. I do apologize.
  team_data$points <- 
    sapply(team_data$name, function(i) { sum(group_match_results[c("pointsForA", "pointsForB")][i == group_match_data[c("team1","team2")]]) })
  team_data$goalsFore <- sapply(team_data$name, function(i) { sum(group_match_results[c("goals1", "goals2")][i == group_match_data[c("team1","team2")]]) })
  
  team_data$goalsAgainst <- sapply(team_data$name, function(i) { sum(group_match_results[c("goals2", "goals2")][i == group_match_data[c("team1","team2")]]) })
  
  team_data$goalsDifference <- team_data$goalsFore-team_data$goalsAgainst
  
  
  # And here we find the winners within each group
  team_data %>% 
    group_by(group) %>% 
    arrange(desc(points), desc(goalsDifference), desc(goalsFore)) %>% 
    mutate(groupRank = row_number()) %>% 
    ungroup() %>%
    arrange(group, groupRank)
}

#' @export
find_knockout_winners <- function(team_data, match_data, play_fun) {
  ## Get the results
  results <- play_game(
    team_data,
    match_data[, 1], # is numeric 
    match_data[, 2], # is numeric
    play_fun = play_fun,
    musthavewinner = TRUE
  )
  
  ## Now form the goals
  goals <- cbind(match_data, results) %>% 
    data.frame()
  
  ## Find the teams that won
  winners <- match_data[cbind(seq(nrow(results)),
                              ifelse(results[, 1] > results[, 2], 1, 2))]
  return(list(winners = winners, goals = goals))
}

