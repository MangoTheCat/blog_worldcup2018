#' Find winners
#' 
#' Find the winners at various stages, round of 16, quarter-finals, semi-finals and final.
#' TODO: To tidy up. I steped into it and stepped out. 
#' 
#' @export
find_group_winners <- function(team_data, group_match_data, play_fun, train_data = NULL, ...) {
  
  ## Create a copy of the matches that we can fill out
  group_match_results <- group_match_data
  
  ## Simulate each match that hasn't already been played  
  group_results <- play_game(
    team_data,
    team_data$number[match(group_match_data$team1, team_data$name)], # map from name to number
    team_data$number[match(group_match_data$team2, team_data$name)],
    play_fun = play_fun,
    train_data = train_data,
    musthavewinner = FALSE,
    ...)
  
  #play_foo <- function(team1, team2){
  #  goals1 <- rpois(length(team1), lambda = normalgoals / 2)
  #  goals2 <- rpois(length(team2), lambda = normalgoals / 2)
  #  #result <- cbind(Agoals, Bgoals) %>% data.frame()
  #  #return(result)
  #  data.frame(goals1, goals2)
  #}
  
  #set.seed(000)
  #group_match_data %>%
  #  mutate(result = map2(team1, team2, play_foo)) %>% unnest() %>% 
  #  View()
 
  #set.seed(000)
  #group_match_data$goals1 <- NULL
  #group_match_data$goals2 <- NULL
  #group_match_results2 <- group_match_data %>%
  #  bind_cols(map2_dfr(.$team1, .$team2, play_foo)) %>%
  #  mutate(points1 = 3*(goals1 > goals2)+1*(goals1==goals2)) %>% 
  #  mutate(points2 = 3*(goals1 < goals2)+1*(goals1==goals2)) 
  #
  #x1 = group_match_results2 %>% 
  #  group_by(team1) %>% 
  #  summarise(points = sum(points1))
  #
  #x2 = group_match_results2 %>% 
  #  group_by(team2) %>% 
  #  summarise(points = sum(points2))
  #
  #
  #
  #
  #teams2 <- team_data %>% 
  #  left_join(select(group_match_results2, team1, points1), by = c("name" = "team1")) %>% 
  #  left_join(select(group_match_results2, team2, points2), by = c("name" = "team2"))
  #  mutate(points = )
  

  ## Now add the results (the goals) to the match results
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
  goals <- team_data %>% 
    group_by(group) %>% 
    arrange(desc(points), desc(goalsDifference), desc(goalsFore)) %>% 
    mutate(groupRank = row_number()) %>% 
    ungroup() %>%
    arrange(group, groupRank)
  
  winners <- goals %>% 
    filter(groupRank %in% c(1, 2)) %>%
    select(number) %>% 
    flatten_int()
  
  return(list(winners = winners, goals = goals))
  
}

#' @export
find_knockout_winners <- function(team_data, match_data, play_fun, train_data = NULL, ...) {
  ## Get the results
  results <- play_game(
    team_data,
    match_data[, 1], # is numeric 
    match_data[, 2], # is numeric
    play_fun = play_fun,
    musthavewinner = TRUE,
    #normalgoals = 2.75,
    train_data = train_data,
    ...
  )
  
  ## Now form the goals
  goals <- data.frame(match_data, results) %>% 
    setNames(c("team1", "team2", "goals1", "goals2"))
  
  ## Find the teams that won
  winners <- match_data[cbind(seq(nrow(results)),
                              ifelse(results[, 1] > results[, 2], 1, 2))]
  return(list(winners = winners, goals = goals))
}

