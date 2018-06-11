#' Simulate tournament
#' 
#' TODO: To tidy up 
#' 
#' @param nsim Number of simulation runs
#' @param play_fun Which play_game function to use
#' @param teams The team data e.g. team_data
#' @param group_matches The group match data e.g. group_match_data
#' 
#' @return Simulated results including ranks and goals scored
#' @export
simulate_tournament <- function(nsim = 10,
                                play_fun = "play_fun_simplest",
                                teams = team_data, 
                                group_matches = group_match_data,
                                train_data = NULL) {
  
  play_fun <- match.fun(play_fun)
  
  replicate(
    nsim,
    simulate_one(
      play_fun = play_fun,
      teams = team_data,
      group_matches = group_match_data,
      train_data = train_data
    ),
    simplify = FALSE
  ) %>% 
    setNames(paste0("sim_", 1:nsim))
}


simulate_one <- function(play_fun,        
                         teams,            
                         group_matches,
                         train_data) {
  play_fun <- match.fun(play_fun)
  
  ## Step 1: Find the results from the group matches
  group_results <- find_group_winners(team_data = teams, 
                                      group_match_data = group_matches,
                                      play_fun = play_fun,
                                      train_data = train_data)
  
  ## Step 2: Design matches for the first part of the knockout match
  ## Select No.1 whthin group and No.2 in next group
  eigth_matches <- cbind(group_results$goals$number[seq(1, 32, by=4)], 
                         group_results$goals$number[c(6, 2, 14, 10, 22, 18, 30, 26)])
  ## and find the results
  eight <- find_knockout_winners(team_data, 
                                 eigth_matches,
                                 play_fun = play_fun,
                                 train_data)
  eigth_winners <- eight$winners
  
  ## Step 3: Design matches for the quarter finals and run them
  quarter_matches <- cbind(eigth_winners[c(1, 2, 5, 6)], eigth_winners[c(3, 4, 7, 8)])
  quarter <- find_knockout_winners(team_data, 
                                   quarter_matches, 
                                   play_fun = play_fun,
                                   train_data)
  quarter_winners <- quarter$winners
  
  ## Step 4: Semi finals ... yada yada yada
  semi_matches <- cbind(quarter_winners[c(1,3)], quarter_winners[c(2,4)])
  semi <- find_knockout_winners(team_data, 
                                semi_matches,
                                play_fun = play_fun,
                                train_data)
  semi_winners <- semi$winners
  
  ## Steps 5 and 6 Find number 1-4
  bronze_match <- matrix(quarter_winners[!quarter_winners %in% semi_winners], ncol=2)
  bronze <- find_knockout_winners(team_data, 
                                  bronze_match,
                                  play_fun = play_fun,
                                  train_data)
  bronze_winner <- bronze$winners
  
  final_match <- matrix(semi_winners, ncol=2)
  final <- find_knockout_winners(team_data, 
                                 final_match,
                                 play_fun = play_fun,
                                 train_data)
  final_result <- final$winners
  
  ## Return a vector with the teams in ranked order. 
  ## Note only the first 4 are individuals - the rest are really groups
  
  final_ranking <- c(final_result, # Number 1
                     final_match[!(final_match %in% final_result)], #2
                     bronze_winner, # Number 3
                     bronze_match[!(bronze_match %in% bronze_winner)], #4
                     quarter_matches[!(quarter_matches %in% quarter_winners)], # 5-8
                     eigth_matches[!(eigth_matches %in% eigth_winners)], # 9-16
                     seq(32)[!(seq(32) %in% eigth_matches)]
  )
  
  #final_ranking 
  
  total_goals <- list(eight, quarter, semi, bronze, final) %>% 
    map_df("goals")
  
  goals <- total_goals %>% 
    select(team1, goals1) %>% 
    setNames(c("team", "goals")) %>% 
    bind_rows(
      total_goals %>% 
        select(team2, goals2) %>% 
        setNames(c("team", "goals")) 
    ) %>% 
    rbind(setNames(select(group_results$goals, number, goalsFore), names(.))) %>% 
    mutate(team = as.character(team)) %>% 
    group_by(team) %>% 
    summarise(goals = sum(goals)) %>% 
    ungroup()
  
  return(list(final_ranking = final_ranking,
              goals = goals))
}