#' Get the winner i.e. top ranking teams
#' 
#' @export
get_winner <- function(result_data) {
  
  result_data <- transpose(result_data) %>% 
    extract2("final_ranking") %>% 
    bind_cols() %>%
    as.matrix()
  #mutate_all(funs(as.character(.)))
  winner <- table(result_data[1, ]) # first row, first ranking teams in each simulation
  names(winner) <- team_data$name[match(names(winner), team_data$number)]
  
  (winner / sum(winner)) %>%
    sort() %>% 
    as.data.frame()
}

#' @export
plot_winner <- function(result_stats){
  stats <- result_stats
  
  col_idx <- stats$Var1 %in% stats$Var1[(nrow(stats)-3):nrow(stats)]
  
  ggplot(stats, aes(x=Var1, y=Freq)) +
    geom_segment( aes(x=Var1, xend=Var1, y=0, yend=Freq), color=ifelse(col_idx, "#2576DF", "lightblue"), size=1 ) +
    geom_point(color=ifelse(col_idx, "#2576DF", "lightblue"), size=2) + #2576DF #82CAFC
    theme_light() +
    coord_flip() +
    theme(
      legend.position="none",
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    geom_text(aes(label=Freq), hjust=-0.2, size=3, color = "grey51") +
    ggtitle("Predicted Winners by Probabilities") + ylab(NULL) + xlab("Probability")
}

#' @export
get_top_scorer <- function(nsim, result_data){
  res <- map_df(result_data, "goals") %>%
    group_by(team) %>%
    summarise(goals = round(sum(goals) / nsim)) %>%
    ungroup()
  
  all <- team_data %>% 
    mutate(number = as.character(number)) %>% 
    right_join(res, by = c("number" = "team")) %>% 
    rename(team = name)

  all %>% 
    group_by(team) %>% 
    summarise(goals = sum(goals)) %>% 
    arrange(goals) %>% 
    mutate(team = reorder(team, goals)) %>% 
    arrange(desc(goals))
}

plot_top_scorer <- function(result_stats){
  
  col_idx <- result_stats$team %in% result_stats$team[(nrow(result_stats)-3):nrow(result_stats)]
  
  ggplot(result_stats, aes(x=team, y=goals)) + 
    geom_segment(aes(x=team, xend=team, y=0, yend=goals), color=ifelse(col_idx, "#BE3D28", "#F8C09F"), size=1 ) +
    geom_point(color=ifelse(col_idx, "#BE3D28", "#F8C09F"), size=2) +
    theme_light() +
    coord_flip() +
    theme(
      legend.position="none",
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    geom_text(aes(label=goals), hjust=-0.75, size=3, color = "grey51") +
    ggtitle("Total Scores (Goals) by Country") + ylab("Total Scores") + xlab(NULL)
}