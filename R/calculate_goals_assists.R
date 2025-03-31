

# Define names to count
names_to_count <- c("Salah", "Jota", "Díaz", "Gakpo","Núñez","Szoboszlai","Mac Allister","Arnold")

calculate_goals_assists <- function(data,team_name,players_to_check) {
  Goal_Scorers<-data %>%
    mutate(row_number = row_number()) %>%
    distinct(Matchweek, .keep_all = TRUE) %>% 
    mutate(Goal_Check=paste0(report$Home_Goals,report$Away_Goals,sep=","))
  
  # Count occurrences of each name
  name_counts <- sapply(names_to_count, function(name) sum(str_count(report$Goal_Check, paste0("\\b", name, "\\b")))) %>% as.data.frame()
  # Convert to a data frame
  name_counts_df <- data.frame(Name = names_to_count, GS = name_counts)
  colnames(name_counts_df)[2]<-"GS"
  Goal_Scorers1=name_counts_df %>%
    dplyr::arrange(desc(GS)) %>%
    mutate(Name = factor(Name, levels = rev(Name))) %>%  # Preserve order
    ggplot(aes(x = Name, y = GS,fill = ifelse(Name %in% "Salah",  "gold", "red"))) +
    geom_col() +  # No need to manually specify fill in geom_col()
    geom_text(aes(label = GS), hjust = 1.2, color = "black", fontface = "bold", size = 5) +  
    coord_flip() +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +  # Ensure integer breaks
    scale_fill_identity() +  # Ensure colors are used directly
    labs(
      title = "Top Goal Scorers for Liverpool 2024/25 Premier League season-updated till Match Week 29",
      x = "Players",
      y = "Goals Scored",
      caption = "Salah has been sensational this season with 27 goals-(18 Non Penalty and 9 Penalties)
       Next best is Diaz with 9,The next 4 goal scorers combined tally is 27 goals,this tells Salah is having a stellar season at Liverpool FC
      Though his contact expires June 2025 will he sign a new contract and stay at Liverpool?"
    ) +
    theme_few()+theme(
    plot.caption = element_text(hjust = 0.5, size = 10, face = "bold.italic", color = "black"), # Align left, italicized, and readable
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold", color = "black")
    )# Apply clean theme
  
  return(Goal_Scorers1)

}







