



calculate_goals_assists <- function(data,team_name) {
  Goal_Scorers=data %>%
    dplyr::filter(Team == team_name, Event_Type %in% c("Goal", "Penalty")) %>%
    dplyr::select(Team, Event_Half, Event_Type, Event_Players, Event_Time) %>% 
    dplyr::count(Event_Players, name = "Goals Scored") %>%
    dplyr::arrange(desc(`Goals Scored`)) %>% 
    dplyr::mutate(Event_Players1=gsub(".*Assist: | [-—]" , "", Event_Players)) %>% 
    group_by(Event_Players1) %>% 
    summarise(GS = sum(`Goals Scored`)) %>% 
    arrange(desc(GS)) %>%
    mutate(Event_Players1 = factor(Event_Players1, levels = rev(Event_Players1))) %>%  # Preserve order
    ggplot(aes(x = Event_Players1, y = GS, fill = ifelse(Event_Players1 %in%
                                                           c("Mohamed Salah","Mohamed Salah Penalty Kick"),  "gold", "red"))) +
    geom_col() +  # No need to manually specify fill in geom_col()
    coord_flip() +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +  # Ensure integer breaks
    scale_fill_identity() +  # Ensure colors are used directly
    labs(
      title = "Top Goal Scorers for Liverpool 2024/25 season",
      x = "Players",
      y = "Goals Scored",
      caption = "Built by: Hari Krishna"
    ) +
    theme_clean()  # Assuming you want a clean theme
  return(Goal_Scorers)

}
calculate_goals_assists(data = liv_match_summaries,team_name="Liverpool")


