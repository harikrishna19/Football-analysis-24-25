


# Libraries to load -------------------------------------------------------

library(ggplot2)
library(ggthemes)





# Graphs Liverpool Analysis-Time Ranges -----------------------------------------------


liv_match_summaries %>%
  dplyr::filter(Team == "Liverpool", Event_Type %in% c("Goal", "Penalty")) %>%
  dplyr::select(Team, Event_Half, Event_Type, Event_Players, Event_Time) %>%
  dplyr::mutate(Time_Range = factor(dplyr::case_when(
    Event_Time >= 0  & Event_Time <= 15  ~ "0-15",
    Event_Time >= 16 & Event_Time <= 30  ~ "16-30",
    Event_Time >= 31 & Event_Time <= 45  ~ "31-45",
    Event_Time >= 46 & Event_Time <= 60  ~ "46-60",
    Event_Time >= 61 & Event_Time <= 75  ~ "61-75",
    Event_Time >= 76 & Event_Time <= 90  ~ "76-90",
    Event_Time >= 91                     ~ "90+"
  ), levels = c("90+", "76-90", "61-75", "46-60", "31-45", "16-30", "0-15"))) %>% 
  dplyr::count(Time_Range, name = "Count") %>%
  ggplot(aes(x=Time_Range, y=Count)) +
  geom_col(fill="red") +
  coord_flip()+ labs(
    title = "Liverpool Goals and Penalties by Time Range",
    subtitle = "Distribution of Goals and Penalties Across Match Minutes",
    x = "Time Range (Minutes)",
    y = "Goal/Penalty Count",
    caption ="Built by: Hari Krishna"
  ) +
  theme_clean()  # Clean theme for better visuals




# Goal Scorers for Liverpool ----------------------------------------------

liv_match_summaries %>%
  dplyr::filter(Team == "Liverpool", Event_Type %in% c("Goal", "Penalty")) %>%
  dplyr::select(Team, Event_Half, Event_Type, Event_Players, Event_Time) %>% 
  dplyr::count(Event_Players, name = "Goals Scored") %>%
  dplyr::arrange(desc(`Goals Scored`))  # Sort in descending order









