


# Libraries to load -------------------------------------------------------

library(ggplot2)
library(ggthemes)
library(patchwork)
library(dplyr)




# Graphs Liverpool Analysis-Time Ranges -----------------------------------------------


goal_ranges<- liv_match_summaries %>%
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
  ) +theme_clean()  # Clean theme for better visuals




# Goal Scorers for Liverpool ----------------------------------------------
#unique goal scoresers for liverpool
Goal_Scorers=liv_match_summaries %>%
  dplyr::filter(Team == "Liverpool", Event_Type %in% c("Goal", "Penalty")) %>%
  dplyr::select(Team, Event_Half, Event_Type, Event_Players, Event_Time) %>% 
  dplyr::count(Event_Players, name = "Goals Scored") %>%
  dplyr::arrange(desc(`Goals Scored`))  # Sort in descending order



Goal_Scorers$Event_Players1 <- gsub(".*Assist: | Kick -  " , "", Goal_Scorers$Event_Players)




goal_scorers<-Goal_Scorers %>%
  dplyr::count(Event_Players1, name = "Goals Scored") %>%
  dplyr::arrange(desc(`Goals Scored`)) %>%
  dplyr::mutate(Event_Players1 = factor(Event_Players1, levels = rev(Event_Players1))) %>%  # Preserve order
  ggplot(aes(x = Event_Players1, y = `Goals Scored`)) +
  geom_col(fill = "red") +
  coord_flip() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +  # Ensure integer breaks
  labs(
    title = "Top Goal Scorers for Liverpool 2024/25 season",
    x = "Players",
    y = "Goals Scored",
    caption ="Built by: Hari Krishna"
  ) +
  theme_clean()

goal_ranges/goal_scorers




