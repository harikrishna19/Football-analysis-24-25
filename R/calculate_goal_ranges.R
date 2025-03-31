
# Libraries to load -------------------------------------------------------

library(ggplot2)
library(ggthemes)
library(patchwork)
library(dplyr)
library(magrittr)
library(roxygen)




#Custom functions

#' Title
#'
#' @param data pass the data as dataframe
#' @param team_name specify the team name as a string 
#' @param team_colour specify the team colour for the plot
#'
#' @returns ggplot
#'
#' @examples  calculate_goal_ranges(liv_match_summaries,"Liverpool","red")

calculate_goal_ranges <- function(data,team_name,team_colour) {
  goal_ranges <- data %>%
    dplyr::filter(Team == team_name, Event_Type %in% c("Goal", "Penalty")) %>%
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
    
    # Create a Fill_Colour column for conditional coloring
    dplyr::mutate(Fill_Colour = ifelse(Time_Range %in% c("46-60", "61-75", "16-30"), "gold", team_colour)) %>%
    
    ggplot(aes(x = Time_Range, y = Count, fill = Fill_Colour)) +  # Use the new Fill_Colour column
    geom_col() +  
    geom_hline(yintercept = 10, linetype = "dashed", color = "red", size = 1)+
    scale_fill_identity() +  
    coord_flip() +  
    labs(
      title = "Liverpool Goal Distribution across different time range",
      subtitle = "Distribution of Goals and Penalties Across Match Minutes",
      x = "Time Range (Minutes)",
      y = "Goal Count",
      caption = "Key scoring periods: 
                16-30, 46-60, and 61-75 minutes where
                Liverpool have score more than 10+ goals in the current season of the PL
                Data Source: {worldfootballR package}-FBref Data 
                Built by: Hari Krishna"
  
    ) + 
    theme_clean() +
    theme(
      plot.caption = element_text(hjust = 0.5, size = 10, face = "bold.italic", color = "black")  # Align left, italicized, and readable
    )# Apply clean theme
  
  return(goal_ranges)
  
  
}




