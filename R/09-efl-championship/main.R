
# Load required libraries for analysis ------------------------------------
library(tidyverse)
library(engsoccerdata)



# Load engsoccerdata from devtools ----------------------------------------

eng_cship<-engsoccerdata::england_current()
eng_cship<-eng_cship %>% dplyr::filter(division==2)

# get league table for the season

league_table<-engsoccerdata::maketable_eng(eng_cship,2025,2,2)




# Data Processing ---------------------------------------------------------

# filter out only the top 6

top_6<-head(league_table,6)
teams <- unique(top_6$team)
eng_cship_h <- eng_cship %>% filter(home %in% teams)
eng_cship_v <- eng_cship %>% filter(visitor %in% teams)
eng_cship<-bind_rows(eng_cship_h,eng_cship_v)
eng_cship<-eng_cship %>% distinct()
library(dplyr)

team_match <- bind_rows(
  
  # Home team
  eng_cship %>%
    mutate(
      team = home,
      opponent = visitor,
      
      ht_state = case_when(
        ht_hgoal > ht_vgoal ~ "Leading",
        ht_hgoal == ht_vgoal ~ "Drawing",
        TRUE ~ "Trailing"
      ),
      
      ft_state = case_when(
        hgoal > vgoal ~ "Win",
        hgoal == vgoal ~ "Draw",
        TRUE ~ "Loss"
      ),
      
      pts = case_when(
        ft_state == "Win" ~ 3,
        ft_state == "Draw" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(team, opponent, ht_state, ft_state, pts),
  
  # Away team
  eng_cship %>%
    mutate(
      team = visitor,
      opponent = home,
      
      ht_state = case_when(
        ht_vgoal > ht_hgoal ~ "Leading",
        ht_vgoal == ht_hgoal ~ "Drawing",
        TRUE ~ "Trailing"
      ),
      
      ft_state = case_when(
        vgoal > hgoal ~ "Win",
        vgoal == hgoal ~ "Draw",
        TRUE ~ "Loss"
      ),
      
      pts = case_when(
        ft_state == "Win" ~ 3,
        ft_state == "Draw" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(team, opponent, ht_state, ft_state, pts)
)



ht_summary <-
  team_match %>%
  group_by(team, ht_state) %>%
  summarise(
    games = n(),
    pts_won = sum(pts),
    max_pts = games * 3,
    pct_pts = round(100 * pts_won / max_pts, 1),
    .groups = "drop"
  ) %>% filter(team %in% teams)


ft_summary <-
  team_match %>%
  group_by(team, ft_state) %>%
  summarise(
    games = n(),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = ft_state,
    values_from = games,
    values_fill = 0
  )%>% filter(team %in% teams)





