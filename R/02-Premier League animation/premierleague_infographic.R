#Inforgaphic to show premier league

# Data Pull 

# showing insights

# Use grammar of graphics package extensions

# June 1st TBD Infographic release


# Load required packages --------------------------------------------------
library(gganimate)
library(ggplot2)
library(worldfootballR)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggimage)
library(rvest)



# Extract results data for 2025 season ------------------------------------

pl_2025<-worldfootballR::fb_match_results("ENG",gender = "M",season_end_year = 2025,tier = "1st")


# View the data -----------------------------------------------------------
View(pl_2025)

# 1. Create long format for easier processing
matches_long <- pl_2025 %>%
  mutate(
    HomePoints = case_when(
      HomeGoals > AwayGoals ~ 3,
      HomeGoals == AwayGoals ~ 1,
      TRUE ~ 0
    ),
    AwayPoints = case_when(
      AwayGoals > HomeGoals ~ 3,
      AwayGoals == HomeGoals ~ 1,
      TRUE ~ 0
    )
  ) %>%
  select(Wk, Home, Away, HomeGoals, AwayGoals, HomePoints, AwayPoints) %>%
  pivot_longer(cols = c(Home, Away),
               names_to = "HomeAway", values_to = "Team") %>%
  mutate(
    GoalsFor = ifelse(HomeAway == "Home", HomeGoals, AwayGoals),
    GoalsAgainst = ifelse(HomeAway == "Home", AwayGoals, HomeGoals),
    Points = ifelse(HomeAway == "Home", HomePoints, AwayPoints),
    Won = ifelse(Points == 3, 1, 0),
    Played=1,
    Drawn = ifelse(Points == 1, 1, 0),
    Lost = ifelse(Points == 0, 1, 0)
  ) %>%
  select(Wk, Team,Played, Won, Drawn, Lost, GoalsFor, GoalsAgainst, Points)

# 2. Cumulative stats after each matchweek
league_table <- matches_long %>%
  group_by(Team, Wk) %>%
  summarise(
    Played = sum(Played),
    Won = sum(Won),
    Drawn = sum(Drawn),
    Lost = sum(Lost),
    GF = sum(GoalsFor),
    GA = sum(GoalsAgainst),
    Points = sum(Points),
    .groups = "drop"
  ) %>%
  arrange(as.numeric(Wk)) %>%
  group_by(Team) %>%
  mutate(
    cPlayed = cumsum(Played),
    cWon = cumsum(Won),
    cDrawn = cumsum(Drawn),
    cLost = cumsum(Lost),
    cGF = cumsum(GF),
    cGA = cumsum(GA),
    cPoints = cumsum(Points),
    cGD = cGF - cGA
  ) %>%
  ungroup()

# 3. Create league table per week with ranking
league_positions <- league_table %>%
  group_by(Wk) %>%
  arrange(Wk, desc(cPoints), desc(cGD), desc(cGF),desc(cGA), Team) %>%
  mutate(Position = row_number()) %>%
  select(Wk, Team, Position, cPlayed, cWon, cDrawn, cLost, cGF, cGA, cGD, cPoints) %>%
  ungroup()



# Plot 1 Viz --------------------------------------------------------------


# To show of the bottom 3 with Spurs and Manchester United
# relegated 3 and including spurs and Manchester united

plot_5<-league_positions %>% 
  filter(Team %in% c("Tottenham", "Southampton", "Ipswich Town", "Manchester Utd","Wolves","Leicester City")) %>%
  mutate(Wk = as.numeric(Wk)) %>%
  ggplot(aes(x = Wk, y = Position, group = Team, color = Team)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:38) +
  scale_color_manual(values=c("blue", "lightblue", "red","pink", "grey", "yellow"))+
  scale_y_reverse(breaks = 1:max(league_positions$Position)) +
  labs(
    title = "Team Positions by Matchweek-Bottom 6 teams",
    subtitle = "This year bottom 6 includes Tottenham and Manchester United",
    x = "Matchweek", y = "Position"
  ) +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  transition_reveal(Wk)

animate(
  fps = 10,
  duration = 10,
  width = 800,   # Increase width here
  height = 600,
  plot_5 + enter_fade() + exit_fly(y_loc = 1),
  renderer = av_renderer()
)



# Plot 2 Viz --------------------------------------------------------------

# We'll reverse the y-axis since position 1 should be at the top
all_teams<-# Assign each team a vertical offset
  team_offsets <- league_positions %>%
  distinct(Team) %>%
  mutate(offset = (row_number() - 1) * 25)  # Adjust spacing as needed

# Join with main data
plot_data <- league_positions %>%
  mutate(Wk = as.numeric(Wk)) %>%
  left_join(team_offsets, by = "Team") %>%
  mutate(Position_offset = offset - Position)  # Invert so 1st is still top

all_teams<-ggplot(plot_data, aes(x = Wk, y = Position_offset, group = Team, color = Team)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Team Positions Over Time (Separated by Team)",
    x = "Matchweek", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  transition_states(Wk)


animate(
  fps = 10,
  duration = 10,
  width = 1200,   # Increase width here
  height = 600,
  all_teams + enter_fade() + exit_fly(y_loc = 1),
  renderer = av_renderer()
)




