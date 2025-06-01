library(gganimate)
library(worldfootballR)
library(dplyr)
library(tidyr)

install.packages("worldfootballR")




pl_2025<-worldfootballR::fb_match_results("ENG",gender = "M",season_end_year = 2025,tier = "1st")

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
    GoalsFor = ifelse(HomeAway == "HomeTeam", HomeGoals, AwayGoals),
    GoalsAgainst = ifelse(HomeAway == "HomeTeam", AwayGoals, HomeGoals),
    Points = ifelse(HomeAway == "HomeTeam", HomePoints, AwayPoints),
    Played = 1,
    Won = ifelse(Points == 3, 1, 0),
    Drawn = ifelse(Points == 1, 1, 0),
    Lost = ifelse(Points == 0, 1, 0)
  ) %>%
  select(Wk, Team, Played, Won, Drawn, Lost, GoalsFor, GoalsAgainst, Points)

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
  arrange(Wk, desc(cPoints), desc(cGD), desc(cGF), Team) %>%
  mutate(Position = row_number()) %>%
  select(Wk, Team, Position, cPlayed, cWon, cDrawn, cLost, cGF, cGA, cGD, cPoints) %>%
  ungroup()


library(gganimate)
library(tidyverse)

p<-league_positions %>% 
  filter(Team %in% c("Tottenham", "Southampton", "Ipswich Town", "Manchester Utd")) %>%
  mutate(Wk = as.numeric(Wk)) %>%
  ggplot(aes(x = Wk, y = Position, group = Team, color = Team)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:38) +
  scale_y_reverse(breaks = 1:max(league_positions$Position)) +
  labs(
    title = "Team Positions by Matchweek",
    x = "Matchweek", y = "Position"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  transition_reveal(Wk)

animate(
  fps = 10,
  duration = 10,
  width = 1200,   # Increase width here
  height = 600,
  p + enter_fade() + exit_fly(y_loc = 1),
  renderer = av_renderer()
)

animate(
  p,
  fps = 15,                   # Higher frames per second
  duration = 10,              # Longer duration = more frames
  width = 1280,               # Larger width
  height = 720,               # Larger height
  res = 150,
  renderer = av_renderer()# Higher resolution (optional)
)



# We'll reverse the y-axis since position 1 should be at the top
p <- league_positions %>%
  filter(Wk <= max(Wk)) %>%   # filter out incomplete weeks if needed
  ggplot(aes(x = -Position, y = cPoints, fill = Team)) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(
    title = "Premier League Standings: Matchweek {closest_state}",
    x = "Position",
    y = "Points"
  ) +
  scale_x_continuous(
    breaks = -rev(1:20),
    labels = rev(1:20)
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  transition_states(Wk, transition_length = 2, state_length = 1, wrap = FALSE) +
  ease_aes('cubic-in-out')

# Video output
animate(
  p + enter_fade() + exit_fly(y_loc = 1),
  renderer = av_renderer()
)




























