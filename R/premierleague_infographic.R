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


anim <- ggplot(mtcars, aes(mpg, disp)) +
  geom_point(aes(color = gear)) +
  transition_states(gear, transition_length = 2, state_length = 1) +
  enter_fade() +
  exit_fade()
animate(anim)
