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
library(magick)
library(ggtext)
library(av)


# Extract results data for 2025 season ------------------------------------

pl_2025<-worldfootballR::fb_match_results("ENG",gender = "M",season_end_year = 2025,tier = "1st")



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


source("./R/02-Premier League animation/team_details.R")
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_5
# Plot 1 to be roughly finalised either plot or table 


# Plot 2 Viz --------------------------------------------------------------
league_positions <- league_positions %>%
  mutate(
    Wk = as.numeric(Wk),
    PositionLabel = as.character(round(Position))  # or as.character(as.integer(Position))
  )

# # We'll reverse the y-axis since position 1 should be at the top

league_positions$Team<-factor(league_positions$Team, levels = hex_codes$Team)
all_teams<-
  ggplot(league_positions, aes(x = Wk, y = Position,color=Team,fill = Team) ) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_text(aes(label = PositionLabel), vjust = -0.8, size = 5)+
  labs(
    title = "Team Positions Over Time",
    subtitle = 'Matchweek: {as.integer(frame_along)}',  # ✅ no decimal in subtitle
    x = "Matchweek", y = NULL
  ) +
  scale_x_continuous(breaks = 1:38) +
  scale_colour_manual(values = setNames(league_positions$Codes, league_positions$Team)) +
  scale_y_reverse(breaks = 1:max(league_positions$Position)) +  # Top position at top
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  )+transition_reveal(along = Wk,keep_last = T)




# Ensure data is ordered by matchweek
league_positions_anim <- league_positions %>%
  mutate(
    fill_color = ifelse(Position %in% c(1,2,3,4,5,17), "CL",
                ifelse(Position %in% c(6,12),"EL",
                ifelse(Position==7,"Conf",
                ifelse(Wk==38 &cPoints<=25,"relegation","normal")))),
    WkLabel = paste("Matchweek", Wk)
  ) %>%
  ungroup()
league_positions_anim$Team <- factor(league_positions$Team, levels = hex_codes$Team)
# Create the animated tile plot
p <- ggplot(league_positions_anim , aes(x = "cPoints", y = Team),fill=Team) +
  geom_tile(aes(fill = fill_color), width = 1, height = 0.9) +
  geom_text(aes(label = round(cPoints, 0)), color = "black", size = 6) +
  scale_fill_manual(values = c("CL" = "blue","EL"="orange","Conf"="green","relegation" = "red","normal"="white")) +
  scale_y_discrete(
    name = NULL,
    limits = rev(levels(factor(league_positions_anim$Team))),
    labels = paste0(
      "<img src='", rev(unique(hex_codes$team_imgs)), "' width='20' />"
    )
  )   +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(color = "black", size = 11),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Cumulative Points by Matchweek", 
       subtitle = "{closest_state}") +
  transition_states(Wk, transition_length = 2, state_length = 1, wrap = FALSE)+enter_fade() + exit_fade()

# Animate both using magick_renderer
plot_1 <- animate(all_teams, fps = 5, width = 800, height = 600, nframes = 38, renderer = magick_renderer())
plot_2 <- animate(p, fps = 5, width = 800, height = 600, nframes = 38, renderer = magick_renderer())



# Combine frames side-by-side
combined <- image_append(c(plot_1[1], plot_2[1]))
for (i in 2:38) {
  combined <- c(combined, image_append(c(plot_1[i], plot_2[i])))
}

image_write(combined,"combined.gif")

