library(ggplot2)
library(dplyr)

set.seed(123)
players <- paste("Player", LETTERS[1:20])  # 20 players
games <- 1:38   # full season

# Dummy availability data
availability <- expand.grid(Game = games, Player = players) %>%
  mutate(Status = sample(c("Start","Bench","Injured"), 
                         size = n(), replace = TRUE, 
                         prob = c(0.6,0.25,0.15)))

# Assign player positions (for demo: first 8 defenders, next 6 mids, last 6 forwards)
availability <- availability %>%
  mutate(Position = case_when(
    Player %in% players[1:8]  ~ "Defender",
    Player %in% players[9:14] ~ "Midfielder",
    TRUE ~ "Forward"
  ))

# Facetted heatmap
ggplot(availability, aes(x = Game, y = reorder(Player, desc(Player)), fill = Status)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("Start"="#2ecc71", "Bench"="#f1c40f", "Injured"="#e74c3c")) +
  facet_wrap(~ Position, scales = "free_y", ncol = 1) +
  labs(title = "Player Availability Across Season by Position",
       subtitle = "Green=Started, Yellow=Bench, Red=Injured",
       x = "Game", y = "Player") +
  theme_minimal() +
  theme(axis.text.y = element_text(size=7))
