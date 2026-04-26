library(tidyverse)

set.seed(42)

seasons <- c("2022/23", "2023/24", "2024/25", "2025/26")
teams <- c("Arsenal", "Man City")

# Generate match-level data
df <- expand_grid(
  season = seasons,
  team = teams,
  matchday = 1:38
) %>%
  mutate(
    result = sample(c("W", "D", "L"), n(), replace = TRUE, prob = c(0.6, 0.2, 0.2)),
    goals_for = case_when(
      result == "W" ~ sample(1:4, n(), replace = TRUE),
      result == "D" ~ sample(0:2, n(), replace = TRUE),
      TRUE ~ sample(0:2, n(), replace = TRUE)
    ),
    goals_against = case_when(
      result == "W" ~ sample(0:1, n(), replace = TRUE),
      result == "D" ~ goals_for,
      TRUE ~ sample(1:3, n(), replace = TRUE)
    )
  )


summary_df <- df %>%
  group_by(season, team) %>%
  summarise(
    GF = sum(goals_for),
    GA = sum(goals_against),
    .groups = "drop"
  )

dots_per_row <- 5   # 👈 you can change this (6, 8, etc.)

df_plot <- df %>%
  group_by(season, team) %>%
  arrange(matchday) %>%
  mutate(
    id = row_number(),
    col = ((id - 1) %% dots_per_row) + 1,
    row = ((id - 1) %/% dots_per_row) + 1
  ) %>%
  ungroup()

ggplot(df_plot, aes(x = col, y = -row)) +
  
  geom_point(aes(color = result), size = 2.8) +
  
  facet_grid(team ~ season) +
  
  scale_color_manual(
    values = c(
      "W" = "#2ecc71",
      "D" = "#f1c40f",
      "L" = "#e74c3c"
    )
  ) +
  geom_text(
    data = summary_df,
    aes(x = 3, y = 1.5, label = paste0(GF, "-", GA)),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 4
  )+
  labs(
    title = "Title Race Grid View",
    subtitle = "Each dot = 1 match (38 per season)",
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal() +
  
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(face = "bold")
  )