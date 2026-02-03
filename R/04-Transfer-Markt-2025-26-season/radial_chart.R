clubs <- c(
  "Arsenal","Aston Villa","Bournemouth","Brentford","Brighton",
  "Chelsea","Crystal Palace","Everton","Fulham","Ipswich",
  "Leicester","Liverpool","Man City","Man United","Newcastle",
  "Nottingham Forest","Southampton","Tottenham","West Ham","Wolves"
)

set.seed(123)

radial_df <- tibble(
  club = rep(clubs, each = 2),
  direction = rep(c("In","Out"), times = 20),
  value = round(runif(40, 10, 120), 1)  # €m
)
radial_df <- radial_df %>%
  group_by(club) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  mutate(
    club = fct_reorder(club, total)
  )
ggplot(radial_df, aes(
  x = club,
  y = value,
  fill = direction
)) +
  geom_col(width = 1, color = "white", size = 0.2) +
  coord_polar(start = 0) +
  scale_fill_manual(
    values = c("In" = "#1b9e77", "Out" = "#d95f02")
  ) +
  labs(
    title = "Premier League Winter Transfer Activity (2025/26)",
    subtitle = "Stacked radial bars show transfer spend in vs out",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 8),
    legend.position = "bottom"
  )








set.seed(123)

radial_df <- tibble(
  club = rep(clubs, each = 6),
  direction = rep(c("In","Out"), times = 60),
  position = sample(c("DEF","MID","FWD"), 120, replace = TRUE,
                    prob = c(0.4, 0.35, 0.25)),
  value = round(runif(120, 5, 60), 1)
)
value_df <- radial_df %>%
  group_by(club, direction) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  group_by(club) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  mutate(club = fct_reorder(club, total))

pos_df <- radial_df %>%
  filter(direction == "In") %>%
  count(club, position) %>%
  mutate(
    club = factor(club, levels = levels(value_df$club)),
    y = n * 6   # scale so it stays inside
  )
ggplot() +
  
  ## ---- MAIN RADIAL BARS (VALUE) ----
geom_col(
  data = value_df,
  aes(x = club, y = value, fill = direction),
  width = 1,
  color = "white",
  size = 0.25
) +
  
  ## ---- INNER RING (PLAYER COUNT BY POSITION) ----
geom_col(
  data = pos_df,
  aes(x = club, y = y, fill = position),
  width = 1,
  alpha = 0.85
) +
  
  coord_polar(start = 0) +
  
  scale_fill_manual(
    values = c(
      "In"  = "#1b9e77",
      "Out" = "#d95f02",
      "DEF" = "#4c72b0",
      "MID" = "#dd8452",
      "FWD" = "#c44e52"
    )
  ) +
  
  labs(
    title = "Premier League Winter Window 2025/26",
    subtitle = "Outer ring: transfer value (In vs Out) • Inner ring: players bought by position",
    fill = NULL
  ) +
  
  theme_void(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )






library(tidyverse)
library(forcats)

# ---- DATA ----
clubs <- c(
  "Arsenal","Aston Villa","Bournemouth","Brentford","Brighton",
  "Chelsea","Crystal Palace","Everton","Fulham","Ipswich",
  "Leicester","Liverpool","Man City","Man United","Newcastle",
  "Nottingham Forest","Southampton","Tottenham","West Ham","Wolves"
)

set.seed(123)

radial_df <- tibble(
  club = rep(clubs, each = 6),
  direction = rep(c("In","Out"), times = 60),
  position = sample(c("DEF","MID","FWD"), 120, replace = TRUE,
                    prob = c(0.4, 0.35, 0.25)),
  value = round(runif(120, 5, 60), 1)
)

# ---- OUTER RING (VALUE) ----
value_df <- radial_df %>%
  group_by(club, direction) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  group_by(club) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  mutate(club = fct_reorder(club, total))

# ---- INNER RING (POSITION COUNTS) ----
pos_df <- radial_df %>%
  filter(direction == "In") %>%
  count(club, position) %>%
  mutate(
    club = factor(club, levels = levels(value_df$club)),
    y = n * 4   # ⬅ thinner inner ring
  )

# ---- LABEL DATA ----
label_df <- value_df %>%
  distinct(club, total) %>%
  mutate(label_y = total * 1.08)  # ⬅ push outside

# ---- PLOT ----
ggplot() +
  
  # OUTER RING
  geom_col(
    data = value_df,
    aes(x = club, y = value, fill = direction),
    width = 0.75,           # ⬅ thinner
    color = "white",
    size = 0.2
  ) +
  
  # INNER RING
  geom_col(
    data = pos_df,
    aes(x = club, y = y, fill = position),
    width = 0.75,           # ⬅ thinner
    alpha = 0.9
  ) +
  
  # CLUB LABELS
  geom_text(
    data = label_df,
    aes(x = club, y = label_y, label = club),
    size = 2.6,
    fontface = "bold"
  ) +
  
  coord_polar(start = 0) +
  
  scale_fill_manual(
    values = c(
      "In"  = "#1b9e77",
      "Out" = "#d95f02",
      "DEF" = "#4c72b0",
      "MID" = "#dd8452",
      "FWD" = "#c44e52"
    )
  ) +
  
  labs(
    title = "Premier League Winter Window 2025/26",
    subtitle = "Outer ring: transfer value (In vs Out) • Inner ring: players bought by position",
    fill = NULL
  ) +
  
  theme_void(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    plot.margin = margin(20, 20, 20, 20)
  )







