

library(ggplot2)
library(ggforce)
library(dplyr)


df <- tibble(
  country = c("Sweden", "Denmark", "Norway"),
  r_inner = c(0.75, 0.55, 0.35),
  r_outer = c(0.90, 0.70, 0.50),
  before  = c(13, 4, 5),
  after   = c(15, 10, 8),
  max_val = 15
) %>%
  pivot_longer(
    cols = c(before, after),
    names_to = "period",
    values_to = "value"
  ) %>%
  mutate(
    start = pi / 2,
    end   = start + (value / max_val) * 2 * pi
  )
ggplot(df) +
  geom_arc_bar(
    aes(
      x0 = 0, y0 = 0,
      r0 = r_inner,
      r  = r_outer,
      start = start,
      end   = end,
      fill  = period
    ),
    color = NA,
    radius = unit(8, "pt")   # rounded ends
  ) +
  coord_fixed() +
  scale_fill_manual(
    values = c(
      before = "#1F2A44",   # dark
      after  = "#F4A6A0"    # light
    ),
    labels = c("Before 2004", "After 2004")
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )



df_labels <- df %>%
  mutate(
    angle = (start + end) / 2,
    r_mid = (r_inner + r_outer) / 2,
    x = r_mid * cos(angle),
    y = r_mid * sin(angle)
  )

ggplot(df) +
  geom_arc_bar(
    aes(
      x0 = 0, y0 = 0,
      r0 = r_inner,
      r  = r_outer,
      start = start,
      end   = end,
      fill  = period
    ),
    radius = unit(8, "pt")
  ) +
  geom_text(
    data = df_labels,
    aes(x, y, label = value),
    color = "white",
    size = 4,
    fontface = "bold"
  ) +
  coord_fixed() +
  scale_fill_manual(
    values = c(
      before = "#1F2A44",
      after  = "#F4A6A0"
    )
  ) +
  theme_void() +
  theme(legend.position = "bottom")








library(ggplot2)
library(ggforce)
library(dplyr)

# Example data for 20 clubs (fill with your exact numbers)
teams <- tibble(
  club = c("Arsenal","Liverpool","Man Utd","Tottenham","Sunderland",
           "Everton","Leeds","Forest","Newcastle","West Ham"),
  spend = c(257,218,171,154,141,114,103,101,99,76)
)

# Normalize angles
max_spend <- max(teams$spend)
teams <- teams %>%
  mutate(
    start = pi/2,
    end   = start + (spend/max_spend)*2*pi,
    r0    = seq(0.2,1.0,length.out = n()),
    r     = r0 + 0.08
  )

ggplot(teams) +
  geom_arc_bar(aes(
    x0 = 0, y0 = 0,
    r0 = r0, r = r,
    start = start, end = end,
    fill = spend
  ), color = NA) +
  coord_fixed() +
  scale_fill_viridis_c(option="C") +
  theme_void() +
  geom_text(aes(
    x = (r0+r)/2 * cos((start+end)/2),
    y = (r0+r)/2 * sin((start+end)/2),
    label = club
  ), size=3) +
  ggtitle("2025 Premier League Transfer Spend by Club")







library(tidyverse)

df <- tibble(
  team = c(
    "Man City","Arsenal","Chelsea","Liverpool","Man United",
    "Tottenham","Newcastle","Aston Villa","West Ham","Brighton",
    "Crystal Palace","Fulham","Wolves","Everton","Brentford",
    "Forest","Bournemouth","Burnley","Sheffield Utd","Luton"
  ),
  spend = c(
    95, 89, 75, 70, 65,
    62, 58, 55, 50, 48,
    45, 42, 40, 38, 36,
    34, 30, 28, 22, 18
  )
) %>%
  arrange(desc(spend)) %>%
  mutate(team = factor(team, levels = team))


library(tidyverse)

df <- tibble(
  team = c(
    "Man City","Arsenal","Chelsea","Liverpool","Man United",
    "Tottenham","Newcastle","Aston Villa","West Ham","Brighton",
    "Crystal Palace","Fulham","Wolves","Everton","Brentford",
    "Forest","Bournemouth","Burnley","Sheffield Utd","Luton"
  ),
  spend = c(
    95, 89, 75, 70, 65,
    62, 58, 55, 50, 48,
    45, 42, 40, 38, 36,
    34, 30, 28, 22, 18
  )
) %>%
  arrange(desc(spend)) %>%
  mutate(team = factor(team, levels = team))


ggplot(df, aes(x = team, y = spend)) +
  geom_col(
    width = 0.95,
    fill = "#2C6BED"
  ) +
  coord_polar(start = pi / 2) +
  ylim(0, 110) +
  theme_void() +
  theme(
    axis.text.x = element_text(
      size = 11,
      face = "bold",
      color = "grey20"
    )
  )


