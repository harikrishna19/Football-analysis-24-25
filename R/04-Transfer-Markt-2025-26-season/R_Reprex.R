library(tidyverse)
library(ggforce)

df <- tibble(
  country = c("USA", "China", "Japan", "UK", "ROK", "Australia", "Italy", "Germany"),
  value   = c(139, 138, 127, 122, 120, 117, 100, 90)
) %>%
  arrange(desc(value)) %>%
  mutate(
    id   = row_number(),
    frac = value / max(value)
  )

ggplot(df) +
  
  # value arcs ONLY
  geom_arc_bar(
    aes(
      x0 = 0, y0 = 0,
      r0 = id - 0.45,
      r  = id + 0.45,
      start = 0,
      end   = 2*pi*frac,
      fill  = country
    ),
    color = NA
  ) +
  
  # labels
  geom_text(
    aes(
      x = 0,
      y = id,
      label = paste0(country, " (", value, ")")
    ),
    hjust = -0.1,
    size = 4
  ) +
  
  # center total
  annotate(
    "text",
    x = 0, y = 0,
    label = paste0("Total\n(", sum(df$value), ")"),
    size = 5,
    fontface = "bold"
  ) +
  
  coord_fixed() +
  scale_fill_brewer(palette = "Set1") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 40, 20, 20)
  )




# Football reprex ---------------------------------------------------------


library(tidyverse)
library(ggforce)

# sample transfer spend data
df <- tibble(
  club  = c("Man City","Crystal Palace","Tottenham","West Ham"),
  spend = c(95, 89.7, 55, 54.3)
) %>%
  arrange(desc(spend)) %>%
  mutate(
    id   = row_number(),
    frac = spend / max(spend)
  )

# club colours (approx)
club_cols <- c(
  "Man City"       = "#6CABDD",
  "Crystal Palace" = "yellow",
  "Tottenham"      = "blue",
  "West Ham"       = "pink"
)

ggplot(df) +
  
  # value arcs only
  geom_arc_bar(
    aes(
      x0 = 0, y0 = 0,
      r0 = id - 0.4,
      r  = id + 0.4,
      start = 0,
      end   = 2*pi * frac,
      fill  = club
    ),
    color = NA
  ) +
  
  # club labels
  geom_text(
    aes(
      x = 0, y = id,
      label = paste0(club, " (", spend, "M €)")
    ),
    hjust = -0.1,
    size = 4
  ) +
  
  # center total
  annotate(
    "text",
    x = 0, y = 0,
    label = paste0("Total\n(", sum(df$spend), "M €)"),
    size = 5,
    fontface = "bold"
  ) +
  
  coord_fixed() +
  scale_fill_manual(values = club_cols) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 40, 20, 20)
  )









