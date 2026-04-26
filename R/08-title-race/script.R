library(dplyr)
library(ggplot2)

set.seed(1)

df <- expand.grid(
  team = c("Arsenal", "Man City"),
  season = c("2022/23","2023/24","2024/25"),
  match = 1:20
) %>%
  group_by(team, season) %>%
  mutate(
    prob = case_when(
      team == "Arsenal" & match < 12 ~ 0.7,
      team == "Arsenal" & match >= 12 ~ 0.45,
      team == "Man City" & match < 12 ~ 0.55,
      TRUE ~ 0.8
    ),
    win = rbinom(n(), 1, prob),
    points = win * 3
  ) %>%
  ungroup()


df_arc <- df %>%
  group_by(team, season) %>%
  arrange(match) %>%
  mutate(
    id = row_number(),
    total = max(id),
    
    # angle per segment
    start = 2*pi*(id-1)/total,
    end   = 2*pi*(id)/total,
    
    # radius (form proxy)
    value = zoo::rollmean(win, 5, fill = NA, align = "right"),
    value = ifelse(is.na(value), mean(win), value),
    
    r0 = 0.6,
    r1 = 0.6 + value*0.35   # thickness based on form
  ) %>%
  ungroup()

arc_df <- df_arc %>%
  rowwise() %>%
  mutate(poly_id = paste(team, season, id, sep = "_")) %>%
  do({
    data.frame(
      team = .$team,
      season = .$season,
      value = .$value,
      poly_id = .$poly_id,
      x = c(
        .$r0 * cos(seq(.$start, .$end, length.out = 20)),
        rev(.$r1 * cos(seq(.$start, .$end, length.out = 20)))
      ),
      y = c(
        .$r0 * sin(seq(.$start, .$end, length.out = 20)),
        rev(.$r1 * sin(seq(.$start, .$end, length.out = 20)))
      )
    )
  }) %>%
  ungroup()
ggplot(arc_df, aes(x, y, group = poly_id)) +
  
  geom_polygon(aes(fill = value), color = NA) +
  
  coord_equal() +
  
  facet_grid(season ~ team) +
  
  scale_fill_gradientn(
    colors = c("#d73027","#f46d43","#fdae61","#a6d96a","#1a9850")
  ) +
  
  theme_void()+
  
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none",
    panel.background = element_rect(fill = "#0e1117", color = NA),
    plot.background = element_rect(fill = "#0e1117", color = NA)
  )
centers <- df %>%
  group_by(team, season) %>%
  summarise(points = sum(points), .groups = "drop")

+ geom_text(
  data = centers,
  aes(x = 0, y = 0, label = points),
  color = "white",
  size = 5,
  fontface = "bold",
  inherit.aes = FALSE
)