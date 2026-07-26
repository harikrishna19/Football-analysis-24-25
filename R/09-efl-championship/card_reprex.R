library(ggplot2)
library(ggimage)
library(patchwork)

#----------------------------------
# Header
#----------------------------------

header <- ggplot() +
  
  annotate(
    "rect",
    xmin = 0, xmax = 12, ymin = 0, ymax = 6,
    fill = "#0F172A", colour = NA
  ) +
  
  annotate(
    "text", x = 0.6, y = 5.35,
    label = "EFL CHAMPIONSHIP 2025/26",
    hjust = 0, colour = "white", fontface = "bold", size = 9
  ) +
  
  annotate(
    "text", x = 0.6, y = 4.75,
    label = "Season Summary",
    hjust = 0, colour = "#93C5FD", fontface = "bold", size = 5.5
  ) +
  
  annotate(
    "segment", x = 0.6, xend = 4.6, y = 4.45, yend = 4.45,
    linewidth = 1.3, colour = "#3B82F6"
  ) +
  
  annotate(
    "text", x = 0.8, y = 3.75,
    label = "\u2022 24 clubs competed across 46 league matches.",
    hjust = 0, colour = "white", size = 4.3
  ) +
  
  annotate(
    "text", x = 0.8, y = 3.05,
    label = "\u2022 Automatic promotion and play-off places were decided after an intense campaign.",
    hjust = 0, colour = "white", size = 4.3
  ) +
  
  annotate(
    "text", x = 0.8, y = 2.35,
    label = "\u2022 Goals, defensive resilience and second-half performances shaped the final standings.",
    hjust = 0, colour = "white", size = 4.3
  ) +
  
  annotate(
    "text", x = 0.8, y = 1.65,
    label = "\u2022 Explore key metrics for every team including points, goals and comeback performances.",
    hjust = 0, colour = "white", size = 4.3
  ) +
  
  annotate(
    "text", x = 0.6, y = 0.55,
    label = "Data Source: SoccerData | Visualisation: ggplot2",
    hjust = 0, colour = "#94A3B8", size = 3.5
  ) +
  
  # KEY FIX: expand = FALSE removes the default ~5% padding
  # that was showing up as a white border around the navy rect
  coord_cartesian(xlim = c(0, 12), ylim = c(0, 6), expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#0F172A", colour = NA),
    plot.margin = margin(0, 0, 0, 0)
  )

#----------------------------------
# Sample team data (swap in your real 6 teams here)
#----------------------------------

team <- data.frame(
  Team = "Leeds United",
  Pts = 100,
  GF = 84,
  GA = 30,
  HT_Pts = 45,
  FT_Pts = 100,
  SH_Pts = 55,
  Comebacks = 8,
  LeadsLost = 3
)

logo <- data.frame(
  x = 5,
  y = 12.8,
  image = "https://upload.wikimedia.org/wikipedia/en/5/54/Leeds_United_F.C._logo.svg"
)

#----------------------------------
# Card (single team)
#----------------------------------

make_card <- function(team, logo) {
  ggplot() +
    
    annotate(
      "rect",
      xmin = 0, xmax = 10, ymin = 0, ymax = 14,
      fill = "#F8FAFC", colour = "#D9E2EC", linewidth = 0.8
    ) +
    
    annotate(
      "text", x = 5, y = 13.6,
      label = team$Team, fontface = "bold", size = 7
    ) +
    
    annotate(
      "text", x = 5, y = 11.3,
      label = paste0(team$Pts, " PTS"),
      fontface = "bold", size = 10, colour = "#0F172A"
    ) +
    
    annotate(
      "text", x = 5, y = 10.6,
      label = "Championship Winners", colour = "grey40", size = 4
    ) +
    
    annotate(
      "segment", x = 1, xend = 9, y = 9.8, yend = 9.8,
      colour = "#CBD5E1"
    ) +
    
    ## Goals
    annotate("text", x = 2, y = 8.9, label = "Goals", hjust = 0, fontface = "bold", size = 4.5) +
    annotate("text", x = 8, y = 8.9, label = paste(team$GF, ":", team$GA), hjust = 1, size = 4.5) +
    
    ## HT Points
    annotate("text", x = 2, y = 7.7, label = "HT Points", hjust = 0, fontface = "bold", size = 4.5) +
    annotate("text", x = 8, y = 7.7, label = team$HT_Pts, hjust = 1, size = 4.5) +
    
    ## FT Points
    annotate("text", x = 2, y = 6.5, label = "FT Points", hjust = 0, fontface = "bold", size = 4.5) +
    annotate("text", x = 8, y = 6.5, label = team$FT_Pts, hjust = 1, size = 4.5) +
    
    ## 2nd Half Points
    annotate("text", x = 2, y = 5.3, label = "2nd Half Points", hjust = 0, fontface = "bold", size = 4.5) +
    annotate("text", x = 8, y = 5.3, label = team$SH_Pts, hjust = 1, size = 4.5) +
    
    ## Comebacks
    annotate("text", x = 2, y = 4.1, label = "Comeback Wins", hjust = 0, fontface = "bold", size = 4.5, colour = "#15803D") +
    annotate("text", x = 8, y = 4.1, label = team$Comebacks, hjust = 1, size = 4.5, colour = "#15803D") +
    
    ## Leads Lost
    annotate("text", x = 2, y = 2.9, label = "Leads Lost", hjust = 0, fontface = "bold", size = 4.5, colour = "#DC2626") +
    annotate("text", x = 8, y = 2.9, label = team$LeadsLost, hjust = 1, size = 4.5, colour = "#DC2626") +
    
    geom_image(data = logo, aes(x, y, image = image), size = 0.10) +
    
    # KEY FIX: coord_cartesian(expand = FALSE) instead of coord_fixed().
    # coord_fixed() forces its own aspect ratio and ignores the grid cell
    # patchwork gives it, which was the other source of white gaps.
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 14), expand = FALSE, clip = "off") +
    theme_void() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

card <- make_card(team, logo)

#----------------------------------
# 6 cards, 2 rows x 3 cols
#----------------------------------

aa <- replicate(6, card, simplify = FALSE)

cards_grid <- wrap_plots(aa, ncol = 3, byrow = TRUE) +
  plot_layout(guides = "collect")

#----------------------------------
# Full dashboard
#----------------------------------

final_plot <- (header / cards_grid) +
  plot_layout(heights = c(0.45, 2)) &
  theme(
    plot.margin = margin(1, 1, 1, 1) # tiny margin only to stop border clipping
  )

final_plot

ggsave(
  "dashboard.png",
  final_plot,
  width = 18,
  height = 10,
  dpi = 300,
  bg = "white"
)