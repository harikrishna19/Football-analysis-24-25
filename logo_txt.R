logo_panel <- ggplot() +
  
  # =====================================================
# PREMIER LEAGUE LOGO
# =====================================================

annotation_custom(
  
  rasterGrob(
    pl_logo,
    interpolate = TRUE
  ),
  
  xmin = 0.2,
  xmax = 3.8,
  
  ymin = 7.8,
  ymax = 10.8
) +
  
  # =====================================================
# TITLE
# =====================================================

annotate(
  "text",
  
  x = 0,
  y = 6.4,
  
  label =
    "PREMIER LEAGUE TITLE RACE TRENDS",
  
  family = "anton",
  
  hjust = 0,
  
  size = 10.5,
  
  colour = "#1A1A1A",
  
  lineheight = .92
) +
  
  # =====================================================
# SUBTITLE
# =====================================================

annotate(
  "text",
  
  x = 0,
  y = 5.15,
  
  label =
    "Each square represents\none league match.",
  
  family = "roboto",
  
  hjust = 0,
  
  size = 4.6,
  
  colour = "grey25",
  
  lineheight = 1.2
) +
  
  # =====================================================
# DIVIDER
# =====================================================

annotate(
  "segment",
  
  x = 0,
  xend = 4.7,
  
  y = 4.25,
  yend = 4.25,
  
  colour = "#D8D0C4",
  
  linewidth = .5
) +
  
  # =====================================================
# LEGEND
# =====================================================

annotate(
  "text",
  
  x = 0,
  y = 3.45,
  
  label = "Wins",
  
  family = "roboto",
  
  fontface = "bold",
  
  colour = "#178A34",
  
  hjust = 0,
  
  size = 5.4
) +
  
  annotate(
    "text",
    
    x = 0,
    y = 2.7,
    
    label = "Draws",
    
    family = "roboto",
    
    fontface = "bold",
    
    colour = "#D9A404",
    
    hjust = 0,
    
    size = 5.4
  ) +
  
  annotate(
    "text",
    
    x = 0,
    y = 1.95,
    
    label = "Losses",
    
    family = "roboto",
    
    fontface = "bold",
    
    colour = "#C8102E",
    
    hjust = 0,
    
    size = 5.4
  ) +
  
  # =====================================================
# SECOND DIVIDER
# =====================================================

annotate(
  "segment",
  
  x = 0,
  xend = 4.7,
  
  y = 1.15,
  yend = 1.15,
  
  colour = "#D8D0C4",
  
  linewidth = .5
) +
  
  # =====================================================
# INSIGHT TEXT
# =====================================================

annotate(
  "text",
  
  x = 0,
  y = -0.1,
  
  label =
    "Manchester City continue to\n\
dominate late-season momentum,\n\
while Arsenal show stronger\n\
consistency in the projected\n\
2024/25 run-in.",
  
  family = "roboto",
  
  hjust = 0,
  
  size = 4.1,
  
  colour = "grey20",
  
  lineheight = 1.35
) +
  
  # =====================================================
# COORDINATES
# =====================================================

coord_cartesian(
  
  xlim = c(0,5),
  
  ylim = c(-1,10)
) +
  
  # =====================================================
# THEME
# =====================================================

theme_void() +
  
  theme(
    
    plot.background =
      
      element_rect(
        fill = bg_col,
        colour = NA
      ),
    
    panel.background =
      
      element_rect(
        fill = bg_col,
        colour = NA
      ),
    
    plot.margin =
      
      margin(
        20,
        10,
        20,
        20
      )
  )