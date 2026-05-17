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
    "PREMIER LEAGUE \nTITLE RACE TRENDS",
  
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
    "Each square represents one league match.",
  
  family = "anton",
  
  hjust = 0,
  
  size = 5,
  
  colour = "black",
  
  lineheight = 2.2
) +
  
  # =====================================================
# DIVIDER
# =====================================================
# 
# annotate(
#   "segment",
#   
#   x = 0,
#   xend = 4.7,
#   
#   y = 4,
#   yend = 4.25,
#   
#   colour = "#D8D0C4",
#   
#   linewidth = .5
# ) +
#   
  # =====================================================
# LEGEND
# =====================================================
annotate(
  "richtext",
  
  x = 0,
  y = 4.5,
  
  label =
    "<span style='color:#178A34;'>Wins</span>
     <span style='color:grey40;'>|</span>
     <span style='color:#D9A404;'>Draws</span>
     <span style='color:grey40;'>|</span>
     <span style='color:#C8102E;'>Losses</span>",
  
  family = "roboto",
  
  fontface = "bold",
  
  hjust = 0,
  
  size = 5.2,
  
  fill = NA,
  
  label.color = NA
)+
  # =====================================================
# SECOND DIVIDER
# =====================================================

# annotate(
#   "segment",
#   
#   x = 0,
#   xend = 4.7,
#   
#   y = 2.15,
#   yend = 2.15,
#   
#   colour = "#D8D0C4",
#   
#   linewidth = .5
# ) +
  
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
        5,
        10,
        20,
        20
      )
  )


team_header <- ggplot() +
  
  geom_image(
    
    data =
      header_df %>%
      filter(team == "Manchester City"),
    
    aes(
      x = 0.8,
      y = 1,
      image = logo
    ),
    
    size = 0.08
  ) +
  
  annotate(
    "text",
    
    x = 1.7,
    y = 1,
    
    label = "Manchester City",
    
    family = "anton",
    
    fontface = "bold",
    
    colour = team_cols[["Manchester City"]],
    
    hjust = 0,
    
    size = 9
  ) +
  
  coord_cartesian(
    xlim = c(0,6),
    ylim = c(0,2)
  ) +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(
      fill = bg_col,
      colour = NA
    )
  )
