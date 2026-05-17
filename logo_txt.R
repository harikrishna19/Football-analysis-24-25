logo_panel <- ggplot() +
  
  # =====================================================
# PREMIER LEAGUE LOGO
# =====================================================

annotation_custom(
  
  rasterGrob(
    pl_logo,
    interpolate = TRUE
  ),
  
  xmin = 1.15,
  xmax = 3.85,
  
  ymin = 8.2,
  ymax = 10.8
) +
  
  # =====================================================
# MAIN TITLE
# =====================================================

annotate(
  "text",
  
  x = 0,
  y = 6.8,
  
  label = "PREMIER LEAGUE TITLE RACE",
  
  family = "anton",
  
  fontface = "plain",
  
  hjust = 0,
  
  size = 11,
  
  colour = "#111111",
  
  lineheight = 0.9
) +
  
  # =====================================================
# SUBTITLE
# =====================================================

annotate(
  "text",
  
  x = 0,
  y = 5.65,
  
  label = "Each square represents one league match",
  
  family = "inter",
  
  fontface = "plain",
  
  hjust = 0,
  
  size = 6.3,
  
  colour = "grey20"
) +
  
  # =====================================================
# LEGEND
# =====================================================

annotate(
  "richtext",
  
  x = 0,
  y = 4.85,
  
  label =
    "<span style='color:#1B7837;'><b>Wins</b></span>
      <span style='color:grey45;'>|</span>
      <span style='color:#C99700;'><b>Draws</b></span>
      <span style='color:grey45;'> | </span>
      <span style='color:#B22222;'><b>Losses</b></span>
      <span style='color:grey45;'> | </span>
      <span style='color:#C8C1B6;'><b> Matches-Remaining</b></span>",
  
  family = "inter",
  
  hjust = 0,
  
  size = 6,
  
  fill = NA,
  
  label.color = NA
) +
  
  # =====================================================
# DIVIDER
# =====================================================

annotate(
  "segment",
  
  x = 0,
  xend = 4.7,
  
  y = 4.15,
  yend = 4.15,
  
  colour = "#CFC6B8",
  
  linewidth = 0.5
) +
  
  # =====================================================
# INSIGHTS HEADER
# =====================================================

annotate(
  "text",
  
  x = 0,
  y = 3.45,
  
  label = "2025/26 SEASON KEY INSIGHTS",
  
  family = "anton",
  
  hjust = 0,
  
  size = 5.6,
  
  colour = "#111111"
) +
  
  # =====================================================
# INSIGHTS TEXT
# =====================================================

annotate(
  "text",
  
  x = 0,
  y = 2.4,
  
  label =
    "• City dominate recent title run-ins
• Arsenal recovered after defeats
• Everton held City to a draw
• Arsenal maintained April momentum
• Only two points separate sides
• Matchweek 38 decides the title

 City or Arsenal?",
  
  family = "inter",
  
  fontface = "plain",
  
  hjust = 0,
  
  vjust = 1,
  
  size = 6.8,
  
  colour = "#151515",
  
  lineheight = 1.15
) +
  
  # =====================================================
# FOOTER
# =====================================================

annotate(
  "text",
  
  x = 0,
  y = -2.9,
  
  label = "Data: UNDERSTATAPI | Graphic: HARI KRISHNA",
  
  family = "inter",
  
  hjust = 0,
  fontface="bold",
  
  size = 4.3,
  
  colour = "grey45"
) +
  
  # =====================================================
# COORDINATES
# =====================================================

coord_cartesian(
  
  xlim = c(0, 5),
  
  ylim = c(-4, 10),
  
  clip = "off"
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
        10,
        15,
        25,
        20
      )
  )