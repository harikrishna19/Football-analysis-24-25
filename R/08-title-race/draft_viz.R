

# Load libraries ----------------------------------------------------------

library(waffle)
library(tidyverse)
library(grid)
library(patchwork)
library(ggimage)
library(ggtext)


# fonts -------------------------------------------------------------------
library(showtext)

font_add_google("Inter", "inter")
font_add_google("Bebas Neue", "bebas")
font_add_google("Anton", "anton")
font_add_google("Roboto Condensed", "roboto")
showtext_auto()

# Load data

data<-read.csv("data/pl_data.csv")
data<-data %>% group_by(season,title) %>% mutate(Matchweek=row_number()) %>% filter(season!="2022/23")

metrics_df<-data  %>% 
  group_by(season,title) %>% 
  select(season,title,wins,draws,loses,pts) %>% 
  summarise(
    "Wins"=sum(wins),
    "Draws"=sum(draws),
    "Losses"=sum(loses),
    "Points"=sum(pts)
  ) %>% filter(season!="2022/23")

last_10<-data %>% group_by(season,title) %>% 
         filter(Matchweek>=29) %>% 
  summarise(
    "Wins"=sum(wins),
    "Draws"=sum(draws),
    "Losses"=sum(loses),
    "Points"=sum(pts),
    "win_per"=(Wins/sum(Wins+Draws+Losses))*100
  ) %>% 
  mutate(
    
    text = paste0(
      
      "<span style='font-size:13pt;'><b>Final10:</b></span>",
      
      "<span style='color:#1B7837;'><b>", Wins, "W</b></span>",
      
      "<span style='color:#C99700;'><b>", Draws, "D</b></span>",
      
      "<span style='color:#B22222;'><b>", Losses, "L</b></span>",
      
      "<span style='color:grey40;'> | </span>",
      
      "<span style='font-size:12pt;'><b>Win Rate:</b> ",
      
      win_per,
      
      "%</span>"
    
  ))
    #mutate(text=paste0("Final10:",Wins,"W"," ",Draws,"D"," ",Losses,"L", " ","Win Rate:", " ", win_per,"%"))



# last_10<-inner_join(last_10 %>% select(-Points),metrics_df %>% select(season,title,Points),by=c("season","title"))
# make_waffle(metrics_df$Wins,metrics_df$Draws,metrics_df$Losses,season = metrics_df$season,metrics_df$title,period = "",metrics_df$Points)

#Logo_data

header_df <- tibble(
  
  team = c(
    "Manchester City",
    "Arsenal"
  ),
  
  x = c(0,1),
  
  y = c(0,1),
  
  colour = c("#6CABDD", "#D00027"),
  
  logo = c(
    
    "https://upload.wikimedia.org/wikipedia/en/e/eb/Manchester_City_FC_badge.svg",
    
    "https://upload.wikimedia.org/wikipedia/en/5/53/Arsenal_FC.svg"
  )
)





make_waffle <- function(wins, draws, losses,
                        season, team, points) {
  vals <- c(
    rep("Wins", wins),
    rep("Draws", draws),
    rep("Losses", losses)
  )
  
  vals <- c(vals, rep("Empty", 40 - length(vals)))
  
  tibble(
    id = 1:40,
    result = vals,
    row = rep(1:4, each = 10),
    col = rep(1:4, times = 4),
    season = season,
    team = team,
    # period = period,
    points = points
  )
}

make_waffle <- function(results) {
  
  vals <- c(results, rep("Empty", 40 - length(results)))
  
  tibble(
    id = 1:40,
    result = vals,
    row = rep(1:5, each = 8),
    col = rep(1:8, times = 5),
    
    tile_label = c(
      paste0("MW", 1:length(results)),
      rep(NA, 40 - length(results))
    )
  )
}


waffle_data <- data %>%
  
  arrange(Matchweek) %>%
  
  group_by(season, title) %>%
  
  summarise(
    results = list(result),
    .groups = "drop"
  ) %>%
  
  mutate(
    waffle = map(results, make_waffle)
  ) %>%
  
  unnest(waffle)

waffle_data<-waffle_data %>% filter(!id %in% c(39,40))

bg_col <- "#F6F1E8"

result_cols <- c(
  "w" = "#4FA66B",
  "d" = "#D8B547",
  "l" = "#D46A6A",
  "Empty" = "#DDD6CC"
)
team_cols <- c(
  "Manchester City" = "#6CABDD",
  "Arsenal"  = "#D00027"
)

base_theme <- theme_minimal(base_family = "anton") +
  theme(
    plot.background = element_rect(
      fill = bg_col,
      color = NA
    ),
    
    panel.background = element_rect(
      fill = bg_col,
      color = NA
    ),
    
    panel.grid = element_blank(),
    
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )


plot_waffle <- function(team_name) {
  plot_df <- waffle_data %>%
    filter(title == team_name)
  run_plot <-
    metrics_df %>%
    filter(title == team_name)
  header_df<-header_df %>% filter(team==team_name)
  
  team_header <- ggplot() +

    geom_image(

      data =
        header_df %>%
        filter(team == team_name),

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

      label = team_name,

      family = "anton",

      fontface = "bold",

      colour = team_cols[[team_name]],

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
  
  ggplot(plot_df,
         aes(col, -row)) +
    
    geom_tile(
      aes(fill = result),
      color = "white",
      linewidth = 0.45,
      width = 0.93,
      height = 0.93
    ) +
    geom_text(
      data = metrics_df %>% filter(title==team_name),
      aes(
        x = 0,
        y = -1.0,vjust=-2.3,hjust=-2.5,
        label = paste0(Points, " Points")
      ),
      fontface="bold",
      inherit.aes = T,
      size = 4
    ) +
#     geom_image(
#   
#   data =
#     
#     header_df %>%
#     
#     filter(team == team_name),
#   
#   aes(
#     x = 4.5,
#     y = 1.8,
#     image = logo
#   ),
#   
#   inherit.aes = FALSE,
#   
#   size = 0.08
# )+
    facet_grid(
      season ~ .,
      switch = "y"
    ) +
    ggtext::geom_richtext(
      
      data = last_10 %>%
        filter(title == team_name),
      
      aes(
        x = 4.5,
        y = -6.3,
        label = text
      ),
      inherit.aes = FALSE,
      
      fill = "#F7F2E8",
      
      label.color = "#D8D0C4",
      
      label.size = 0.8,
      
      colour = "black",
      
      size = 6,
      
      lineheight = 1.1,
      
      label.r = unit(0.18, "lines")
    )+
    
    scale_fill_manual(values = result_cols) +
    
    coord_equal() +
    
    labs(
      title = "",
      subtitle = team_name
    ) +
    
    base_theme +
    
    theme(
      
      strip.text.y = element_text(
        size = 14,
        face = "bold",
        margin = margin(r = 15)
      ),
      
      plot.title = element_text(
        size = 12,
        face = "bold",
        hjust = 0.5,
        colour = "black"
      ),
      plot.subtitle = element_text(
        size = 22,
        face = "bold",
        hjust = 0.5,
        colour = team_cols[[team_name]]
      ),
      
      legend.position = "none",
      
      panel.spacing.y = unit(1.8, "lines")
    ) 
  
  
}

# =========================================================
# LEFT PANEL WITH LOGO
# =========================================================


# =========================================================
# RIGHT INSIGHT PANEL
# =========================================================

insight_panel <- ggplot() +
  
  annotate(
    "label",
    x = 1,
    y = 8.5,
    
    label =
      "2022/23\nCity won 16 of 19\nmatches during the\nbusiness end.",
    
    fill = "white",
    colour = "#6CABDD",
    label.size = 0,
    fontface = "bold",
    size = 4.5
  ) +
  
  annotate(
    "label",
    x = 1,
    y = 5.2,
    
    label =
      "2023/24\nArsenal pushed\nCity until the\nfinal weeks.",
    
    fill = "white",
    colour = "#D00027",
    label.size = 0,
    fontface = "bold",
    size = 4.5
  ) +
  
  annotate(
    "label",
    x = 1,
    y = 2,
    
    label =
      "2024/25\nArsenal show the\nstronger projected\nrun-in form.",
    
    fill = "white",
    colour = "#1B7837",
    label.size = 0,
    fontface = "bold",
    size = 4.5
  ) +
  
  xlim(0, 2) +
  ylim(0, 10) +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(
      fill = bg_col,
      colour = NA
    )
  )

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
  
  family = "Nunito",
  
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
    "<span style='color:#4FA66B;'><b>Wins</b></span>
      <span style='color:grey45;'>|</span>
      <span style='color:#D8B547;'><b>Draws</b></span>
      <span style='color:grey45;'> | </span>
      <span style='color:#D46A6A;'><b>Losses</b></span>
      <span style='color:grey45;'> | </span>
      <span style='color:#C8C1B6;'><b> Matches-Remaining</b></span>",
  
  family = "roboto",
  
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
  
  family = "Nunito",
  fontface="bold",
  
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
• Arsenal recovered after defeats after the final international break
• Everton held City to a draw which was crucial
• Arsenal maintained April momentum and were top for 200+ days
• Only two points separate sides
• Matchweek 38 decides the title

Data is updated till MatchWeek 36 for the ongoing season(2025/26)",

  
  family = "Nunito",
  
  fontface = "plain",
  
  hjust = 0,
  
  vjust = 1,
  
  size = 5.8,
  
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
  
  family = "Nunito",
  
  hjust = 0,
  fontface="bold",
  
  size = 4.3,
  
  colour = "black"
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


logo_panel + plot_waffle("Manchester City") + plot_waffle("Arsenal")+  plot_layout(
  widths = c(8, 5,5)
)

