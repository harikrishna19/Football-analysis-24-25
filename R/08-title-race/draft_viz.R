

# Load data

data<-read.csv("data/pl_data.csv")


metrics_df<-data  %>% 
  group_by(season,title) %>% 
  select(season,title,wins,draws,loses,pts) %>% 
  summarise(
    "Wins"=sum(wins),
    "Draws"=sum(draws),
    "Losses"=sum(loses),
    "Points"=sum(pts)
  )
# make_waffle(metrics_df$Wins,metrics_df$Draws,metrics_df$Losses,season = metrics_df$season,metrics_df$title,period = "",metrics_df$Points)

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
    row = rep(1:5, each = 8),
    col = rep(1:8, times = 5),
    season = season,
    team = team,
    # period = period,
    points = points
  )
}

waffle_data <- pmap_dfr(
  list(metrics_df$Wins,
       metrics_df$Draws,
       metrics_df$Losses,
       metrics_df$season,
       metrics_df$title,
       # df$period,
       metrics_df$Points),
  make_waffle
)


bg_col <- "#F6F1E8"

result_cols <- c(
  "Wins"   = "#1B7837",
  "Draws"  = "#C99700",
  "Losses" = "#B22222",
  "Empty"  = "#E8DED0"
)

team_cols <- c(
  "Manchester City" = "#6CABDD",
  "Arsenal"  = "#D00027"
)

base_theme <- theme_minimal(base_family = "Helvetica") +
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
    filter(team == team_name)
  
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
      data = plot_df %>%
        distinct(season,points),
      
      aes(
        x = 4.5,
        y = 1.2,
        label = paste0(points, " pts")
      ),
      
      inherit.aes = FALSE,
      fontface = "bold",
      size = 4.2
    ) +
    
    facet_grid(
      season ~ .,
      switch = "y"
    ) +
    
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
        size = 16,
        face = "bold",
        hjust = 0.5,
        colour = "black"
      ),
      
      plot.subtitle = element_text(
        size = 13,
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

logo_panel <- ggplot() +
  
  annotation_custom(
    rasterGrob(pl_logo,
               interpolate = TRUE),
    xmin = 0,
    xmax = 5,
    ymin = 6,
    ymax = 10
  ) +
  
  annotate(
    "text",
    x = 0,
    y = 5.2,
    
    label = "Premier League title race trends\nacross recent seasons",
    
    hjust = 0,
    size = 5.2,
    colour = "grey25",
    lineheight = 1.2
  ) +
  
  annotate(
    "text",
    x = 0,
    y = 3.8,
    
    label = "■ Wins",
    colour = "#1B7837",
    fontface = "bold",
    hjust = 0,
    size = 5
  ) +
  
  annotate(
    "text",
    x = 0,
    y = 3.0,
    
    label = "■ Draws",
    colour = "#C99700",
    fontface = "bold",
    hjust = 0,
    size = 5
  ) +
  
  annotate(
    "text",
    x = 0,
    y = 2.2,
    
    label = "■ Losses",
    colour = "#B22222",
    fontface = "bold",
    hjust = 0,
    size = 5
  ) +
  
  annotate(
    "text",
    x = 0,
    y = 0.8,
    
    label =
      "Manchester City dominated\nlate-season momentum during\n2022/23 and 2023/24,\nwhile Arsenal appear stronger\nin the projected 2024/25 run-in.",
    
    hjust = 0,
    size = 4.2,
    colour = "grey20",
    lineheight = 1.3
  ) +
  
  xlim(0, 10) +
  ylim(0, 10) +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(
      fill = bg_col,
      colour = NA
    )
  )


logo_panel + plot_waffle("Manchester City") + plot_waffle("Arsenal")
