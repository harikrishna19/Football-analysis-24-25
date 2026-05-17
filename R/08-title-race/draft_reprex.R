  # =========================================================
  # FT STYLE PREMIER LEAGUE TITLE RACE INFOGRAPHIC
  # Arsenal vs Man City
  # =========================================================
  
  library(tidyverse)
  library(patchwork)
  library(ggtext)
  library(grid)
  library(png)
  
  # =========================================================
  # LOAD PREMIER LEAGUE LOGO
  # =========================================================
  # Download PL logo manually and save locally
  # Example:
  # pl_logo.png
  
  # pl_logo <- png::readPNG("pl_logo.png")
  
  # =========================================================
  # DATA
  # =========================================================
  
  df <- tribble(
    ~season,    ~team,       ~period,        ~wins, ~draws, ~losses, ~points,
    
    "2022/23",  "Man City",  "Full Season",    28,      5,      5,      89,
    "2022/23",  "Arsenal",   "Full Season",    26,      6,      6,      84,
    
    "2022/23",  "Man City",  "Jan-May",        16,      2,      1,      50,
    "2022/23",  "Arsenal",   "Jan-May",        10,      4,      5,      34,
    
    "2023/24",  "Man City",  "Full Season",    29,      6,      3,      93,
    "2023/24",  "Arsenal",   "Full Season",    28,      5,      5,      89,
    
    "2023/24",  "Man City",  "Jan-May",        18,      1,      0,      55,
    "2023/24",  "Arsenal",   "Jan-May",        14,      3,      2,      45,
    
    "2024/25",  "Man City",  "Full Season",    25,      7,      6,      82,
    "2024/25",  "Arsenal",   "Full Season",    27,      7,      4,      88,
    
    "2024/25",  "Man City",  "Jan-May",        11,      4,      4,      37,
    "2024/25",  "Arsenal",   "Jan-May",        15,      2,      2,      47
  )
  
  # =========================================================
  # BUILD WAFFLE DATA
  # =========================================================
  
  make_waffle <- function(wins, draws, losses,
                          season, team, period, points) {
    
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
      period = period,
      points = points
    )
  }
  
  waffle_data <- pmap_dfr(
    list(df$wins,
         df$draws,
         df$losses,
         df$season,
         df$team,
         df$period,
         df$points),
    make_waffle
  )
  
  # =========================================================
  # THEME + COLORS
  # =========================================================
  
  bg_col <- "#F6F1E8"
  
  result_cols <- c(
    "Wins"   = "#1B7837",
    "Draws"  = "#C99700",
    "Losses" = "#B22222",
    "Empty"  = "#E8DED0"
  )
  
  team_cols <- c(
    "Man City" = "#6CABDD",
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
  
  # =========================================================
  # WAFFLE FUNCTION
  # =========================================================
  
  plot_waffle <- function(team_name, period_name) {
    
    plot_df <- waffle_data %>%
      filter(team == team_name,
             period == period_name)
    
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
          distinct(season, points),
        
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
        title = period_name,
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
  
  # =========================================================
  # BUILD PLOTS
  # =========================================================
  
  city_full <- plot_waffle(
    "Man City",
    "Full Season"
  )
  
  ars_full <- plot_waffle(
    "Arsenal",
    "Full Season"
  )
  
  city_jm <- plot_waffle(
    "Man City",
    "Jan-May"
  )
  
  ars_jm <- plot_waffle(
    "Arsenal",
    "Jan-May"
  )
  
  # =========================================================
  # REMOVE DUPLICATE SEASON LABELS
  # =========================================================
  
  ars_full <- ars_full +
    theme(strip.text.y = element_blank())
  
  city_jm <- city_jm +
    theme(strip.text.y = element_blank())
  
  ars_jm <- ars_jm +
    theme(strip.text.y = element_blank())
  
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
  
  # =========================================================
  # FINAL LAYOUT
  # =========================================================
  
  final_plot <-
    
    logo_panel +
    
    city_full +
    ars_full +
    city_jm +
    ars_jm +
    
    insight_panel +
    
    plot_layout(
      widths = c(1.6, 1, 1, 1, 1, 1.1)
    )
  
  # =========================================================
  # DISPLAY
  # =========================================================
  
  final_plot
  
  # =========================================================
  # SAVE
  # =========================================================
  
  ggsave(
    "pl_title_race_infographic.png",
    final_plot,
    width = 19,
    height = 11,
    dpi = 320,
    bg = bg_col
  )