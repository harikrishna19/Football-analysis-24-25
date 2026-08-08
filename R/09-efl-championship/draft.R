
# Load required libraries for analysis ------------------------------------
library(tidyverse)
library(engsoccerdata)
library(cowplot)
library(ggtext)
library(soccerplotR)
library(showtext)




# Load engsoccerdata from devtools ----------------------------------------
eng_cship<-engsoccerdata::england_current()
eng_cship<-eng_cship %>% dplyr::filter(division==2)

# get league table for the season

league_table<-engsoccerdata::maketable_eng(eng_cship,2025,2,2)



# Data Processing ---------------------------------------------------------

# filter out only the top 6
top_6<-head(league_table,6)
teams <- unique(top_6$team)
eng_cship_h <- eng_cship %>% filter(home %in% teams)
eng_cship_v <- eng_cship %>% filter(visitor %in% teams)
eng_cship<-bind_rows(eng_cship_h,eng_cship_v)
eng_cship<-eng_cship %>% distinct()


# Home Away Table ---------------------------------------------------------
away<-maketable_all(eng_cship,2025,2,3,type="away") %>% filter(team %in% teams)
home<-maketable_all(eng_cship,2025,2,3,type="home") %>% filter(team %in% teams)

# Home/Away Data ----------------------------------------------------------
plot_df <-
  home %>%
  rename(Home = Pts) %>%
  left_join(
    away %>% rename(Away = Pts),
    by = "team"
  ) %>%
  mutate(
    Home = -Home,
    Difference = Away - abs(Home)
  ) %>%
  arrange(abs(Home)) %>%
  mutate(
    team = factor(team, levels = team)
  )



# Colours for the plots ---------------------------------------------------

home_col <- "#7DB7E8"
away_col <- "#F4A261"

bg_col <- "#F8FAFC"

grid_col <- "#E2E8F0"

text_col <- "#1E293B"


# Plot --------------------------------------------------------------------
# For logos
team_names<-c("Hull","Middlesbrough","Millwall","Southampton","Ipswich","Coventry")
plot_df[['team_name']]<-team_names
plot_df$pts<-abs(plot_df$Home)+abs(plot_df$Away)
plot_df<-plot_df %>% arrange(pts)
chart<-ggplot(plot_df) +

  ## Home bars
  geom_col(
    aes(x = Home, y = team,fill="Home"),
    width = 0.05
  ) +

  ## Away bars
  geom_col(
    aes(x = Away, y = team,fill="Away"),
    width = 0.05
  ) +
  annotate(
    "text",
    x = -35,
    y = 0.5,
    label = "Home",
    colour = home_col,
    fontface = "bold",
    size = 4.5
  ) +
  
  annotate(
    "text",
    x = 35,
    y = 0.5,
    label = "Away",
    colour = away_col,
    fontface = "bold",
    size = 4.5
  ) +
  scale_fill_manual(
    values = c(
      "Home" = home_col,
      "Away" = away_col
    ),
    name = NULL
  ) +
  ## Team logos
  geom_soccer_logos(
    aes(
      x = -68,
      y = team,
      team_name = team_names
    ),
    width = 0.09
  ) +
  
  ## Home labels
  geom_text(
    aes(Home - 3, team,
        label = abs(Home)),
    colour = text_col,
    fontface = "bold",
    size = 4.5
  ) +
  
  ## Away labels
  geom_text(
    aes(Away + 3, team,
        label = Away),
    colour = text_col,
    fontface = "bold",
    size = 4.5
  ) +
  annotate(
    "text",
    x = -48,
    y = 6.25,
    label = "Coventry were the better side and earned promotion/n
    Lampard also signed a new contract",
    hjust = 0,
    vjust = 0,
    fontface = "bold",
    size = 3.4,
    lineheight = 1.1,
    colour = col_text_dark
  )+
  annotate(
    "text",
    x = -48,
    y = 5.25,
    label = "Kieran Mckenna won Ipswich back to back promotions",
    hjust = 0,
    vjust = 0,
    fontface = "bold",
    size = 3.4,
    lineheight = 1.1,
    colour = col_text_dark
  )+
  annotate(
    "text",
    x = -42,
    y = 3.35,
    label = "◯",
    size = 9,
    colour = "#DC2626"
  ) +
  
  annotate(
    "text",
    x = -42,
    y = 3.35,
    label = "S",
    size = 3.2,
    fontface = "bold",
    colour = "#DC2626"
  ) +
  
  annotate(
    "text",
    x = -38,
    y = 3.25,
    label = "Southampton were expelled from the Championship",
    hjust = 0,
    vjust = 0.5,
    fontface = "bold",
    size = 3.4,
    lineheight = 1.1,
    colour = "#DC2626"
  ) +
  scale_x_continuous(
    limits = c(-70, 70),
    breaks = seq(-60, 60, 20),
    labels = abs,
    expand = expansion(mult = c(.02, .02))
  ) +
  
  coord_cartesian(clip = "off") +
  
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL,
    y = NULL
    # caption = "Design:Hari Krishna"
  ) +
  
  theme_minimal(base_size = 15) +
  
  theme(
    legend.position ="none",
    text = element_text(family = "Inter"),
    plot.background = element_rect(
      fill = bg_col,
      colour = NA
    ),
    
    panel.background = element_rect(
      fill = bg_col,
      colour = NA
    ),
    plot.caption = element_text(hjust = 0.5
    ),
    panel.grid.major.y = element_blank(),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_line(
      colour = grid_col,
      linewidth = .5
    ),
    
    axis.text.y = element_blank(),
    
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    
    plot.title = element_text(
      size = 20,
      face = "bold",
      colour = text_col
    ),
    
    plot.subtitle = element_text(
      size = 12,
      colour = "#64748B"
    ),
    
    plot.margin = margin(
      0,
      0,
      0,
      0
    )
  )

chart
# Card code ---------------------------------------------------------------

# ============================================================================
# EFL CHAMPIONSHIP DASHBOARD — TEMPLATE (v3: half-time state edition)
# ============================================================================
# Replace `teams_data` with your real rows. Required columns:
#   team, GP, W, D, L, gf, ga, gd, Pts, Pos,
#   drawing_games, leading_games, trailing_games,
#   drawing_pts_won, leading_pts_won, trailing_pts_won,
#   drawing_max_pts, leading_max_pts, trailing_max_pts,
#   drawing_pct_pts, leading_pct_pts, trailing_pct_pts,
#   Logo
# `Rank` (used for accent-tier colouring) is computed from Pts, not taken
# from your data, so cards always tier correctly regardless of what a
# `Rank`/`Pos` column in your source means.
# ============================================================================


#===================================================
# 1. PALETTE / TYPOGRAPHY
#===================================================

col_bg_dark    <- "#0B1220"
col_card_bg    <- "#FFFFFF"
col_border     <- "#E2E8F0"
col_accent     <- "#2563EB"
col_accent_lt  <- "#93C5FD"
col_text_dark  <- "#0F172A"
col_text_mid   <- "#475569"
col_text_light <- "#94A3B8"
col_green      <- "#059669"
col_red        <- "#DC2626"
col_grey_state <- "#64748B"   # "drawing" state colour
col_gold       <- "#D97706"
col_grey_rank  <- "#64748B"
col_shadow     <- "#94A3B8"

sz_title    <- 12
sz_subtitle <- 5
sz_section  <- 4.2
sz_value    <- 4.3
sz_footer   <- 4.9

#===================================================
# 4. CARD
#===================================================

make_card <- function(t, t_names) {
  
  rank_colour <- if (t$Rank %in% c(1,2,6)) col_gold else if (t$Rank <= 6) col_grey_rank
  tint_alpha  <- if (t$Rank == 1) 0.10 else if (t$Rank <= 3) 0.05 else 0
  
  card_title <- trunc_label(t$team, 16)
  initials   <- paste(substr(strsplit(t$team, " ")[[1]], 1, 1), collapse = "")
  
  pts_delta    <- t$Pts - avg_pts
  delta_label  <- paste0(ifelse(pts_delta >= 0, "+", ""),
                         round(pts_delta, 0),
                         " pts vs league avg")
  delta_colour <- if (pts_delta >= 0) col_green else col_red
  
  
  # -------------------------------------------------------
  # BADGE
  # -------------------------------------------------------
  
  badge_label <- ""
  badge_colour <- col_accent
  
  badge_layer <- if (nzchar(badge_label)) {
    
    annotate(
      "label",
      x = 0.7,
      y = 12.15,
      hjust = 0,
      label = badge_label,
      fill = badge_colour,
      colour = "white",
      size = 2.3,
      fontface = "bold",
      label.size = 0
    )
    
  } else NULL
  
  
  # -------------------------------------------------------
  # GOALS DIVERGING BAR
  # -------------------------------------------------------
  
  goal_scale <- 3.2 / max_goals
  
  gf_end <- 5 + t$gf * goal_scale
  ga_end <- 5 - t$ga * goal_scale
  
  gd_label  <- paste0(ifelse(t$gd >= 0, "+", ""), t$gd, " GD")
  gd_colour <- if (t$gd >= 0) col_green else col_red
  
  
  # -------------------------------------------------------
  # C. HALF-TIME STATE DISTRIBUTION
  # -------------------------------------------------------
  
  seg_x0 <- 1.0
  seg_x1 <- 9.0
  total_w <- seg_x1 - seg_x0
  
  lw <- (t$leading_games  / t$GP) * total_w
  dw <- (t$drawing_games  / t$GP) * total_w
  tw <- (t$trailing_games / t$GP) * total_w
  
  l_x0  <- seg_x0
  l_x1  <- l_x0 + lw
  
  d_x0  <- l_x1
  d_x1  <- d_x0 + dw
  
  tr_x0 <- d_x1
  tr_x1 <- tr_x0 + tw
  
  
  # -------------------------------------------------------
  # D. POINTS WON BY HALF-TIME STATE
  # -------------------------------------------------------
  
  bullet_x0 <- 3.0
  bullet_x1 <- 9.0
  bullet_w  <- bullet_x1 - bullet_x0
  
  scale_pct <- function(pct) {
    bullet_x0 + (pct / 100) * bullet_w
  }
  
  
  ht_dots <- data.frame(
    state  = c("Leading", "Drawing", "Trailing"),
    pct    = c(
      t$leading_pct_pts,
      t$drawing_pct_pts,
      t$trailing_pct_pts
    ),
    avg    = c(
      avg_leading_pct,
      avg_drawing_pct,
      avg_trailing_pct
    ),
    colour = c(
      col_green,
      col_grey_state,
      col_red
    ),
    stringsAsFactors = FALSE
  )
  
  
  # -------------------------------------------------------
  # DYNAMIC DUMBBELL LABEL POSITIONING
  # -------------------------------------------------------
  
  ht_dots$x     <- scale_pct(ht_dots$pct)
  ht_dots$avg_x <- scale_pct(ht_dots$avg)
  
  # Sort from left to right
  ht_dots <- ht_dots[order(ht_dots$x), ]
  
  row_y <- 2.35
  
  # Minimum horizontal distance between labels
  min_gap <- 0.85
  
  # Start with labels above the dumbbell
  ht_dots$label_y <- row_y + 0.48
  ht_dots$label_vj <- 0
  
  # Check neighbouring dots.
  # If dots are close, alternate the labels vertically.
  for (i in 2:nrow(ht_dots)) {
    
    if (abs(ht_dots$x[i] - ht_dots$x[i - 1]) < min_gap) {
      
      # Push the current label below
      ht_dots$label_y[i] <- row_y - 0.48
      ht_dots$label_vj[i] <- 1
      
      # If three dots are extremely close,
      # move the previous label slightly higher.
      if (
        i >= 3 &&
        abs(ht_dots$x[i] - ht_dots$x[i - 2]) < min_gap
      ) {
        
        ht_dots$label_y[i - 1] <- row_y + 0.68
      }
    }
  }
  
  
  # -------------------------------------------------------
  # LABEL TEXT
  # -------------------------------------------------------
  
  ht_dots$label <- paste0(
    # ht_dots$state,
    # " ",
    round(ht_dots$pct),
    "%"
  )
  
  
  # -------------------------------------------------------
  # PLOT
  # -------------------------------------------------------
  
  ggplot() +
    
    # Card background
    annotate(
      "rect",
      xmin = 0.15,
      xmax = 10.15,
      ymin = -0.15,
      ymax = 13.85,
      fill = col_shadow,
      alpha = 0.35,
      colour = NA
    ) +
    
    annotate(
      "rect",
      xmin = 0,
      xmax = 10,
      ymin = 0,
      ymax = 14,
      fill = rank_colour,
      alpha = tint_alpha,
      colour = NA
    ) +
    
    annotate(
      "rect",
      xmin = 0,
      xmax = 10,
      ymin = 0,
      ymax = 14,
      fill = col_card_bg,
      colour = col_border,
      linewidth = 0.8
    ) +
    
    annotate(
      "rect",
      xmin = 0,
      xmax = 10,
      ymin = 13.5,
      ymax = 14,
      fill = rank_colour,
      colour = NA
    ) +
    
    annotate(
      "text",
      x = 0.4,
      y = 13.75,
      label = case_when(
        
        t$team %in% c("Coventry City", "Ipswich Town") ~
          "Promoted to Premier League 2026/27",
        
        t$team == "Hull City" ~
          "Promotion via Playoffs 2026/27",
        
        t$team %in% c("Southampton", "Middlesbrough", "Millwall") ~
          "Finished in Play-off Places in Top 6",
        
        TRUE ~ ""
      ),
      hjust = -0.05,
      vjust = 0.4,
      colour = "black",
      fontface = "bold",
      size = 4.2
    ) +
    
    # Team name
    annotate(
      "text",
      x = 1.5,
      y = 9,
      label = ifelse(
        t$team == "Millwall",
        "Millwall FC",
        card_title
      ),
      angle = 90,
      hjust = 0,
      vjust = 1.5,
      fontface = "bold",
      size = 5,
      colour = col_text_dark
    ) +
    
    # Logo
    geom_soccer_logos(
      aes(
        x = 8.7,
        y = 10.6,
        team_name = t_names$logo
      ),
      width = 0.2
    ) +
    
    badge_layer +
    
    # Record
    geom_richtext(
      aes(
        x = 5,
        y = 10.9,
        label = paste0(
          "<span style='color:#22C55E'><b>", t$W, "W</b></span> - ",
          "<span style='color:#CBD5E1'><b>", t$D, "D</b></span> - ",
          "<span style='color:#EF4444'><b>", t$L, "L</b></span>"
        )
      ),
      size = 5.9,
      fill = NA,
      label.color = NA,
      colour = col_text_mid
    ) +
    
    annotate(
      "text",
      x = 5,
      y = 10,
      label = paste0(t$Pts, " PTS"),
      fontface = "bold",
      size = 11.0,
      colour = col_text_dark
    ) +
    
    annotate(
      "text",
      x = 5,
      y = 9.1,
      label = paste0(
        round(t$Pts / t$GP, 2),
        " pts / game"
      ),
      colour = col_text_mid,
      size = 5.4
    ) +
    
    annotate(
      "segment",
      x = 0.6,
      xend = 9.4,
      y = 8.30,
      yend = 8.30,
      colour = col_border
    ) +
    
    
    # -------------------------------------------------------
  # C. GOALS
  # -------------------------------------------------------
  
  annotate(
    "text",
    x = 0.6,
    y = 6.95,
    hjust = 0,
    fontface = "bold",
    size = sz_section,
    colour = col_text_mid,
    label = "GOALS (FOR / AGAINST)"
  ) +
    
    annotate(
      "text",
      x = 9.4,
      y = 6.55,
      hjust = 1,
      fontface = "bold",
      size = sz_value,
      colour = gd_colour,
      label = gd_label
    ) +
    
    annotate(
      "rect",
      xmin = 5,
      xmax = gf_end,
      ymin = 5.90,
      ymax = 6.20,
      fill = col_green
    ) +
    
    annotate(
      "rect",
      xmin = ga_end,
      xmax = 5,
      ymin = 5.90,
      ymax = 6.20,
      fill = col_red
    ) +
    
    annotate(
      "segment",
      x = 5,
      xend = 5,
      y = 5.70,
      yend = 6.40,
      colour = col_grey_rank,
      linewidth = 0.6
    ) +
    
    annotate(
      "text",
      x = gf_end + 0.12,
      y = 6.05,
      hjust = 0,
      size = sz_value,
      fontface = "bold",
      colour = col_green,
      label = t$gf
    ) +
    
    annotate(
      "text",
      x = ga_end - 0.12,
      y = 6.05,
      hjust = 1,
      size = sz_value,
      fontface = "bold",
      colour = col_red,
      label = t$ga
    ) +
    
    
    # -------------------------------------------------------
  # D. HALF-TIME STATE
  # -------------------------------------------------------
  
  geom_richtext(
    aes(
      x = 0.6,
      y = 5.05,
      label = "HALF TIME STATE"
    ),
    hjust = 0,
    vjust = 0.5,
    size = 4.2,
    fill = NA,
    label.color = NA,
    colour = col_text_mid
  ) +
    # Stacked bar
    annotate(
      "rect",
      xmin = l_x0,
      xmax = l_x1,
      ymin = 4.35,
      ymax = 4.65,
      fill = col_green
    ) +
    
    annotate(
      "rect",
      xmin = d_x0,
      xmax = d_x1,
      ymin = 4.35,
      ymax = 4.65,
      fill = col_grey_state
    ) +
    
    annotate(
      "rect",
      xmin = tr_x0,
      xmax = tr_x1,
      ymin = 4.35,
      ymax = 4.65,
      fill = col_red
    ) +
    
    # State labels
    annotate(
      "text",
      x = 1.0,
      y = 4.05,
      hjust = 0,
      size = 2.5,
      colour = col_green,
      label = paste0(
        "Leading: ",
        t$leading_games
      )
    ) +
    
    annotate(
      "text",
      x = 5,
      y = 4.05,
      hjust = 0.5,
      size = 2.5,
      colour = col_grey_state,
      label = paste0(
        "Drawing: ",
        t$drawing_games
      )
    ) +
    
    annotate(
      "text",
      x = 9,
      y = 4.05,
      hjust = 1,
      size = 2.5,
      colour = col_red,
      label = paste0(
        "Trailing: ",
        t$trailing_games
      )
    ) +
    
    
    # -------------------------------------------------------
  # E. POINTS WON BY HALF-TIME STATE
  # ALWAYS AFTER HALF-TIME STATE
  # -------------------------------------------------------
  
  # annotate(
  #   "text",
  #   x = 0.6,
  #   y = .15,
  #   hjust = 0,
  #   # fontface = "bold",
  #   size = sz_section,
  #   colour = col_text_mid,
  #   size=2,
  #   label = "POINTS WON BY HALF-TIME STATE"
  # ) +
  
  # Full 0–100% track
  annotate(
    "segment",
    x = bullet_x0,
    xend = bullet_x1,
    y = row_y,
    yend = row_y,
    colour = col_border,
    linewidth = 2,
    lineend = "round"
  ) +
    
    # Dumbbell body
    annotate(
      "segment",
      x = min(ht_dots$x),
      xend = max(ht_dots$x),
      y = row_y,
      yend = row_y,
      colour = col_grey_state,
      linewidth = 1
    ) +
    
    # League-average ticks
    geom_segment(
      data = ht_dots,
      aes(
        x = avg_x,
        xend = avg_x,
        y = row_y - 0.16,
        yend = row_y + 0.16,
        colour = colour
      ),
      linewidth = 0.5,
      linetype = "dashed"
    ) +
    
    # Dots
    geom_point(
      data = ht_dots,
      aes(
        x = x,
        y = row_y,
        colour = colour
      ),
      size = 4.6
    ) +
    
    # -------------------------------------------------------
  # LEADER LINES
  # Connect displaced labels to their dots
  # -------------------------------------------------------
  
  geom_segment(
    data = ht_dots[
      abs(ht_dots$label_y - row_y) > 0.01,
    ],
    aes(
      x = x,
      xend = x,
      y = row_y,
      yend = label_y
    ),
    colour = col_border,
    linewidth = 0.35,
    linetype = "dotted"
  ) +
    
    # Dynamic labels
    geom_text(
      data = ht_dots,
      aes(
        x = x,
        y = label_y,
        label = label,
        colour = colour,
        vjust = label_vj
      ),
      size = 3.3,
      fontface = "bold"
    ) +
    
    scale_colour_identity() +
    
    coord_cartesian(
      xlim = c(-0.3, 10.3),
      ylim = c(-0.3, 14.3),
      expand = FALSE,
      clip = "off"
    ) +
    
    theme(
      text = element_text(
        family = "Inter",
        size = 30
      ),
      plot.margin = margin(0, 0, 0, 0),
      plot.background = element_rect(
        fill = NA,
        colour = NA
      )
    ) +
    
    theme_void()
}


#===================================================
# 6. ASSEMBLE & EXPORT
#===================================================


sysfonts::font_add_google("Plex","Plex")
font_add_google("IBM Plex Sans", "Plex")
font_add_google("Font Name", "Alias")
showtext::showtext_auto()
showtext_auto()

update_geom_defaults("text", list(
  family = "Inter"
))

update_geom_defaults("label", list(
  family = "Inter"
))

update_geom_defaults("richtext", list(
  family = "Inter"
))


final_df$logo<-c("Coventry","Ipswich","Millwall","Southampton","Middlesbrough","Hull")
cards <- lapply(seq_len(nrow(final_df)), function(i) make_card(final_df[i, ],final_df[i,]))
cards_grid <- wrap_plots(cards, ncol = 3, byrow = TRUE)


# -------------------------------------------------------
# UNIVERSAL LEGEND
# -------------------------------------------------------

state_legend <- ggplot() +
  
  geom_richtext(
    aes(
      x = 0.5,
      y = 0.5,
      label = paste0(
        "<span style='color:", col_green, "'>●</span> ",
        "&nbsp;&nbsp;&nbsp;&nbsp;",
        "<span style='color:", col_text_mid, "'>Leading/Wins</span>",
        "&nbsp;&nbsp;&nbsp;&nbsp;",
        "<span style='color:", col_grey_state, "'>●</span> ",
        "&nbsp;&nbsp;&nbsp;&nbsp;",
        "<span style='color:", col_text_mid, "'>Drawing/Draws</span>",
        "&nbsp;&nbsp;&nbsp;&nbsp;",
        "<span style='color:", col_red, "'>●</span> ",
                "&nbsp;&nbsp;&nbsp;&nbsp;",
        "<span style='color:", col_text_mid, "'>Trailing/Losses</span>"
      )
    ),
    hjust = 0.5,
    vjust = 0.5,
    size = 5,
    fill = NA,
    label.color = NA
  ) +
  
  coord_cartesian(
    xlim = c(0, 1),
    ylim = c(0, 1),
    expand = FALSE,
    clip = "off"
  ) +
  
  theme_void() +
  
  theme(
    plot.margin = margin(
      t = 2,
      r = 0,
      b = 2,
      l = 0
    )
  )


# -------------------------------------------------------
# CARDS + LEGEND
# -------------------------------------------------------

cards_with_legend <- plot_grid(
  cards_grid,
  state_legend,
  ncol = 1,
  
  # Give legend enough vertical room
  rel_heights = c(1, 0.06),
  
  align = "v"
)


# -------------------------------------------------------
# CARDS + CHART
# -------------------------------------------------------

final <- plot_grid(
  cards_with_legend,
  chart,
  
  ncol = 2,
  
  # More space for cards, but still enough for chart
  rel_widths = c(2, 1),
  
  align = "h",
  axis = "tb",
  greedy = FALSE
)

