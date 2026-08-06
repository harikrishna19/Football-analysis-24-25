
# Load required libraries for analysis ------------------------------------
library(tidyverse)
library(engsoccerdata)
library(cowplot)




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

sysfonts::font_add_google("Inter", "Inter")
sysfonts::font_add_google("Bebas Neue","Bebas")
sysfonts::font_add_google("Manrope","Manrope")
sysfonts::font_add_google("Oswald","Oswald")
showtext::showtext_auto()
# For logos
team_names<-c("Hull","Middlesbrough","Millwall","Southampton","Ipswich","Coventry")
plot_df[['team_name']]<-team_names

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
  ) +
  
  theme_minimal(base_size = 15) +
  
  theme(
    legend.position ="bottom",
    text = element_text(family = "Oswald"),
    plot.background = element_rect(
      fill = bg_col,
      colour = NA
    ),
    
    panel.background = element_rect(
      fill = bg_col,
      colour = NA
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

library(ggplot2)
library(ggimage)
library(patchwork)

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

sz_title    <- 9
sz_subtitle <- 5.5
sz_section  <- 3.2
sz_value    <- 3.3
sz_footer   <- 2.9

#===================================================
# 4. CARD
#===================================================

make_card <- function(t,t_names) {
  
  rank_colour <- if (t$Rank %in% c(1,2)) col_gold else if (t$Rank <= 3) col_accent else col_grey_rank
  tint_alpha  <- if (t$Rank == 1) 0.10 else if (t$Rank <= 3) 0.05 else 0
  
  card_title <- trunc_label(t$team, 16)
  initials   <- paste(substr(strsplit(t$team, " ")[[1]], 1, 1), collapse = "")
  
  pts_delta    <- t$Pts - avg_pts
  delta_label  <- paste0(ifelse(pts_delta >= 0, "+", ""), round(pts_delta, 0), " pts vs league avg")
  delta_colour <- if (pts_delta >= 0) col_green else col_red
  
  # Auto badge — first match wins
  badge_label <- ""; badge_colour <- col_accent
  # if (t$team == best_attack_team)        { badge_label <- "BEST ATTACK";        badge_colour <- col_green }
  # else if (t$team == best_defense_team)  { badge_label <- "BEST DEFENSE";       badge_colour <- col_accent }
  # else if (t$team == best_closer_team)   { badge_label <- "BEST CLOSERS";       badge_colour <- col_gold }
  # else if (t$team == best_comeback_team) { badge_label <- "BEST COMEBACK RATE"; badge_colour <- col_red }
  badge_layer <- if (nzchar(badge_label)) {
    annotate("label", x = 0.7, y = 12.15, hjust = 0, label = badge_label,
             fill = badge_colour, colour = "white", size = 2.3, fontface = "bold", label.size = 0)
  } else NULL
  
  # Goals diverging bar
  goal_scale <- 3.2 / max_goals
  gf_end <- 5 + t$gf * goal_scale
  ga_end <- 5 - t$ga * goal_scale
  gd_label  <- paste0(ifelse(t$gd >= 0, "+", ""), t$gd, " GD")
  gd_colour <- if (t$gd >= 0) col_green else col_red
  
  # --- Section C: half-time STATE DISTRIBUTION (stacked bar, ordered leading/drawing/trailing) ---
  seg_x0 <- 1.0; seg_x1 <- 9.0; total_w <- seg_x1 - seg_x0
  lw <- (t$leading_games  / t$GP) * total_w
  dw <- (t$drawing_games  / t$GP) * total_w
  tw <- (t$trailing_games / t$GP) * total_w
  l_x0 <- seg_x0;       l_x1 <- l_x0 + lw
  d_x0 <- l_x1;         d_x1 <- d_x0 + dw
  tr_x0 <- d_x1;        tr_x1 <- tr_x0 + tw
  
  # --- Section D: bullet charts — points won as % of max, by HT state ---
  bullet_x0 <- 3.0; bullet_x1 <- 9.0; bullet_w <- bullet_x1 - bullet_x0
  scale_pct <- function(pct) bullet_x0 + (pct / 100) * bullet_w
  lead_end  <- scale_pct(t$leading_pct_pts)
  draw_end  <- scale_pct(t$drawing_pct_pts)
  trail_end <- scale_pct(t$trailing_pct_pts)
  lead_avg_x  <- scale_pct(avg_leading_pct)
  draw_avg_x  <- scale_pct(avg_drawing_pct)
  trail_avg_x <- scale_pct(avg_trailing_pct)
  
  ggplot() +
    
    annotate("rect", xmin = 0.15, xmax = 10.15, ymin = -0.15, ymax = 13.85,
             fill = col_shadow, alpha = 0.35, colour = NA) +
    annotate("rect", xmin = 0, xmax = 10, ymin = 0, ymax = 14, fill = rank_colour, alpha = tint_alpha, colour = NA) +
    annotate("rect", xmin = 0, xmax = 10, ymin = 0, ymax = 14,
             fill = col_card_bg, colour = col_border, linewidth = 0.8) +
    annotate("rect", xmin = 0, xmax = 10, ymin = 13.5, ymax = 14, fill = rank_colour, colour = NA) +
    annotate("text", x = 0.4, y = 13.75, label = "Promoted to EPL 2026/27",
             hjust = 0, colour = "white", fontface = "bold", size = 4.2) +
    
    annotate("text", x = 3.2, y = 12.6, label = card_title,
             hjust = 0,
             vjust = 1.5, fontface = "bold", size = 5.4, colour = col_text_dark) +
    geom_soccer_logos(
      aes(
        x = 8.7,
        y = 10.6,
        team_name = t_names$logo
      ),
      width = 0.2
    ) +
    # logo_layer(8.7, 12.6, t$Logo, initials) +
    
    badge_layer +
    
    # ---- Uniform vertical rhythm from here down: every anchor is 0.8 apart,
    # so nothing can drift into its neighbour, and the generous card height
    # gets used instead of leaving dead space above the footer. ----
  annotate("text", x = 5, y = 11.50, size = 2.9, colour = col_text_mid,
           label = paste0("Record: ", t$W, "W-", t$D, "D-", t$L, "L")) +
    
    annotate("text", x = 5, y = 10.70, label = paste0(t$Pts, " PTS"),
             fontface = "bold", size = 9.0, colour = col_text_dark) +
    annotate("text", x = 5, y = 9.90, label = paste0(round(t$Pts / t$GP, 2), " pts / game"),
             colour = col_text_mid, size = 3.4) +
    annotate("text", x = 5, y = 9.10, label = delta_label, colour = delta_colour, size = 2.8, fontface = "bold") +
    annotate("segment", x = 0.6, xend = 9.4, y = 8.30, yend = 8.30, colour = col_border) +
    
    # C. Half-time state distribution
    annotate("text", x = 0.6, y = 7.50, hjust = 0, fontface = "bold", size = sz_section,
             colour = col_text_mid, label = "HALF-TIME STATE (GAMES PLAYED)") +
    annotate("rect", xmin = l_x0,  xmax = l_x1,  ymin = 6.50, ymax = 6.90, fill = col_green) +
    annotate("rect", xmin = d_x0,  xmax = d_x1,  ymin = 6.50, ymax = 6.90, fill = col_grey_state) +
    annotate("rect", xmin = tr_x0, xmax = tr_x1, ymin = 6.50, ymax = 6.90, fill = col_red) +
    annotate("text", x = 1.0, y = 5.90, hjust = 0, size = 2.6, colour = col_green,
             label = paste0("Leading: ", t$leading_games)) +
    annotate("text", x = 5.0, y = 5.90, hjust = 0.5, size = 2.6, colour = col_grey_state,
             label = paste0("Drawing: ", t$drawing_games)) +
    annotate("text", x = 9.0, y = 5.90, hjust = 1, size = 2.6, colour = col_red,
             label = paste0("Trailing: ", t$trailing_games)) +
    
    # D. Points won by HT state — bullet charts, dashed tick = league average
    annotate("text", x = 0.6, y = 5.10, hjust = 0, fontface = "bold", size = sz_section,
             colour = col_text_mid, label = "POINTS WON BY HT STATE (% OF MAX, vs league avg)") +
    
    annotate("text", x = 0.7, y = 4.30, hjust = 0, size = 2.9, colour = col_green, label = "Leading") +
    annotate("rect", xmin = bullet_x0, xmax = bullet_x1, ymin = 4.12, ymax = 4.48, fill = col_border) +
    annotate("rect", xmin = bullet_x0, xmax = lead_end, ymin = 4.12, ymax = 4.48, fill = col_green) +
    annotate("segment", x = lead_avg_x, xend = lead_avg_x, y = 4.02, yend = 4.58,
             colour = col_text_dark, linewidth = 0.6, linetype = "dashed") +
    annotate("text", x = lead_end + 0.15, y = 4.30, hjust = 0, size = sz_value, fontface = "bold",
             colour = col_green, label = paste0(t$leading_pct_pts, "%")) +
    
    annotate("text", x = 0.7, y = 3.50, hjust = 0, size = 2.9, colour = col_grey_state, label = "Drawing") +
    annotate("rect", xmin = bullet_x0, xmax = bullet_x1, ymin = 3.32, ymax = 3.68, fill = col_border) +
    annotate("rect", xmin = bullet_x0, xmax = draw_end, ymin = 3.32, ymax = 3.68, fill = col_grey_state) +
    annotate("segment", x = draw_avg_x, xend = draw_avg_x, y = 3.22, yend = 3.78,
             colour = col_text_dark, linewidth = 0.6, linetype = "dashed") +
    annotate("text", x = draw_end + 0.15, y = 3.50, hjust = 0, size = sz_value, fontface = "bold",
             colour = col_grey_state, label = paste0(t$drawing_pct_pts, "%")) +
    
    annotate("text", x = 0.7, y = 2.70, hjust = 0, size = 2.9, colour = col_red, label = "Trailing") +
    annotate("rect", xmin = bullet_x0, xmax = bullet_x1, ymin = 2.52, ymax = 2.88, fill = col_border) +
    annotate("rect", xmin = bullet_x0, xmax = trail_end, ymin = 2.52, ymax = 2.88, fill = col_red) +
    annotate("segment", x = trail_avg_x, xend = trail_avg_x, y = 2.42, yend = 2.98,
             colour = col_text_dark, linewidth = 0.6, linetype = "dashed") +
    annotate("text", x = trail_end + 0.15, y = 2.70, hjust = 0, size = sz_value, fontface = "bold",
             colour = col_red, label = paste0(t$trailing_pct_pts, "%")) +
    
    # E. Goals
    annotate("text", x = 0.6, y = 1.90, hjust = 0, fontface = "bold", size = sz_section,
             colour = col_text_mid, label = "GOALS (FOR / AGAINST)") +
    annotate("text", x = 9.4, y = 1.90, hjust = 1, fontface = "bold", size = sz_value,
             colour = gd_colour, label = gd_label) +
    annotate("rect", xmin = 5, xmax = gf_end, ymin = 1.10, ymax = 1.50, fill = col_green) +
    annotate("rect", xmin = ga_end, xmax = 5, ymin = 1.10, ymax = 1.50, fill = col_red) +
    annotate("segment", x = 5, xend = 5, y = 0.95, yend = 1.65, colour = col_grey_rank, linewidth = 0.6) +
    annotate("text", x = gf_end + 0.15, y = 1.30, hjust = 0, size = sz_value, fontface = "bold",
             colour = col_green, label = t$gf) +
    annotate("text", x = ga_end - 0.15, y = 1.30, hjust = 1, size = sz_value, fontface = "bold",
             colour = col_red, label = t$ga) +
    
    annotate("text", x = 5, y = 0.5, colour = col_text_light, size = sz_footer,
             label = paste0(t$GP, " games played")) +
    
    coord_cartesian(xlim = c(-0.3, 10.3), ylim = c(-0.3, 14.3), expand = FALSE, clip = "off") +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0), plot.background = element_rect(fill = NA, colour = NA))
}


#===================================================
# 6. ASSEMBLE & EXPORT
#===================================================
final_df$logo<-c("Coventry","Ipswich","Millwall","Southampton","Middlesbrough","Hull")
cards <- lapply(seq_len(nrow(final_df)), function(i) make_card(final_df[i, ],final_df[i,]))
cards_grid <- wrap_plots(cards, ncol = 3, byrow = TRUE)


final <- plot_grid(
  cards_grid,
  chart,
  ncol = 2,
  rel_widths = c(2,1),
  align = "h",
  greedy = FALSE
)

