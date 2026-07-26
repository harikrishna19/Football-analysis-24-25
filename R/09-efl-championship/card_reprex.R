library(ggplot2)
library(ggimage)
library(patchwork)

#===================================================
# BRANDING / PALETTE — edit these to re-theme everything
#===================================================

col_bg_dark    <- "#0B1220"   # header background
col_card_bg    <- "#FFFFFF"   # card background
col_border     <- "#E2E8F0"   # card border / track colour
col_accent     <- "#2563EB"   # primary accent (FT points, bars)
col_accent_lt  <- "#93C5FD"   # subtitle / secondary accent on dark bg
col_text_dark  <- "#0F172A"   # primary text on light bg
col_text_mid   <- "#475569"   # secondary text on light bg
col_text_light <- "#94A3B8"   # tertiary / footer text
col_green      <- "#059669"   # positive stat (goals for, comebacks)
col_red        <- "#DC2626"   # negative stat (goals against, leads lost)
col_gold       <- "#D97706"   # rank #1 accent
col_grey_rank  <- "#64748B"   # rank 4-6 accent

# Typography scale (ggplot `size` units)
sz_title    <- 9
sz_subtitle <- 5.5
sz_section  <- 3.2   # section labels within a card
sz_value    <- 3.3   # numeric callouts
sz_footer   <- 2.9

#===================================================
# Sample / illustrative data — swap in your real numbers + logo URLs
#===================================================

teams_data <- data.frame(
  Team      = c("Leeds United", "Burnley", "Sheffield Utd", "Sunderland", "West Brom", "Hull City"),
  Pts       = c(100, 92, 88, 84, 79, 75),
  GF        = c(84, 70, 65, 60, 58, 55),
  GA        = c(30, 35, 38, 40, 45, 48),
  HT_Pts    = c(45, 40, 38, 35, 33, 30),
  FT_Pts    = c(100, 92, 88, 84, 79, 75),
  Comebacks = c(8, 6, 5, 7, 4, 3),
  LeadsLost = c(3, 4, 5, 3, 6, 5),
  # TODO: replace with each club's actual crest URL
  Logo      = "https://upload.wikimedia.org/wikipedia/en/5/54/Leeds_United_F.C._logo.svg",
  stringsAsFactors = FALSE
)
teams_data$Rank <- rank(-teams_data$Pts, ties.method = "first")

# FIX: HT and FT points must share ONE scale (HT is a subset of the season,
# not an independent stat) — previously they used different maxima, which
# made the dumbbell distances visually meaningless.
max_pts_scale <- max(teams_data$FT_Pts)
max_goals     <- max(teams_data$GF, teams_data$GA)
max_cb_ll     <- max(teams_data$Comebacks, teams_data$LeadsLost)

# Truncate long labels so they never collide with neighbouring elements
trunc_label <- function(x, n = 14) ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1), "\u2026"), x)

#===================================================
# HEADER — insights (left) + standings bar chart (right)
#===================================================

standings <- teams_data[order(-teams_data$Pts), ]
standings$y      <- seq(4.35, 0.95, length.out = nrow(standings))
standings$ymin   <- standings$y - 0.26
standings$ymax   <- standings$y + 0.26
standings$xmin   <- 6.9
standings$xmax   <- 6.9 + (standings$Pts / max(standings$Pts)) * 4.3
standings$Label  <- trunc_label(standings$Team, 13)

header <- ggplot() +
  
  annotate("rect", xmin = 0, xmax = 12, ymin = 0, ymax = 6, fill = col_bg_dark, colour = NA) +
  
  annotate("text", x = 0.6, y = 5.35, label = "EFL CHAMPIONSHIP 2025/26",
           hjust = 0, colour = "white", fontface = "bold", size = sz_title) +
  annotate("text", x = 0.6, y = 4.75, label = "Season Summary",
           hjust = 0, colour = col_accent_lt, fontface = "bold", size = sz_subtitle) +
  annotate("segment", x = 0.6, xend = 4.6, y = 4.45, yend = 4.45,
           linewidth = 1.3, colour = col_accent) +
  
  annotate("text", x = 0.8, y = 3.75, hjust = 0, colour = "white", size = 4.0,
           label = "\u2022 24 clubs competed across 46 league matches.") +
  annotate("text", x = 0.8, y = 3.10, hjust = 0, colour = "white", size = 4.0,
           label = "\u2022 Automatic promotion and play-offs decided after an intense campaign.") +
  annotate("text", x = 0.8, y = 2.45, hjust = 0, colour = "white", size = 4.0,
           label = "\u2022 Second-half performances and comebacks separated the top six.") +
  annotate("text", x = 0.8, y = 1.80, hjust = 0, colour = "white", size = 4.0,
           label = "\u2022 Explore points splits, goals and comeback stats for every team below.") +
  annotate("text", x = 0.6, y = 0.55, label = "Data Source: SoccerData | Visualisation: ggplot2",
           hjust = 0, colour = col_text_light, size = 3.4) +
  
  annotate("segment", x = 6.0, xend = 6.0, y = 0.4, yend = 5.5, colour = "#1E293B", linewidth = 0.8) +
  
  annotate("text", x = 6.4, y = 5.15, label = "FINAL STANDINGS \u2014 TOP 6",
           hjust = 0, colour = col_accent_lt, fontface = "bold", size = 3.5) +
  geom_rect(data = standings, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = col_accent) +
  geom_text(data = standings, aes(x = 6.75, y = y, label = Label),
            hjust = 1, colour = "white", size = 2.85) +
  geom_text(data = standings, aes(x = xmax + 0.15, y = y, label = Pts),
            hjust = 0, colour = "white", size = 2.85, fontface = "bold") +
  
  coord_cartesian(xlim = c(0, 12), ylim = c(0, 6), expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = col_bg_dark, colour = NA),
    plot.margin = margin(0, 0, 0, 0)
  )

#===================================================
# CARD — dumbbell, diverging goals bar, comeback/leads-lost bars
#===================================================

make_card <- function(team) {
  
  scale_x <- function(val, max_val, x0 = 1.2, x1 = 8.8) x0 + (val / max_val) * (x1 - x0)
  
  # both on the SAME scale now — the gap between the two dots is meaningful
  ht_x <- scale_x(team$HT_Pts, max_pts_scale)
  ft_x <- scale_x(team$FT_Pts, max_pts_scale)
  
  goal_scale <- 3.2 / max_goals
  gf_end <- 5 + team$GF * goal_scale
  ga_end <- 5 - team$GA * goal_scale
  
  cb_end <- 2.3 + (team$Comebacks / max_cb_ll) * 5.7
  ll_end <- 2.3 + (team$LeadsLost / max_cb_ll) * 5.7
  
  rank_colour <- if (team$Rank == 1) col_gold else if (team$Rank <= 3) col_accent else col_grey_rank
  
  logo <- data.frame(x = 8.7, y = 12.6, image = team$Logo)
  card_title <- trunc_label(team$Team, 16)
  
  ggplot() +
    
    # Shell + rank accent
    annotate("rect", xmin = 0, xmax = 10, ymin = 0, ymax = 14,
             fill = col_card_bg, colour = col_border, linewidth = 0.8) +
    annotate("rect", xmin = 0, xmax = 10, ymin = 13.5, ymax = 14, fill = rank_colour, colour = NA) +
    annotate("text", x = 0.4, y = 13.75, label = paste0("#", team$Rank),
             hjust = 0, colour = "white", fontface = "bold", size = 4.2) +
    
    annotate("text", x = 0.7, y = 12.6, label = card_title,
             hjust = 0, fontface = "bold", size = 5.4, colour = col_text_dark) +
    geom_image(data = logo, aes(x, y, image = image), size = 0.075) +
    
    annotate("text", x = 5, y = 11.0, label = paste0(team$Pts, " PTS"),
             fontface = "bold", size = 9.0, colour = col_text_dark) +
    annotate("text", x = 5, y = 10.35, label = paste0(round(team$Pts / 46, 2), " pts / game"),
             colour = col_text_mid, size = 3.4) +
    annotate("segment", x = 0.6, xend = 9.4, y = 9.75, yend = 9.75, colour = col_border) +
    
    # 1. Points split — dumbbell (labels BELOW the line only, no title collision)
    annotate("text", x = 0.6, y = 9.35, hjust = 0, fontface = "bold", size = sz_section,
             colour = col_text_mid, label = "POINTS SPLIT (HT \u2192 FT)") +
    annotate("segment", x = 1.2, xend = 8.8, y = 8.55, yend = 8.55,
             colour = col_border, linewidth = 2.2, lineend = "round") +
    annotate("segment", x = ht_x, xend = ft_x, y = 8.55, yend = 8.55,
             colour = col_grey_rank, linewidth = 1.2) +
    annotate("point", x = ht_x, y = 8.55, size = 3.8, colour = col_grey_rank) +
    annotate("point", x = ft_x, y = 8.55, size = 4.4, colour = col_accent) +
    annotate("text", x = ht_x, y = 8.05, size = 2.9, colour = col_grey_rank, label = team$HT_Pts) +
    annotate("text", x = ft_x, y = 8.05, size = 3.0, colour = col_accent, fontface = "bold",
             label = team$FT_Pts) +
    
    # 2. Goals — diverging bar
    annotate("text", x = 0.6, y = 7.35, hjust = 0, fontface = "bold", size = sz_section,
             colour = col_text_mid, label = "GOALS (FOR / AGAINST)") +
    annotate("rect", xmin = 5, xmax = gf_end, ymin = 6.65, ymax = 7.05, fill = col_green) +
    annotate("rect", xmin = ga_end, xmax = 5, ymin = 6.65, ymax = 7.05, fill = col_red) +
    annotate("segment", x = 5, xend = 5, y = 6.5, yend = 7.2, colour = col_grey_rank, linewidth = 0.6) +
    annotate("text", x = gf_end + 0.15, y = 6.85, hjust = 0, size = sz_value, fontface = "bold",
             colour = col_green, label = team$GF) +
    annotate("text", x = ga_end - 0.15, y = 6.85, hjust = 1, size = sz_value, fontface = "bold",
             colour = col_red, label = team$GA) +
    
    # 3. Second-half swings — labels sit ABOVE each bar, not squeezed beside it
    annotate("text", x = 0.6, y = 5.95, hjust = 0, fontface = "bold", size = sz_section,
             colour = col_text_mid, label = "SECOND-HALF SWINGS") +
    annotate("text", x = 0.7, y = 5.72, hjust = 0, size = 2.8, colour = col_green, label = "Comebacks") +
    annotate("rect", xmin = 2.3, xmax = cb_end, ymin = 5.25, ymax = 5.6, fill = col_green) +
    annotate("text", x = cb_end + 0.15, y = 5.42, hjust = 0, size = sz_value, fontface = "bold",
             colour = col_green, label = team$Comebacks) +
    annotate("text", x = 0.7, y = 4.92, hjust = 0, size = 2.8, colour = col_red, label = "Leads Lost") +
    annotate("rect", xmin = 2.3, xmax = ll_end, ymin = 4.45, ymax = 4.8, fill = col_red) +
    annotate("text", x = ll_end + 0.15, y = 4.62, hjust = 0, size = sz_value, fontface = "bold",
             colour = col_red, label = team$LeadsLost) +
    
    annotate("text", x = 5, y = 0.5, colour = col_text_light, size = sz_footer,
             label = "46 games played") +
    
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 14), expand = FALSE, clip = "off") +
    theme_void() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      plot.background = element_rect(fill = col_card_bg, colour = NA)
    )
}

#===================================================
# ASSEMBLE
#===================================================

cards <- lapply(seq_len(nrow(teams_data)), function(i) make_card(teams_data[i, ]))
cards_grid <- wrap_plots(cards, ncol = 3, byrow = TRUE)

final_plot <- (header / cards_grid) +
  plot_layout(heights = c(0.55, 2)) &
  theme(plot.margin = margin(1, 1, 1, 1))

final_plot

ggsave(
  "dashboard.png",
  final_plot,
  width = 18,
  height = 11,
  dpi = 300,
  bg = "white"
)