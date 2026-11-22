library(engsoccerdata)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(magrittr)
library(patchwork)


# Get Points for each match week Unites,Tottenham & Chelsea


get_pl_matchweeks <- function(season = 2026,   # 2026 = the 2026/27 season
                             teams=NULL) {
  
  # 1. Try engsoccerdata (it uses the start year in Season, tier 1 = Premier League)
  raw <- NULL
  if (requireNamespace("engsoccerdata", quietly = TRUE)) {
    raw <- engsoccerdata::england_current(Season = season) %>% filter(tier==1) %>% 
      transmute(date = as.Date(Date), home, away = visitor,
                hg = hgoal, ag = vgoal)
    if(is.null(teams)){
      teams<-unique(raw$home)
    }
  }
  
  # 2. Fall back to football-data.co.uk if engsoccerdata has nothing for the season
  if (is.null(raw) || nrow(raw) == 0) {
    message("engsoccerdata has no ", season, "/", substr(season + 1, 3, 4),
            " data; fetching from football-data.co.uk instead.")
    code <- sprintf("%02d%02d", season %% 100, (season + 1) %% 100)
    url  <- sprintf("https://www.football-data.co.uk/mmz4281/%s/E0.csv", code)
    
    name_map <- c("Man United" = "Manchester United",
                  "Tottenham"  = "Tottenham Hotspur")
    
    raw <- read_csv(url, show_col_types = FALSE) %>%
      filter(!is.na(FTHG)) %>%                       # played matches only
      transmute(date = dmy(Date),
                home = recode(HomeTeam, !!!name_map),
                away = recode(AwayTeam, !!!name_map),
                hg = FTHG, ag = FTAG)
  }
  
  # 3. One row per team per match, numbered in date order (= that team's match week)
  bind_rows(
    raw %>% transmute(date, team = home, opponent = away,
                      venue = "H", gf = hg, ga = ag),
    raw %>% transmute(date, team = away, opponent = home,
                      venue = "A", gf = ag, ga = hg)
  ) %>%
    filter(team %in% teams) %>%
    arrange(team, date) %>%
    group_by(team) %>%
    mutate(matchweek = row_number(),
           result    = case_when(gf > ga ~ "W", gf < ga ~ "L", TRUE ~ "D"),
           points    = case_when(result == "W" ~ 3L, result == "D" ~ 1L, TRUE ~ 0L),
           cum_pts   = cumsum(points),
           gd        = cumsum(gf - ga)) %>%
    ungroup() %>%
    select(team, matchweek, date, opponent, venue, gf, ga, result, points, cum_pts, gd)
}

# Usage
pl <- get_pl_matchweeks(teams = c("Manchester United",
                                   "Tottenham Hotspur",
                                   "Chelsea"))
pl
team_cols  <- c(Chelsea = blue, "Manchester United" = red, "Tottenham Hotspur" = navy)
end_labels <- pl |> filter(matchweek == 10)
pts_prog<-pl


# ---- Panel 1 ------------------------------------------------
h1 <- panel_heading("1", "The Giants are Wobbling",
                    "Chelsea, Man United and Spurs are struggling for consistency\n- with Manchester United in relegation places already")

c1 <- ggplot(pl, aes(matchweek, cum_pts, colour = team)) +
  geom_line(linewidth = 1) +
  geom_point(data = end_labels, size = 3.6, show.legend = FALSE) +
  geom_label(data = end_labels, aes(label = cum_pts, fill = team), colour = "white",
             fontface = "bold", size = 3.4, label.size = 0, show.legend = FALSE,
             position = position_nudge(x = 0.55)) +
  scale_colour_manual(values = team_cols) + scale_fill_manual(values = team_cols) +
  scale_x_continuous(breaks = 1:10, labels = paste0("GW", 1:10),
                     expand = expansion(add = c(0.4, 1.3))) +
  scale_y_continuous(limits = c(0, 16), breaks = seq(0, 14, 2)) +
  labs(x = NULL, y = NULL) + theme_chart() +
  theme(axis.text.x = element_text(size = 7.5), panel.grid.major.x = element_blank())

cards <- tibble(team = c("Chelsea", "Manchester United", "Tottenham Hotspur"),
                ppg = sprintf("%.1f", c(end_labels$cum_pts / 10)),
                x = c(0.95, 2.95, 4.95)
                )
c1_cards <- ggplot(cards) +
  geom_rect(aes(xmin = x - 0.9, xmax = x + 0.9, ymin = 0.02, ymax = 0.98),
            fill = "#e6e2da", colour = "#cfc9be") +
  geom_text(aes(x, 0.83, label = team), fontface = "bold", colour = navy, size = 3.1) +
  geom_text(aes(x, 0.57, label = ppg), fontface = "bold", colour = navy, size = 6.5) +
  geom_text(aes(x, 0.34, label = "Pts/Game"), size = 2.6, colour = navy) +
  annotate("text", x = 6.5, y = 0.85, label = "Key Issue?", hjust = 0,
           fontface = "bold", size = 4.2, colour = navy) +
  annotate("text", x = 6.5, y = 0.40, hjust = 0, size = 2.9, lineheight = 1.05,
           label = "Defensive frailties\nand missed chances\nare hurting the big clubs.") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 1)) + theme_void(base_family = fam) + bg

panel1 <- h1 / tag_label("Points Progression") / c1 / c1_cards +
  plot_layout(heights = c(1.5, 0.5, 3.6, 1.3))

panel1


# code panel 2 Arsenal and brighton
pl_ <- get_pl_matchweeks()
pl_agg<-pl_ %>% group_by(team) %>% summarise("gf"=sum(gf),"ga"=sum(ga),gd=gf-ga,pts=sum(points))
pl_agg$hl <- ifelse(pl_agg$team %in% c("Arsenal", "Brighton & Hove Albion"), pl_agg$team, "Other")


# ---- Panel 2 ------------------------------------------------
h2 <- panel_heading("2", "Brighton: Genuine Contenders?",
                    "Brighton are second in the table, with a remarkable +19 goal difference.\nTheir underlying numbers suggest this isn't a fluke.")

c2 <- ggplot(pl_agg, aes(gf, ga)) +
  annotate("rect", xmin = med_gf, xmax = Inf, ymin = -Inf, ymax = med_ga,
           fill = "#C5D0EE", alpha = .4) +
  geom_vline(xintercept = med_gf, linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept = med_ga, linetype = "dashed", colour = "grey40") +
  geom_point(aes(size = pts, fill = hl), shape = 21, colour = "white", stroke = 1) +
  geom_label_repel(data = filter(pl_agg, hl != "Other"),
                   aes(label = team, fill = hl), colour = "white", fontface = "bold") +
  scale_y_reverse() +
  scale_fill_manual(values = c(hl_cols, other = "grey65"), guide = "none") +
  scale_size_continuous(range = c(3, 10), name = "Points") +
  labs(x = "Goals scored (better attack →)", y = "Goals conceded (↑ better defence)")+
  theme_chart() + theme(legend.position = "none")

side2 <- stat_block(max(pl_agg$gf), "Goals Scored", "(most in PL)") /
  stat_block(min(pl_agg$ga), "Goals Conceded", "(2nd best)") /
  stat_block(max(pl_agg$gd), "Goal Difference", "(+2.5 above xG)") /
  callout("Are they the real deal?\nTheir numbers say yes.") +
   plot_layout(heights = c(1, 1, 1, 1.1))
panel2 <- h2 / tag_label("Contender Credibility Matrix") / (c2 + side2 + plot_layout(widths = c(2.6, 1.4))) +
  plot_layout(heights = c(1.5, 0.5, 5.0))


library(tidyverse)

# 1. Illustrative data from our previous example
df <- tribble(
  ~team, ~ht_points, ~ft_points,
  "Arsenal",          16, 14,
  "Aston Villa",      15, 18,
  "Man City",         14, 13,
  "Bournemouth",      13, 20,
  "Brighton",         12, 19,
  "Chelsea",          11, 12,
  "Liverpool",        10, 11,
  "Brentford",         9, 17,
  "Fulham",            9,  7,
  "Man United",        8,  9,
  "Crystal Palace",    8,  6,
  "Tottenham",         7,  8,
  "Newcastle",         6, 10,
  "West Ham",          6,  4,
  "Everton",           5,  5,
  "Wolves",            4,  3,
  "Leeds",             3,  2,
  "Burnley",           2,  1,
  "Sunderland",        1,  0,
  "Nottingham Forest", 0,  0
)

# 2. Calculate second-half impact
top_four <- c(
  "Bournemouth", "Brighton",
  "Aston Villa", "Brentford"
)

df <- df %>%
  mutate(
    impact = ft_points - ht_points,
    
    group = case_when(
      team %in% top_four ~ team,
      impact > 0 ~ "Other positive",
      impact < 0 ~ "Other negative",
      TRUE ~ "No change"
    ),
    
    impact_label = sprintf("%+d", impact),
    
    team = fct_reorder(team, impact)
  )

# 3. Club colour palette
club_cols <- c(
  "Bournemouth" = "#C8102E",
  "Brighton" = "#0057B8",
  "Aston Villa" = "#670E36",
  "Brentford" = "#D71920",
  "Other positive" = "#328B78",
  "Other negative" = "#CE7770",
  "No change" = "#A0A0A0"
)

# 4. Plot
panel3<-ggplot(df, aes(y = team)) +
  
  # Positive impact background
  annotate(
    "rect",
    xmin = 0, xmax = 9,
    ymin = -Inf, ymax = Inf,
    fill = "#E7F0EA",
    alpha = 0.45
  ) +
  
  # Negative impact background
  annotate(
    "rect",
    xmin = -3, xmax = 0,
    ymin = -Inf, ymax = Inf,
    fill = "#F5E9E6",
    alpha = 0.45
  ) +
  
  # Zero reference
  geom_vline(
    xintercept = 0,
    colour = "#333333",
    linewidth = 0.7
  ) +
  
  # Lollipop stems
  geom_segment(
    aes(
      x = 0,
      xend = impact,
      yend = team,
      colour = group
    ),
    linewidth = 1.4,
    lineend = "round"
  ) +
  
  # Impact points
  geom_point(
    aes(
      x = impact,
      colour = group
    ),
    size = 4
  ) +
  
  # Values at endpoints
  geom_text(
    aes(
      x = impact,
      label = impact_label,
      hjust = if_else(impact >= 0, -0.65, 1.65),
      colour = group
    ),
    fontface = "bold",
    size = 3.5
  ) +
  
  scale_colour_manual(values = club_cols) +
  
  scale_x_continuous(
    breaks = seq(-2, 8, 2),
    limits = c(-3, 10),
    expand = expansion(mult = 0)
  ) +
  
  labs(
    title = "WHO OWNS THE SECOND HALF?",
    subtitle = paste(
      "Net change in league points",
      "between half-time and full-time"
    ),
    x = "Net points swing",
    y = NULL,
    caption = paste(
      "Illustrative data | Positive = gained table points",
      "| Negative = lost table points"
    )
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.background = element_rect(
      fill = "#F8F7F3",
      colour = NA
    ),
    
    panel.background = element_rect(
      fill = "#F8F7F3",
      colour = NA
    ),
    
    panel.grid = element_blank(),
    
    axis.text.y = element_text(
      size = 10,
      colour = "#292929",
      face = "bold"
    ),
    
    axis.text.x = element_text(
      colour = "#555555"
    ),
    
    axis.title.x = element_text(
      face = "bold",
      margin = margin(t = 12)
    ),
    
    plot.title = element_text(
      size = 23,
      face = "bold",
      colour = "#171717",
      margin = margin(b = 8)
    ),
    
    plot.subtitle = element_text(
      size = 11,
      colour = "#666666"
    ),
    
    plot.caption = element_text(
      size = 9,
      colour = "#888888"
    ),
    
    legend.position = "none",
    
    plot.margin = margin(20, 35, 20, 20)
  ) +
  
  coord_cartesian(clip = "off")









library(ggplot2)
library(scales)

NAVY <- "#14112e"; RED <- "#d0101b"; BLUE <- "#0a44a0"
BG   <- "#f4f1ec"; GRID <- "#dcd8d0"; GREY <- "#6b6b6b"

# ---- 1. P(new run of 3 straight losses within n remaining games) ----------
# State machine: "old" = still inside the current losing run (does not count),
# f0/f1/f2 = 0/1/2 fresh consecutive losses. Reaching 3 fresh losses = success.
p_new <- function(p, n = 28) {
  old <- 1; f0 <- 0; f1 <- 0; f2 <- 0; succ <- 0
  for (i in seq_len(n)) {
    q    <- 1 - p
    succ <- succ + f2 * p
    o <- old * p
    a0 <- (old + f0 + f1 + f2) * q
    a1 <- f0 * p
    a2 <- f1 * p
    old <- o; f0 <- a0; f1 <- a1; f2 <- a2
  }
  succ
}
p_new_v <- Vectorize(p_new, "p")

# ---- 2. Loss-rate uncertainty: Beta prior updated with 3 losses in 10 -----
a <- 4.5 + 3          # prior Beta(4.5, 25.5), mean 15%
b <- 25.5 + 7         # 3 losses, 7 non-losses so far
ps <- seq(0.0005, 0.9995, length.out = 3000)
w  <- dbeta(ps, a, b); w <- w / sum(w)
headline <- sum(w * p_new_v(ps))          # ~0.149
band     <- qbeta(c(0.1, 0.9), a, b)      # 80% credible range
cat(sprintf("P(again) = %.1f%%, loss-rate mean = %.1f%%\n",
            100 * headline, 100 * a / (a + b)))

# ---- 3. Data for plot ------------------------------------------------------
curve_df <- data.frame(p = seq(0, 0.5, length.out = 300))
curve_df$prob <- p_new_v(curve_df$p)

pts <- data.frame(
  p     = c(0.10, 0.15, 0.30),
  label = c("2% if loss rate is 10%", "7% if loss rate is 15%",
            "40% if loss rate is 30%\n(this season's 3 in 10)"),
  lx    = c(0.012, 0.17, 0.335),
  ly    = c(0.20, 0.30, 0.22)
)
pts$prob <- p_new_v(pts$p)

# ---- 4. Plot ---------------------------------------------------------------
g <- ggplot() +
  annotate("rect", xmin = band[1], xmax = band[2], ymin = 0, ymax = 0.62,
           fill = "#c9cfe6", alpha = 0.6) +
  annotate("text", x = mean(band), y = 0.55, colour = BLUE, fontface = "italic",
           size = 3.4, lineheight = 0.95,
           label = "80% credible range\nfor City's loss rate") +
  geom_line(data = curve_df, aes(p, prob), colour = NAVY, linewidth = 1.3) +
  geom_segment(data = pts, aes(x = p, y = prob, xend = lx, yend = ly),
               colour = "#888888", linewidth = 0.4) +
  geom_point(data = pts, aes(p, prob), colour = RED, size = 3.5) +
  geom_text(data = pts, aes(lx, ly, label = label), hjust = 0, colour = NAVY,
            size = 3.4, lineheight = 0.95) +
  annotate("text", x = 0.01, y = 0.84, label = percent(headline, accuracy = 1),
           colour = RED, fontface = "bold", size = 16, hjust = 0) +
  annotate("text", x = 0.115, y = 0.88, hjust = 0, colour = NAVY, fontface = "bold",
           size = 4, label = "best estimate that it happens again") +
  annotate("text", x = 0.115, y = 0.80, hjust = 0, colour = GREY, size = 3.3,
           label = sprintf("averaged over loss-rate uncertainty (mean %s)",
                           percent(a / (a + b), accuracy = 1))) +
  scale_x_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1),
                     expand = c(0, 0)) +
  labs(
    title    = "Will City lose three in a row again?",
    subtitle = paste0("City won their first 7, then lost at Villa, Brighton and Forest (GW8-10).\n",
                      "Chance of a new run of 3+ straight league defeats in the remaining 28 games."),
    x = "City's per-game loss probability",
    y = "P(new 3-game losing run)",
    caption = paste0("Assumes independent league games, 28 remaining; prior Beta(4.5, 25.5) on loss rate ",
                     "updated with 3 losses in 10.\n'New run' = 3 straight losses after at least one non-loss.")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = BG, colour = NA),
    panel.background = element_rect(fill = BG, colour = NA),
    panel.grid.major = element_line(colour = GRID, linewidth = 0.4),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold.italic", colour = NAVY, size = 17),
    plot.subtitle    = element_text(colour = "#333333", size = 10.5, lineheight = 1.2),
    plot.caption     = element_text(colour = GREY, size = 8, hjust = 0),
    axis.title       = element_text(colour = NAVY),
    axis.text        = element_text(colour = NAVY),
    plot.margin      = margin(12, 16, 10, 12)
  )

print(g)
# ggsave("panel4_streak.png", g, width = 8, height = 5.5, dpi = 200)

# ---- 5. To combine with your other 3 ggplots in one shot -------------------
# library(patchwork)
# (p1 | p2) / (p3 | g) + plot_annotation(theme = theme(plot.background = element_rect(fill = BG, colour = NA)))


(panel1| panel2) / (panel3 | g) + plot_annotation(theme = theme(plot.background = element_rect(fill = BG, colour = NA)))

