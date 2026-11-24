# =====================================================================
#  Premier League 2026/27 - four-panel infographic (publication version)
#  Output: pl_infographic.png (300 dpi) + pl_infographic.pdf (vector)
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(ggplot2)
  library(ggrepel)
  library(patchwork)
  library(scales)
  library(forcats)
  library(tibble)
})

# ---------------------------------------------------------------------
# 0. Global design system  (one place to change fonts / sizes / colours)
# ---------------------------------------------------------------------
# Font: Inter if installed, otherwise the system sans.
# For guaranteed portability: install Inter, or set fam <- "Helvetica".
fam <- if ("Inter" %in% systemfonts::system_fonts()$family) "Inter" else "sans"

NAVY <- "#14112e"; RED <- "#d0101b"; BLUE <- "#0a44a0"
BG   <- "#f4f1ec"; GRID <- "#dcd8d0"; GREY <- "#5f5f5f"; CARD <- "#e9e5dc"
blue <- BLUE; red <- RED; navy <- NAVY

# Type scale in POINTS (final figure is 18 x 13 in, so these are true print sizes)
SZ <- list(title = 26, h = 19, sub = 11.5, tag = 11, axis = 11,
           label = 11, stat = 30, cap = 9.5)
mm <- function(pt) pt / .pt          # points -> ggplot "size" (mm)

bg <- theme(plot.background = element_rect(fill = BG, colour = NA))
spacer <- function() ggplot() + theme_void() + bg
full   <- function(p) wrap_elements(full = p)

theme_chart <- function() {
  theme_minimal(base_size = SZ$axis, base_family = fam) +
    theme(
      plot.background   = element_rect(fill = BG, colour = NA),
      panel.background  = element_rect(fill = BG, colour = NA),
      panel.grid.major  = element_line(colour = GRID, linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      axis.text         = element_text(colour = NAVY, size = SZ$axis),
      axis.title        = element_text(colour = NAVY, size = SZ$axis + 0.5,
                                       face = "bold"),
      axis.title.x      = element_text(margin = margin(t = 8)),
      axis.title.y      = element_text(margin = margin(r = 8)),
      legend.position   = "none",
      plot.margin       = margin(4, 14, 4, 6)
    )
}

# ---- reusable layout pieces -----------------------------------------
panel_heading <- function(num, title, sub) {
  ggplot() +
    annotate("point", x = 0.035, y = 0.66, size = 15, colour = NAVY) +
    annotate("text",  x = 0.035, y = 0.66, label = num, colour = "white",
             fontface = "bold", size = mm(18), family = fam) +
    annotate("text",  x = 0.08, y = 0.76, label = title, hjust = 0, vjust = 0.5,
             fontface = "bold", size = mm(SZ$h), colour = NAVY, family = fam) +
    annotate("text",  x = 0.08, y = 0.50, label = sub, hjust = 0, vjust = 1,
             size = mm(SZ$sub), colour = "#333333", lineheight = 1.1, family = fam) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_void() + bg
}

tag_label <- function(txt) {
  ggplot() +
    annotate("text", x = 0, y = 0.55, label = toupper(txt), hjust = 0,
             fontface = "bold", size = mm(SZ$tag), colour = BLUE, family = fam) +
    annotate("segment", x = 0, xend = 1, y = 0.08, yend = 0.08,
             colour = NAVY, linewidth = 0.5) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_void() + bg
}

stat_block <- function(value, label, sub) {
  ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0.04, ymax = 0.96,
             fill = CARD, colour = "#cfc9be") +
    annotate("text", x = 0.5, y = 0.74, label = label, fontface = "bold",
             size = mm(SZ$label), colour = NAVY, family = fam) +
    annotate("text", x = 0.5, y = 0.43, label = value, fontface = "bold",
             size = mm(SZ$stat), colour = BLUE, family = fam) +
    annotate("text", x = 0.5, y = 0.15, label = sub,
             size = mm(SZ$label - 1.5), colour = GREY, family = fam) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_void() + bg
}

callout <- function(txt) {
  ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0.04, ymax = 0.96, fill = NAVY) +
    annotate("text", x = 0.5, y = 0.5, label = txt, colour = "white",
             fontface = "bold", size = mm(SZ$label + 0.5), lineheight = 1.15,
             family = fam) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_void() + bg
}

# ---------------------------------------------------------------------
# 1. Data: one row per team per match, numbered in date order
# ---------------------------------------------------------------------
get_pl_matchweeks <- function(season = 2026,      # 2026 = the 2026/27 season
                              teams  = NULL) {
  
  # (a) engsoccerdata (start year in Season, tier 1 = Premier League)
  raw <- NULL
  if (requireNamespace("engsoccerdata", quietly = TRUE)) {
    raw <- engsoccerdata::england_current(Season = season) %>%
      filter(tier == 1) %>%
      transmute(date = as.Date(Date), home, away = visitor,
                hg = hgoal, ag = vgoal)
  }
  
  # (b) fall back to football-data.co.uk
  if (is.null(raw) || nrow(raw) == 0) {
    message("engsoccerdata has no ", season, "/", substr(season + 1, 3, 4),
            " data; fetching from football-data.co.uk instead.")
    code <- sprintf("%02d%02d", season %% 100, (season + 1) %% 100)
    url  <- getOption("pl_csv",
                      sprintf("https://www.football-data.co.uk/mmz4281/%s/E0.csv", code))
    
    name_map <- c("Man United"    = "Manchester United",
                  "Man City"      = "Manchester City",
                  "Tottenham"     = "Tottenham Hotspur",
                  "Brighton"      = "Brighton & Hove Albion",
                  "Newcastle"     = "Newcastle United",
                  "West Ham"      = "West Ham United",
                  "Wolves"        = "Wolverhampton Wanderers",
                  "Leeds"         = "Leeds United",
                  "Nott'm Forest" = "Nottingham Forest")
    
    raw <- read_csv(url, show_col_types = FALSE) %>%
      filter(!is.na(FTHG)) %>%                       # played matches only
      transmute(date = dmy(Date),
                home = recode(HomeTeam, !!!name_map),
                away = recode(AwayTeam, !!!name_map),
                hg = FTHG, ag = FTAG)
  }
  
  if (is.null(teams)) teams <- unique(raw$home)
  
  bind_rows(
    raw %>% transmute(date, team = home, opponent = away, venue = "H", gf = hg, ga = ag),
    raw %>% transmute(date, team = away, opponent = home, venue = "A", gf = ag, ga = hg)
  ) %>%
    filter(team %in% teams) %>%
    arrange(team, date) %>%
    group_by(team) %>%
    mutate(matchweek = row_number(),   # a team's n-th league match
           result  = case_when(gf > ga ~ "W", gf < ga ~ "L", TRUE ~ "D"),
           points  = case_when(result == "W" ~ 3L, result == "D" ~ 1L, TRUE ~ 0L),
           cum_pts = cumsum(points),
           gd      = cumsum(gf - ga)) %>%
    ungroup() %>%
    select(team, matchweek, date, opponent, venue, gf, ga, result, points, cum_pts, gd)
}

pl_all <- get_pl_matchweeks()
GW     <- max(pl_all$matchweek[pl_all$team == "Chelsea"])   # gameweeks played
pl     <- pl_all %>% filter(team %in% c("Manchester United", "Tottenham Hotspur", "Chelsea"))

# ---------------------------------------------------------------------
# 2. Panel 1 - points progression
# ---------------------------------------------------------------------
team_cols  <- c(Chelsea = BLUE, "Manchester United" = RED, "Tottenham Hotspur" = NAVY)
end_labels <- pl %>% filter(matchweek == GW)

h1 <- panel_heading("1", "The Giants are Wobbling",
                    "Chelsea, Man United and Spurs are struggling for consistency,\nwith Manchester United already in the relegation places.")

c1 <- ggplot(pl, aes(matchweek, cum_pts, colour = team)) +
  geom_line(linewidth = 1.4, lineend = "round") +
  geom_point(data = end_labels, size = 4.2, show.legend = FALSE) +
  geom_label(data = end_labels, aes(label = cum_pts, fill = team), colour = "white",
             fontface = "bold", size = mm(SZ$label + 1), family = fam,
             label.size = 0, show.legend = FALSE,
             label.padding = unit(0.25, "lines"),
             position = position_nudge(x = 0.6)) +
  scale_colour_manual(values = team_cols, name = NULL) +
  scale_fill_manual(values = team_cols) +
  scale_x_continuous(breaks = 1:GW, labels = paste0("GW", 1:GW),
                     expand = expansion(add = c(0.4, 1.4))) +
  scale_y_continuous(limits = c(0, NA), breaks = pretty_breaks(7),
                     expand = expansion(mult = c(0.02, 0.08))) +
  labs(x = NULL, y = "Cumulative points") +
  theme_chart() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "top", legend.justification = "left",
        legend.text = element_text(size = SZ$axis + 0.5, colour = NAVY),
        legend.key.width = unit(1.6, "lines"),
        legend.margin = margin(0, 0, 4, 0))

cards <- end_labels %>%
  arrange(factor(team, levels = names(team_cols))) %>%
  transmute(team, ppg = sprintf("%.2f", cum_pts / GW)) %>%
  mutate(x = c(1.65, 4.95, 8.25))

c1_cards <- ggplot(cards) +
  geom_rect(aes(xmin = x - 1.5, xmax = x + 1.5, ymin = 0.06, ymax = 0.94),
            fill = CARD, colour = "#cfc9be") +
  geom_text(aes(x, 0.80, label = team), fontface = "bold", colour = NAVY,
            size = mm(SZ$label - 0.5), family = fam) +
  geom_text(aes(x, 0.50, label = ppg), fontface = "bold", colour = NAVY,
            size = mm(SZ$stat - 4), family = fam) +
  geom_text(aes(x, 0.20, label = "points per game"), size = mm(SZ$label - 1.5),
            colour = GREY, family = fam) +
  annotate("text", x = 10.2, y = 0.84, label = "Key issue?", hjust = 0,
           fontface = "bold", size = mm(SZ$label + 2), colour = NAVY, family = fam) +
  annotate("text", x = 10.2, y = 0.62, hjust = 0, vjust = 1, lineheight = 1.15,
           size = mm(SZ$label), colour = "#333333", family = fam,
           label = "Defensive frailties and\nmissed chances are\nhurting the big clubs.") +
  coord_cartesian(xlim = c(0, 14), ylim = c(0, 1), expand = FALSE) +
  theme_void() + bg

panel1 <- full(h1) / full(tag_label("Points progression")) / c1 / full(c1_cards) +
  plot_layout(heights = c(1.45, 0.35, 4.9, 1.35))

# ---------------------------------------------------------------------
# 3. Panel 2 - Brighton credibility matrix
# ---------------------------------------------------------------------
pl_agg <- pl_all %>%
  group_by(team) %>%
  summarise(gf = sum(gf), ga = sum(ga), gd = gf - ga, pts = sum(points), .groups = "drop") %>%
  mutate(hl = ifelse(team %in% c("Arsenal", "Brighton & Hove Albion"), team, "Other"),
         rank_pts = min_rank(-pts))
med_gf <- median(pl_agg$gf); med_ga <- median(pl_agg$ga)
hl_cols <- c("Arsenal" = RED, "Brighton & Hove Albion" = BLUE, "Other" = "grey65")
bri <- filter(pl_agg, team == "Brighton & Hove Albion")
ord <- function(n) paste0(n, ifelse(n %% 100 %in% 11:13, "th",
                                    c("th","st","nd","rd","th","th","th","th","th","th")[n %% 10 + 1]))

h2 <- panel_heading("2", "Brighton: Genuine Contenders?",
                    sprintf("Brighton are %s in the table with a %+d goal difference.\nTheir underlying numbers suggest this isn't a fluke.",
                            tolower(ord(bri$rank_pts)), bri$gd))

c2 <- ggplot(pl_agg, aes(gf, ga)) +
  annotate("rect", xmin = med_gf, xmax = Inf, ymin = -Inf, ymax = med_ga,
           fill = "#C5D0EE", alpha = .45) +
  annotate("text", x = Inf, y = med_ga, label = "Strong attack,\nstrong defence",
           hjust = 1.08, vjust = -0.4, colour = BLUE, fontface = "italic",
           size = mm(SZ$label - 1), lineheight = 1, family = fam) +
  geom_vline(xintercept = med_gf, linetype = "dashed", colour = "grey45") +
  geom_hline(yintercept = med_ga, linetype = "dashed", colour = "grey45") +
  geom_point(aes(size = pts, fill = hl), shape = 21, colour = "white", stroke = 1) +
  geom_label_repel(data = filter(pl_agg, hl != "Other"),
                   aes(label = team, fill = hl), colour = "white", fontface = "bold",
                   size = mm(SZ$label), family = fam, label.size = 0,
                   box.padding = 0.8, min.segment.length = 0, seed = 7) +
  scale_y_reverse() +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.10))) +
  scale_fill_manual(values = hl_cols, guide = "none") +
  scale_size_continuous(range = c(3.5, 11)) +
  labs(x = "Goals scored  (more = better attack)",
       y = "Goals conceded  (top = better defence)") +
  theme_chart()

side2 <- stat_block(bri$gf, "Goals scored",
                    sprintf("%s in the league", ord(min_rank(-pl_agg$gf)[pl_agg$team == bri$team]))) /
  stat_block(bri$ga, "Goals conceded",
             sprintf("%s best", ord(min_rank(pl_agg$ga)[pl_agg$team == bri$team]))) /
  stat_block(sprintf("%+d", bri$gd), "Goal difference",
             sprintf("%s in the league", ord(min_rank(-pl_agg$gd)[pl_agg$team == bri$team]))) /
  callout("Are they the real deal?\nTheir numbers say yes.") +
  plot_layout(heights = c(1, 1, 1, 0.9))

panel2 <- full(h2) / full(tag_label("Contender credibility matrix")) /
  (c2 + side2 + plot_layout(widths = c(2.7, 1.1))) +
  plot_layout(heights = c(1.45, 0.35, 5.9))

# ---------------------------------------------------------------------
# 4. Panel 3 - second-half impact
#    *** ILLUSTRATIVE DATA - replace with real half-time/full-time points ***
# ---------------------------------------------------------------------
df <- tribble(
  ~team, ~ht_points, ~ft_points,
  "Arsenal",          16, 14, "Aston Villa",      15, 18, "Man City",         14, 13,
  "Bournemouth",      13, 20, "Brighton",         12, 19, "Chelsea",          11, 12,
  "Liverpool",        10, 11, "Brentford",         9, 17, "Fulham",            9,  7,
  "Man United",        8,  9, "Crystal Palace",    8,  6, "Tottenham",         7,  8,
  "Newcastle",         6, 10, "West Ham",          6,  4, "Everton",           5,  5,
  "Wolves",            4,  3, "Leeds",             3,  2, "Burnley",           2,  1,
  "Sunderland",        1,  0, "Nottingham Forest", 0,  0
)
top_four <- c("Bournemouth", "Brighton", "Aston Villa", "Brentford")

df <- df %>%
  mutate(impact = ft_points - ht_points,
         group  = case_when(team %in% top_four ~ team,
                            impact > 0 ~ "Other positive",
                            impact < 0 ~ "Other negative",
                            TRUE ~ "No change"),
         impact_label = ifelse(impact == 0, "0", sprintf("%+d", impact)),
         team = fct_reorder(team, impact))

club_cols <- c("Bournemouth" = "#C8102E", "Brighton" = "#0057B8",
               "Aston Villa" = "#670E36", "Brentford" = "#D71920",
               "Other positive" = "#328B78", "Other negative" = "#CE7770",
               "No change" = "#8c8c8c")

h3 <- panel_heading("3", "Who Owns the Second Half?",
                    "Bournemouth, Brighton, Villa and Brentford collect the most\npoints after the break; Arsenal and City leak them.")

c3 <- ggplot(df, aes(y = team)) +
  annotate("rect", xmin = 0,  xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#E7F0EA", alpha = 0.6) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, fill = "#F5E9E6", alpha = 0.6) +
  geom_vline(xintercept = 0, colour = "#333333", linewidth = 0.7) +
  geom_segment(aes(x = 0, xend = impact, yend = team, colour = group),
               linewidth = 1.5, lineend = "round") +
  geom_point(aes(x = impact, colour = group), size = 4.2) +
  geom_text(aes(x = impact, label = impact_label, colour = group,
                hjust = case_when(impact > 0 ~ -0.7, impact < 0 ~ 1.7, TRUE ~ -1.5)),
            fontface = "bold", size = mm(SZ$label), family = fam) +
  scale_colour_manual(values = club_cols) +
  scale_x_continuous(breaks = seq(-2, 8, 2), limits = c(-3, 9.5), expand = c(0, 0)) +
  labs(x = "Net points swing, half-time to full-time", y = NULL) +
  theme_chart() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size = SZ$axis, face = "bold"))

panel3 <- full(h3) / full(tag_label("Second-half points swing")) / c3 +
  plot_layout(heights = c(1.45, 0.35, 5.9))

# ---------------------------------------------------------------------
# 5. Panel 4 - will City lose three in a row again?
# ---------------------------------------------------------------------
# P(new run of 3 straight losses within n remaining games).
# "old" = still inside the current losing run (does not count);
# f0/f1/f2 = 0/1/2 fresh consecutive losses. 3 fresh losses = success.
p_new <- function(p, n = 38 - GW) {
  old <- 1; f0 <- 0; f1 <- 0; f2 <- 0; succ <- 0
  for (i in seq_len(n)) {
    q <- 1 - p
    succ <- succ + f2 * p
    o  <- old * p
    a0 <- (old + f0 + f1 + f2) * q
    a1 <- f0 * p
    a2 <- f1 * p
    old <- o; f0 <- a0; f1 <- a1; f2 <- a2
  }
  succ
}
p_new_v <- Vectorize(p_new, "p")

# Loss-rate uncertainty: Beta(4.5, 25.5) prior (mean 15%) updated with 3 losses in 10
a <- 4.5 + 3; b <- 25.5 + 7
ps <- seq(0.0005, 0.9995, length.out = 3000)
w  <- dbeta(ps, a, b); w <- w / sum(w)
headline <- sum(w * p_new_v(ps))
band     <- qbeta(c(0.1, 0.9), a, b)
cat(sprintf("P(again) = %.1f%%, loss-rate mean = %.1f%%\n",
            100 * headline, 100 * a / (a + b)))

curve_df <- data.frame(p = seq(0, 0.5, length.out = 300))
curve_df$prob <- p_new_v(curve_df$p)

pts <- data.frame(p = c(0.10, 0.15, 0.30), lx = c(0.012, 0.25, 0.335), ly = c(0.22, 0.10, 0.20))
pts$prob  <- p_new_v(pts$p)
pts$label <- c(sprintf("%s if loss rate is 10%%", percent(pts$prob[1], 1)),
               sprintf("%s if loss rate is 15%%", percent(pts$prob[2], 1)),
               sprintf("%s if loss rate is 30%%\n(this season's 3 in 10)", percent(pts$prob[3], 1)))

h4 <- panel_heading("4", "Will City Lose Three in a Row Again?",
                    "City won their first 7, then lost at Villa, Brighton and Forest (GW8-10).\nChance of a new run of 3+ straight league defeats in the remaining 28 games.")

c4 <- ggplot() +
  annotate("rect", xmin = band[1], xmax = band[2], ymin = 0, ymax = 0.62,
           fill = "#c9cfe6", alpha = 0.6) +
  annotate("text", x = mean(band), y = 0.56, colour = BLUE, fontface = "italic",
           size = mm(SZ$label - 0.5), lineheight = 1, family = fam,
           label = "80% credible range\nfor City's loss rate") +
  geom_line(data = curve_df, aes(p, prob), colour = NAVY, linewidth = 1.5) +
  geom_segment(data = pts, aes(x = p, y = prob, xend = lx, yend = ly),
               colour = "#888888", linewidth = 0.45) +
  geom_point(data = pts, aes(p, prob), colour = RED, size = 4.2) +
  geom_text(data = pts, aes(lx, ly, label = label), hjust = 0, vjust = 1, colour = NAVY,
            size = mm(SZ$label), lineheight = 1.05, family = fam) +
  annotate("text", x = 0.012, y = 0.90, label = percent(headline, accuracy = 1),
           colour = RED, fontface = "bold", size = mm(60), hjust = 0, family = fam) +
  annotate("text", x = 0.14, y = 0.94, hjust = 0, colour = NAVY, fontface = "bold",
           size = mm(SZ$label + 1.5), family = fam,
           label = "best estimate that it happens again") +
  annotate("text", x = 0.14, y = 0.86, hjust = 0, colour = GREY,
           size = mm(SZ$label - 1), family = fam,
           label = sprintf("averaged over loss-rate uncertainty (mean %s)",
                           percent(a / (a + b), accuracy = 1))) +
  scale_x_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0),
                     breaks = seq(0, 0.5, 0.1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1),
                     expand = c(0, 0)) +
  labs(x = "City's per-game loss probability", y = "P(new 3-game losing run)") +
  theme_chart()

panel4 <- full(h4) / full(tag_label("Streak risk model")) / c4 +
  plot_layout(heights = c(1.45, 0.35, 5.9))

# ---------------------------------------------------------------------
# 6. Assemble + export
# ---------------------------------------------------------------------
final <- (panel1 | spacer() | panel2 | plot_layout(widths = c(1, 0.06, 1))) /
  (panel3 | spacer() | panel4 | plot_layout(widths = c(1, 0.06, 1))) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title    = "Premier League 2026/27: Four Stories After the First Ten Gameweeks",
    caption  = paste0("Data: engsoccerdata / football-data.co.uk. Panel 3 uses illustrative half-time data. ",
                      "Panel 4 assumes independent league games."),
    theme = theme(
      plot.background = element_rect(fill = BG, colour = NA),
      plot.title   = element_text(family = fam, face = "bold", size = SZ$title,
                                  colour = NAVY, margin = margin(b = 14)),
      plot.caption = element_text(family = fam, size = SZ$cap, colour = GREY,
                                  hjust = 0, margin = margin(t = 12)),
      plot.margin  = margin(24, 28, 16, 28)
    )
  )

W <- 18; H <- 13.5
ggsave("pl_infographic.png", final, width = W, height = H, dpi = 300,
       device = ragg::agg_png, bg = BG)
ggsave("pl_infographic.pdf", final, width = W, height = H,
       device = cairo_pdf, bg = BG)




# Half Time Analysis Break Up ---------------------------------------------

pl_data=engsoccerdata::england_current(Season = 2026) %>% dplyr::filter(tier==1)

pl_data_h <- pl_data %>% filter(home %in% pl_data$home %>% unique())
pl_data_v <- pl_data %>% filter(visitor %in% pl_data$visitor %>% unique())
all_data<-bind_rows(pl_data_h,pl_data_v)
all_data<-all_data %>% distinct()
library(dplyr)

team_match <- bind_rows(
  
  # Home team
  all_data %>%
    mutate(
      team = home,
      opponent = visitor,
      
      ht_state = case_when(
        ht_hgoal > ht_vgoal ~ "Leading",
        ht_hgoal == ht_vgoal ~ "Drawing",
        TRUE ~ "Trailing"
      ),
      
      ft_state = case_when(
        hgoal > vgoal ~ "Win",
        hgoal == vgoal ~ "Draw",
        TRUE ~ "Loss"
      ),
      
      pts = case_when(
        ft_state == "Win" ~ 3,
        ft_state == "Draw" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(team, opponent, ht_state, ft_state, pts),
  
  # Away team
  all_data %>%
    mutate(
      team = visitor,
      opponent = home,
      
      ht_state = case_when(
        ht_vgoal > ht_hgoal ~ "Leading",
        ht_vgoal == ht_hgoal ~ "Drawing",
        TRUE ~ "Trailing"
      ),
      
      ft_state = case_when(
        vgoal > hgoal ~ "Win",
        vgoal == hgoal ~ "Draw",
        TRUE ~ "Loss"
      ),
      
      pts = case_when(
        ft_state == "Win" ~ 3,
        ft_state == "Draw" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(team, opponent, ht_state, ft_state, pts)
)



ht_summary <- team_match %>%
  group_by(team, ht_state) %>%
  summarise(
    games = n(),
    pts_won = sum(pts),
    max_pts = games * 3,
    pts_won_ht = case_when(
      first(ht_state) == "Drawing"  ~ games,
      first(ht_state) == "Leading"  ~ games * 3,
      first(ht_state) == "Trailing"   ~ games * 0,
      # TRUE                          ~ NA_real_
    ),
    pct_pts = round(100 * pts_won / max_pts, 1),
    .groups = "drop"
  )

ft_summary <- team_match %>%
  group_by(team, ft_state) %>%
  summarise(
    games = n(),
    pts_won = sum(pts),
    max_pts = games * 3,
    pts_won_ft = case_when(
      first(ft_state) == "Draw"  ~ games,
      first(ft_state) == "Win"  ~ games * 3,
      first(ft_state) == "Loss"   ~ games * 0,
      # TRUE                          ~ NA_real_
    ),
    pct_pts = round(100 * pts_won / max_pts, 1),
    .groups = "drop"
  )

ht_summary %>% group_by(team) %>% summarise("Points Won at HT"=sum(pts_won_ht))






