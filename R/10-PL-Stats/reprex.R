# ============================================================
# "The State of the Premier League" - v2 (metrics + probability model)
# install.packages(c("ggplot2","patchwork","dplyr","tidyr","ggrepel","ragg","scales"))
# ALL DATA IS SAMPLE / ILLUSTRATIVE. Swap the tibbles in the DATA section.
# ============================================================
library(ggplot2); library(patchwork); library(dplyr)
library(tidyr);   library(ggrepel);   library(scales)

# ---- Font (change if you like; layout is tuned to survive wide fonts) --
fam <- Sys.getenv("PL_FONT", "sans")
update_geom_defaults("text",  list(family = fam))
update_geom_defaults("label", list(family = fam))

# ---- Palette ------------------------------------------------
navy <- "#1b1646"; red <- "#d62828"; blue <- "#0b5cab"; lblue <- "#8fbbe8"
paper <- "#f6f3ee"; grey <- "#8a8a8a"; pink <- "#f6c9d0"; pale <- "#cfdcf2"
gold <- "#f2b705"; win_c <- "#2a9d8f"; draw_c <- "#c9c4b9"; loss_c <- "#d62828"
ink <- "#222222"; gridc <- "#e6e2da"

# ---- Helpers -----------------------------------------------
bg <- theme(plot.background = element_rect(fill = paper, colour = NA))

theme_chart <- function() {
  theme_minimal(base_size = 11, base_family = fam) +
    theme(
      plot.background  = element_rect(fill = paper, colour = NA),
      panel.background = element_rect(fill = paper, colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = gridc, linewidth = 0.3),
      legend.position  = "top", legend.title = element_blank(),
      legend.key.size  = unit(0.38, "cm"),
      legend.text      = element_text(colour = navy, size = 8.5),
      axis.title       = element_text(colour = navy, size = 9),
      axis.text        = element_text(colour = navy, size = 8.5),
      plot.margin      = margin(2, 8, 2, 8))
}

void_plot <- function() ggplot() + xlim(0, 10) + ylim(0, 1) + theme_void(base_family = fam) + bg

tag_label <- function(txt) {
  void_plot() +
    annotate("label", x = 0, y = 0.5, label = txt, hjust = 0, fill = navy,
             colour = "white", fontface = "bold", size = 3.8,
             label.r = unit(0.15, "lines"), label.padding = unit(0.45, "lines"))
}

panel_heading <- function(num, title, blurb) {
  blurb <- paste(strwrap(gsub("\n", " ", blurb), width = 68), collapse = "\n")
  void_plot() +
    annotate("point", x = 0.25, y = 0.78, size = 9, colour = navy) +
    annotate("text", x = 0.25, y = 0.78, label = num, colour = "white", fontface = "bold", size = 5) +
    annotate("text", x = 0.6, y = 0.78, label = title, hjust = 0,
             fontface = "bold.italic", size = 5.2, colour = navy) +
    annotate("text", x = 0.05, y = 0.27, label = blurb, hjust = 0, size = 3.3,
             colour = ink, lineheight = 1)
}

stat_block <- function(big, label, sub = "", col = navy) {
  void_plot() +
    annotate("text", x = 0.2, y = 0.5, label = big, hjust = 0, fontface = "bold", size = 6.5, colour = col) +
    annotate("text", x = 4.2, y = 0.62, label = label, hjust = 0, fontface = "bold", size = 2.5, colour = navy) +
    annotate("text", x = 4.2, y = 0.30, label = sub, hjust = 0, size = 2.6, colour = grey)
}

callout <- function(txt, fill = pale, col = navy) {
  void_plot() +
    annotate("label", x = 5, y = 0.5, label = txt, fill = fill, colour = col,
             fontface = "italic", size = 3.7, lineheight = 1.1,
             label.r = unit(0.4, "lines"), label.padding = unit(0.7, "lines"))
}

# KPI tile row: tibble(big, small, fill, col)
kpi_row <- function(d) {
  n <- nrow(d); d$x <- (seq_len(n) - 1) * (10 / n) + 0.1
  w <- 10 / n - 0.25
  ggplot(d) +
    geom_rect(aes(xmin = x, xmax = x + w, ymin = 0.04, ymax = 0.96, fill = fill)) +
    geom_text(aes(x = x + w / 2, y = 0.60, label = big, colour = col),
              fontface = "bold", size = 8.5) +
    geom_text(aes(x = x + w / 2, y = 0.20, label = small, colour = col),
              size = 3.0, lineheight = 0.95) +
    scale_fill_identity() + scale_colour_identity() +
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 1), expand = FALSE) +
    theme_void(base_family = fam) + bg
}

# ============================================================
# DATA (sample)
# ============================================================

# ---- 1. Points progression ---------------------------------
pts_prog <- tibble(
  gw = 1:10,
  Chelsea   = cumsum(c(3, 1, 3, 1, 0, 0, 1, 0, 1, 1)),
  `Man Utd` = cumsum(c(1, 3, 0, 1, 3, 0, 1, 1, 0, 1)),
  Spurs     = cumsum(c(0, 3, 1, 0, 1, 3, 0, 1, 0, 0))) |>
  pivot_longer(-gw, names_to = "team", values_to = "pts")
team_cols  <- c(Chelsea = blue, `Man Utd` = red, Spurs = navy)
end_labels <- pts_prog |> filter(gw == 10)

# ---- 2. xPts vs actual -------------------------------------
set.seed(7)
others <- tibble(team = paste("Team", 1:18), xpts = runif(18, 12.5, 23.5)) |>
  mutate(pts = pmin(pmax(xpts + rnorm(18, 0, 2.2), 13), 24))
cred <- bind_rows(others,
                  tibble(team = "Brighton", xpts = 21.5, pts = 23),
                  tibble(team = "Arsenal",  xpts = 24.0, pts = 27)) |>
  mutate(hl = ifelse(team %in% c("Arsenal", "Brighton"), team, "Other"))

# ---- 3a. Title credentials ---------------------------------
title_cmp <- tribble(
  ~metric,                ~Arsenal, ~Brighton, ~`Man City`,
  "Points\nper game",         2.7, 2.2, 2.0,
  "Goal diff.\nper game",    1.7, 1.4, 1.1,
  "xG diff.\nper game",      1.5, 1.2, 0.9,
  "Conceded\nper game",      0.5, 0.7, 0.9) |>
  pivot_longer(-metric, names_to = "team", values_to = "value") |>
  mutate(metric = factor(metric, levels = unique(metric)),
         team = factor(team, levels = c("Arsenal", "Brighton", "Man City")),
         lab = ifelse(metric %in% c("Goal diff.\nper game", "xG diff.\nper game"),
                      sprintf("+%.1f", value), sprintf("%.1f", value)))

# ---- 3b. Title odds: Monte Carlo over the remaining 28 games
table10 <- tribble(
  ~team,        ~pts, ~xppg,
  "Arsenal",     27,  2.40,
  "Brighton",    23,  2.10,
  "Man City",    20,  2.00,
  "Liverpool",   19,  1.85,
  "Newcastle",   18,  1.70,
  "Aston Villa", 17,  1.60,
  "Chelsea",     11,  1.50,
  "Man Utd",     11,  1.40)
set.seed(42)
n_sims <- 20000; rem <- 28
true_ppg <- 0.6 * table10$xppg + 0.4 * 1.45          # shrink toward league mean
# each sim draws a "true strength" (sd 0.30 pts/game) PLUS game-by-game noise
sim <- sapply(seq_len(nrow(table10)), function(i)
  table10$pts[i] + rem * rnorm(n_sims, true_ppg[i], 0.30) + rnorm(n_sims, 0, sqrt(rem) * 1.1))
winner <- max.col(sim + matrix(runif(length(sim), 0, 1e-6), nrow(sim)))
title_odds <- table10 |>
  mutate(p = tabulate(winner, nbins = nrow(table10)) / n_sims) |>
  arrange(desc(p)) |> slice_head(n = 5) |>
  mutate(team = factor(team, levels = rev(team)),
         fill = case_when(team == "Arsenal" ~ red, team == "Brighton" ~ blue,
                          team == "Man City" ~ lblue, TRUE ~ "#b9b5ab"))

# ---- 4. Man City: probability model --------------------------
# Poisson match model: expected goals = league avg x attack x opp defence x venue
avg_goals <- 1.40; home_adv <- 1.15
city_att  <- 1.45; city_def <- 0.80     # def < 1 = concedes less than average
form_adj  <- 0.95                       # small "confidence dip" after 3 defeats

fixtures <- tribble(
  ~opp,           ~venue, ~att,  ~def,
  "Liverpool",     "A",   1.25,  0.85,
  "Newcastle",     "H",   1.10,  0.95,
  "Spurs",         "A",   1.05,  1.05,
  "Fulham",        "H",   0.95,  1.05,
  "Arsenal",       "A",   1.20,  0.70,
  "Bournemouth",   "H",   1.00,  1.05,
  "Everton",       "A",   0.85,  0.95,
  "Chelsea",       "H",   1.10,  1.00,
  "Brighton",      "A",   1.25,  0.80,
  "Man Utd",       "H",   1.00,  1.05)

wdl_probs <- function(att, def, venue) {
  h <- ifelse(venue == "H", home_adv, 1 / home_adv)
  lc <- avg_goals * city_att * form_adj * def * h
  lo <- avg_goals * att * city_def / h
  g  <- outer(dpois(0:10, lc), dpois(0:10, lo))
  c(win = sum(g[lower.tri(g)]), draw = sum(diag(g)), loss = sum(g[upper.tri(g)]))
}
probs <- t(mapply(wdl_probs, fixtures$att, fixtures$def, fixtures$venue))
fixtures <- bind_cols(fixtures, as_tibble(probs)) |>
  mutate(label = paste0(opp, " (", venue, ")"), game = row_number())

# Exact P(at least one run of k straight losses in the next n games)
p_streak <- function(p, k = 3) {
  s <- c(1, rep(0, k - 1)); hit <- 0          # s[j] = P(current run = j-1)
  for (pi in p) {
    hit <- hit + s[k] * pi
    s <- c(sum(s) * (1 - pi), s[1:(k - 1)] * pi)
  }
  hit
}
horizon <- tibble(n = 3:10,
                  p = sapply(3:10, function(n) p_streak(fixtures$loss[1:n])))
N <- 6
p_n         <- p_streak(fixtures$loss[1:N])
exp_pts_n   <- sum(3 * fixtures$win[1:N] + fixtures$draw[1:N])
p_next_loss <- fixtures$loss[1]
p_unbeaten  <- prod(1 - fixtures$loss[1:N])

# Last three matches: chances created vs goals
city_form <- tibble(
  opp  = c("Aston Villa", "Brighton", "Nott'm Forest"),
  xg   = c(2.3, 1.9, 2.6), goals = c(1, 1, 0), poss = c(63, 62, 79)) |>
  mutate(opp_lab = paste0(opp, "\n", poss, "% possession"),
         opp_lab = factor(opp_lab, levels = rev(opp_lab)))

# ============================================================
# PANELS
# ============================================================
header <- ggplot() +
  annotate("rect", xmin = 0, xmax = 10, ymin = 0, ymax = 1, fill = navy) +
  annotate("text", x = 0.25, y = 0.5, label = "Premier\nLeague", hjust = 0,
           colour = "white", fontface = "bold", size = 7, lineheight = 0.9) +
  annotate("segment", x = 1.75, xend = 1.75, y = 0.15, yend = 0.85, colour = "#6f6a9c", linewidth = 0.4) +
  annotate("text", x = 2.0, y = 0.68, label = "THE STATE OF THE PREMIER LEAGUE",
           hjust = 0, colour = "white", fontface = "bold", size = 11) +
  annotate("text", x = 2.0, y = 0.25, label = "10 games in. Four stories. One unpredictable season.",
           hjust = 0, colour = "white", fontface = "italic", size = 6) +
  scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) + theme_void(base_family = fam)

# ---- Panel 1 ------------------------------------------------
h1 <- panel_heading("1", "The Giants are Wobbling",
                    "Chelsea, Man United and Spurs are struggling for consistency\n- with just 11 points combined from the first 10 games.")

c1 <- ggplot(pts_prog, aes(gw, pts, colour = team)) +
  geom_line(linewidth = 1) +
  geom_point(data = end_labels, size = 2.6, show.legend = FALSE) +
  geom_label(data = end_labels, aes(label = pts, fill = team), colour = "white",
             fontface = "bold", size = 3.4, label.size = 0, show.legend = FALSE,
             position = position_nudge(x = 0.55)) +
  scale_colour_manual(values = team_cols) + scale_fill_manual(values = team_cols) +
  scale_x_continuous(breaks = 1:10, labels = paste0("GW", 1:10),
                     expand = expansion(add = c(0.4, 1.3))) +
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, 2)) +
  labs(x = NULL, y = NULL) + theme_chart() +
  theme(axis.text.x = element_text(size = 7.5), panel.grid.major.x = element_blank())

cards <- tibble(team = c("Chelsea", "Man Utd", "Spurs"),
                ppg = sprintf("%.1f", c(11, 11, 9) / 10),
                xppg = sprintf("xPts/G %.1f", c(1.6, 1.4, 1.5)),
                x = c(0.95, 2.95, 4.95))
c1_cards <- ggplot(cards) +
  geom_rect(aes(xmin = x - 0.9, xmax = x + 0.9, ymin = 0.02, ymax = 0.98),
            fill = "#e6e2da", colour = "#cfc9be") +
  geom_text(aes(x, 0.83, label = team), fontface = "bold", colour = navy, size = 3.1) +
  geom_text(aes(x, 0.57, label = ppg), fontface = "bold", colour = navy, size = 6.5) +
  geom_text(aes(x, 0.34, label = "Pts/Game"), size = 2.6, colour = navy) +
  geom_text(aes(x, 0.17, label = xppg), size = 2.4, colour = grey) +
  annotate("text", x = 6.5, y = 0.85, label = "Key Issue?", hjust = 0,
           fontface = "bold", size = 4.2, colour = navy) +
  annotate("text", x = 6.5, y = 0.40, hjust = 0, size = 2.9, lineheight = 1.05,
           label = "Defensive frailties\nand missed chances\nare hurting the big clubs.") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 1)) + theme_void(base_family = fam) + bg

panel1 <- h1 / tag_label("Points Progression") / c1 / c1_cards +
  plot_layout(heights = c(1.5, 0.5, 3.6, 1.3))

# ---- Panel 2 ------------------------------------------------
h2 <- panel_heading("2", "Brighton: Genuine Contenders?",
                    "Brighton are second in the table, with a remarkable +19 goal difference.\nTheir underlying numbers suggest this isn't a fluke.")

c2 <- ggplot(cred, aes(xpts, pts)) +
  annotate("rect", xmin = 20, xmax = 28, ymin = 20, ymax = 28, fill = pale, alpha = 0.55) +
  geom_vline(xintercept = 20, linetype = "dashed", colour = "#666666", linewidth = 0.3) +
  geom_hline(yintercept = 20, linetype = "dashed", colour = "#666666", linewidth = 0.3) +
  annotate("text", x = 27.8, y = 12.6, label = "Underperforming", hjust = 1, size = 2.9, colour = grey, fontface = "italic") +
  annotate("text", x = 12.2, y = 27.5, label = "Overperforming", hjust = 0, size = 2.9, colour = grey, fontface = "italic") +
  annotate("text", x = 27.8, y = 20.7, label = "Elite and deserved", hjust = 1, size = 2.9, colour = blue, fontface = "bold.italic") +
  geom_point(data = filter(cred, hl == "Other"), colour = "#aaaaaa", size = 2.2) +
  geom_point(data = filter(cred, hl != "Other"), aes(fill = hl), shape = 21,
             colour = "white", stroke = 1, size = 4.2) +
  geom_label_repel(data = filter(cred, hl != "Other"), aes(label = hl, fill = hl),
                   colour = "white", fontface = "bold", size = 3.5, label.size = 0,
                   nudge_x = -3.2, nudge_y = -1.2, segment.colour = "#777777") +
  scale_fill_manual(values = c(Arsenal = red, Brighton = blue)) +
  scale_x_continuous(breaks = seq(12, 28, 4)) + scale_y_continuous(breaks = seq(12, 28, 4)) +
  coord_cartesian(xlim = c(12, 28), ylim = c(12, 28)) +
  labs(x = "Expected Points (xG based)", y = "Actual Points") +
  theme_chart() + theme(legend.position = "none")

side2 <- stat_block("26", "Goals Scored", "(most in PL)") /
  stat_block("7", "Goals Conceded", "(2nd best)") /
  stat_block("+19", "Goal Difference", "(+2.5 above xG)") /
  callout("Are they the real deal?\nTheir numbers say yes.") +
  plot_layout(heights = c(1, 1, 1, 1.1))
panel2 <- h2 / tag_label("Contender Credibility Matrix") / (c2 + side2 + plot_layout(widths = c(2.6, 1.4))) +
  plot_layout(heights = c(1.5, 0.5, 5.0))

# ---- Panel 3 ------------------------------------------------
h3 <- panel_heading("3", "Will This Finally Be Arsenal's Year?",
                    "With 27 points from 10 games and just 5 goals conceded,\nArsenal look like the strongest title contenders.")

c3 <- ggplot(title_cmp, aes(metric, value, fill = team)) +
  geom_col(position = position_dodge(width = 0.82), width = 0.7, colour = paper, linewidth = 0.6) +
  geom_text(aes(label = lab), position = position_dodge(width = 0.82),
            vjust = -0.5, size = 2.4, colour = navy) +
  scale_fill_manual(values = c(Arsenal = red, Brighton = blue, `Man City` = lblue)) +
  scale_y_continuous(limits = c(0, 3.2), breaks = seq(0, 3, 1), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) + theme_chart() +
  theme(axis.text.x = element_text(size = 7.5), panel.grid.major.x = element_blank())

side3 <- stat_block("27", "Points", "(1st)", col = red) /
  stat_block("5", "Goals Conceded", "(fewest)", col = red) /
  callout("Are they ready\nto go all the way?", fill = pink, col = "#8a1c2b") +
  plot_layout(heights = c(1, 1, 1.1))

c3b <- ggplot(title_odds, aes(p, team)) +
  geom_col(aes(fill = fill), width = 0.62) +
  geom_text(aes(label = percent(p, accuracy = 1)), hjust = -0.2, size = 3.4,
            fontface = "bold", colour = navy) +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0, max(title_odds$p) * 1.2), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) + theme_chart() +
  theme(axis.text.x = element_blank(), panel.grid = element_blank(),
        axis.text.y = element_text(face = "bold", size = 9))

odds_note <- void_plot() +
  annotate("text", x = 0.05, y = 0.5, hjust = 0, size = 2.6, colour = grey, lineheight = 1,
           label = "Monte Carlo: 20,000 simulations of the remaining 28 games.\nStrength from xPts/game (shrunk to league mean) + uncertainty. Sample inputs.")

panel3 <- h3 / tag_label("Title Credentials Comparison") /
  (c3 + side3 + plot_layout(widths = c(2.8, 1.3))) /
  tag_label("Title Odds - who finishes top?") / c3b / odds_note +
  plot_layout(heights = c(1.2, 0.5, 3.6, 0.5, 2.2, 0.45))

# ---- Panel 4: probability engine ----------------------------
h4 <- panel_heading("4", "Maresca's Three-Game Nightmare",
                    "Manchester City have lost their last three matches despite\ncontinuing to dominate possession and create chances.")

kpis <- kpi_row(tibble(
  big   = c(percent(p_n, accuracy = 0.1), percent(p_next_loss, accuracy = 1), sprintf("%.1f", exp_pts_n)),
  small = c(paste0("chance of 3 straight\nlosses in next ", N, " games"),
            "chance of losing\nthe next match",
            paste0("expected points\nfrom next ", N, " games")),
  fill  = c(navy, "#e6e2da", "#e6e2da"),
  col   = c("white", navy, navy)))

# xG created vs goals scored (dumbbell, one axis)
c4a <- ggplot(city_form, aes(y = opp_lab)) +
  geom_segment(aes(x = goals, xend = xg, yend = opp_lab), colour = "#b9b5ab", linewidth = 2.2, lineend = "round") +
  geom_point(aes(x = xg, colour = "xG created"), size = 4.5) +
  geom_point(aes(x = goals, colour = "Goals scored"), size = 4.5) +
  geom_text(aes(x = xg, label = sprintf("%.1f", xg)), vjust = -1.5, size = 3, fontface = "bold", colour = navy) +
  geom_text(aes(x = goals, label = goals), vjust = -1.5, size = 3, fontface = "bold", colour = navy) +
  scale_colour_manual(values = c(`xG created` = lblue, `Goals scored` = navy)) +
  scale_x_continuous(limits = c(-0.2, 3.6), breaks = 0:3) +
  labs(x = NULL, y = NULL) + theme_chart() +
  theme(panel.grid.major.y = element_blank())

# Win / draw / loss probabilities for the next six fixtures
wdl <- fixtures |> slice(1:N) |>
  select(game, label, win, draw, loss) |>
  pivot_longer(c(win, draw, loss), names_to = "res", values_to = "p") |>
  mutate(res = factor(res, levels = c("loss", "draw", "win"), labels = c("Loss", "Draw", "Win")),
         label = factor(label, levels = rev(unique(label))))

c4b <- ggplot(wdl, aes(p, label, fill = res)) +
  geom_col(width = 0.68, colour = paper, linewidth = 0.8, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = ifelse(p > 0.12, percent(p, accuracy = 1), "")),
            position = position_stack(vjust = 0.5, reverse = TRUE),
            size = 2.5, colour = "white", fontface = "bold") +
  scale_fill_manual(values = c(Win = win_c, Draw = "#9b968b", Loss = loss_c),
                    breaks = c("Win", "Draw", "Loss")) +
  scale_x_continuous(labels = percent, expand = c(0, 0)) +
  labs(x = NULL, y = NULL) + theme_chart() +
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_text(size = 8.5))

# Probability of a 3-game losing run vs horizon
c4c <- ggplot(horizon, aes(n, p)) +
  geom_area(fill = lblue, alpha = 0.35) +
  geom_line(colour = navy, linewidth = 1) +
  geom_point(colour = navy, size = 2) +
  geom_point(data = filter(horizon, n == N), shape = 21, fill = red, colour = "white", stroke = 1, size = 4.8) +
  geom_text(data = filter(horizon, n == N),
            aes(label = paste0(percent(p, accuracy = 0.1), " by game ", N)),
            vjust = -1.2, hjust = 0.7, size = 3.3, fontface = "bold", colour = navy) +
  scale_x_continuous(breaks = 3:10, labels = function(x) paste0("next ", x)) +
  scale_y_continuous(labels = percent, limits = c(0, max(horizon$p) * 1.35), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) + theme_chart() +
  theme(axis.text.x = element_text(size = 8), panel.grid.major.x = element_blank())

model_note <- void_plot() +
  annotate("text", x = 0.05, y = 0.5, hjust = 0, size = 2.6, colour = grey, lineheight = 1,
           label = paste0("Model: Poisson goals from attack/defence ratings + home advantage;\n",
                          "games independent; streak probability computed exactly.\n",
                          "Fixtures and ratings are illustrative."))

c4_mid <- (c4a | c4b) + plot_layout(widths = c(1, 1.15))

panel4 <- h4 / tag_label("Will it happen again? City's loss-streak probability") / kpis /
  tag_label("xG vs goals  |  Next six: W / D / L odds") / c4_mid /
  tag_label("Chance of a 3-game losing run, by horizon") / c4c / model_note +
  plot_layout(heights = c(1.2, 0.5, 1.3, 0.5, 2.3, 0.5, 1.7, 0.55))

# ---- Footer -------------------------------------------------
footer <- ggplot() +
  annotate("rect", xmin = 0, xmax = 10, ymin = 0, ymax = 1, fill = navy) +
  annotate("text", x = 0.3, y = 0.68, label = "The Big Picture", hjust = 0, colour = "white", fontface = "bold", size = 8) +
  annotate("text", x = 0.3, y = 0.28, hjust = 0, colour = "white", size = 4, fontface = "italic",
           label = "A tight title race, resurgent Brighton, and unsettled giants - the 2026/27 Premier League is wide open.") +
  annotate("segment", x = 7.3, xend = 7.3, y = 0.15, yend = 0.85, colour = "#6f6a9c", linewidth = 0.4) +
  annotate("text", x = 7.5, y = 0.5, hjust = 0, colour = gold, fontface = "bold", size = 4.6, lineheight = 1,
           label = "Who will be top of the table\nat the end of the season?") +
  scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) + theme_void(base_family = fam)

# ============================================================
# ASSEMBLE & SAVE
# ============================================================
final <- header / (panel1 | panel2) / (panel3 | panel4) / footer +
  plot_layout(heights = c(1, 5.3, 9.2, 0.9)) +
  plot_annotation(theme = theme(plot.background = element_rect(fill = paper, colour = NA)))

out <- Sys.getenv("PL_OUT", "premier_league_infographic_v2.png")
ggsave(out, final, width = 13, height = 17, dpi = 150, device = ragg::agg_png)
message(sprintf("P(3 straight losses in next %d) = %.1f%% | exp pts = %.2f | P(loss next) = %.0f%%",
                N, 100 * p_n, exp_pts_n, 100 * p_next_loss))
print(horizon); print(title_odds[, c("team", "p")])






# Half Time Analysis Break Up ---------------------------------------------

pl_data=engsoccerdata::england_current(Season = 2026) %>% dplyr::filter(tier==1)

pl_data_h <- pl_data %>% filter(home %in% pl_$team %>% unique())
pl_data_v <- pl_data %>% filter(visitor %in% pl_$team %>% unique())
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

View(ht_summary %>% group_by(team) %>% summarise("Points Won at HT"=sum(pts_won_ht)))




# Chart reprex for HT vs FT Summary

library(tidyverse)

# 1. Illustrative Premier League data
df <- tribble(
  ~team, ~ht_points, ~ft_points,
  "Arsenal",          16, 14,
  "Man City",         14, 13,
  "Aston Villa",      15, 18,
  "Bournemouth",      13, 20,
  "Brighton",         12, 19,
  "Brentford",         9, 17,
  "Chelsea",          11, 12,
  "Liverpool",        10, 11,
  "Man United",        8,  9,
  "Tottenham",         7,  8,
  "Newcastle",         6, 10,
  "Fulham",            9,  7,
  "Crystal Palace",    8,  6,
  "Everton",           5,  5,
  "West Ham",          6,  4,
  "Wolves",            4,  3,
  "Leeds",             3,  2,
  "Burnley",           2,  1,
  "Sunderland",        1,  0,
  "Nottingham Forest", 0,  0
)

# 2. Calculate league positions
df <- df %>%
  mutate(
    ht_rank = rank(-ht_points, ties.method = "first"),
    ft_rank = rank(-ft_points, ties.method = "first"),
    movement = ht_rank - ft_rank,
    highlight = team %in% c(
      "Bournemouth", "Brighton",
      "Aston Villa", "Brentford"
    )
  )

# 3. Club colours for the top four
club_cols <- c(
  "Bournemouth" = "#C8102E",
  "Brighton"    = "#0057B8",
  "Aston Villa" = "#670E36",
  "Brentford"   = "#D71920"
)

# 4. Build the graphic
ggplot() +
  
  # Background connections
  geom_curve(
    data = filter(df, !highlight),
    aes(
      x = 1, y = ht_rank,
      xend = 2, yend = ft_rank
    ),
    curvature = 0.20,
    colour = "#C9C9C9",
    linewidth = 0.7,
    alpha = 0.8
  ) +
  
  # Highlighted top four
  geom_curve(
    data = filter(df, highlight),
    aes(
      x = 1, y = ht_rank,
      xend = 2, yend = ft_rank,
      colour = team
    ),
    curvature = 0.20,
    linewidth = 1.5
  ) +
  
  # HT and FT endpoints
  geom_point(
    data = df,
    aes(x = 1, y = ht_rank),
    size = 2,
    colour = "#777777"
  ) +
  
  geom_point(
    data = df,
    aes(x = 2, y = ft_rank,
        colour = if_else(highlight, team, "Other")),
    size = 2.5
  ) +
  
  # Left-side labels
  geom_text(
    data = df,
    aes(x = 0.91, y = ht_rank,
        label = paste0(ht_rank, "  ", team),
        fontface = if_else(highlight, "bold", "plain")),
    hjust = 1,
    size = 3.1,
    colour = "#353535"
  ) +
  
  # Right-side labels
  geom_text(
    data = df,
    aes(x = 2.09, y = ft_rank,
        label = paste0(team, "  ", ft_rank),
        fontface = if_else(highlight, "bold", "plain")),
    hjust = 0,
    size = 3.1,
    colour = "#353535"
  ) +
  
  scale_colour_manual(
    values = c(club_cols, "Other" = "#777777"),
    guide = "none"
  ) +
  
  scale_y_reverse(
    breaks = 1:20,
    limits = c(20.5, 0.5)
  ) +
  
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("HALF-TIME", "FULL-TIME"),
    limits = c(0.25, 2.75)
  ) +
  
  labs(
    title = "TWO TABLES. ONE LEAGUE.",
    subtitle = "How the Premier League standings transform after the break",
    caption = "Illustrative data | Rank 1 = top of the table"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(
      fill = "#F8F7F3", colour = NA
    ),
    panel.background = element_rect(
      fill = "#F8F7F3", colour = NA
    ),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(
      face = "bold", size = 11,
      colour = "#222222"
    ),
    plot.title = element_text(
      face = "bold", size = 22
    ),
    plot.subtitle = element_text(
      size = 11, colour = "#666666"
    ),
    plot.caption = element_text(
      colour = "#888888"
    ),
    plot.margin = margin(20, 65, 20, 65)
  ) +
  coord_cartesian(clip = "off")

# Reprex 2



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
