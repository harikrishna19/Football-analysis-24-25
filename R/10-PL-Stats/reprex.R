# ============================================================
# "The State of the Premier League" infographic in R
# install.packages(c("ggplot2","patchwork","dplyr","tidyr","ggtext","ggrepel","ragg"))
# All data below is SAMPLE data, tuned to match the numbers in the mock-up.
# ============================================================
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
library(ggtext)
library(ggrepel)

# ---- Palette ------------------------------------------------
navy   <- "#1b1646"
red    <- "#d62828"
blue   <- "#0b5cab"
lblue  <- "#9cc3ea"
paper  <- "#f6f3ee"
grey   <- "#8a8a8a"
pink   <- "#f6c9d0"
pale   <- "#cfdcf2"

# ---- Shared theme / helpers --------------------------------
theme_chart <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.background  = element_rect(fill = paper, colour = NA),
      panel.background = element_rect(fill = paper, colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#e3dfd8", linewidth = 0.3),
      legend.position  = "top",
      legend.title     = element_blank(),
      legend.key.size  = unit(0.4, "cm"),
      axis.title       = element_text(colour = navy, size = 10),
      axis.text        = element_text(colour = navy, size = 9),
      plot.margin      = margin(2, 8, 2, 8)
    )
}

# Pill-shaped section label (e.g. "Points Progression")
tag_label <- function(txt) {
  ggplot() +
    annotate("label", x = 0, y = 0.5, label = txt, hjust = 0, fill = navy,
             colour = "white", fontface = "bold", size = 4,
             label.r = unit(0.15, "lines"), label.padding = unit(0.45, "lines")) +
    xlim(0, 10) + ylim(0, 1) +
    theme_void() + theme(plot.background = element_rect(fill = paper, colour = NA))
}

# Numbered panel heading + blurb
panel_heading <- function(num, title, blurb) {
  ggplot() +
    annotate("point", x = 0.25, y = 0.78, size = 9, colour = navy) +
    annotate("text",  x = 0.25, y = 0.78, label = num, colour = "white",
             fontface = "bold", size = 5) +
    annotate("text",  x = 0.6, y = 0.78, label = title, hjust = 0,
             fontface = "bold.italic", size = 6.5, colour = navy) +
    annotate("text",  x = 0.05, y = 0.28, label = blurb, hjust = 0, vjust = 0.5,
             size = 4, colour = "#222222", lineheight = 1) +
    xlim(0, 10) + ylim(0, 1) +
    theme_void() + theme(plot.background = element_rect(fill = paper, colour = NA))
}

# Big number + caption, used in the side stat columns
stat_block <- function(big, label, sub = NULL, col = navy) {
  ggplot() +
    annotate("text", x = 0.2, y = 0.5, label = big, hjust = 0, fontface = "bold",
             size = 8, colour = col) +
    annotate("text", x = 5.0, y = 0.62, label = label, hjust = 0, fontface = "bold",
             size = 3.1, colour = navy) +
    annotate("text", x = 5.0, y = 0.30, label = if (is.null(sub)) "" else sub, hjust = 0,
             size = 3.2, colour = grey) +
    xlim(0, 10) + ylim(0, 1) +
    theme_void() + theme(plot.background = element_rect(fill = paper, colour = NA))
}

callout <- function(txt, fill = pale, col = navy) {
  ggplot() +
    annotate("label", x = 5, y = 0.5, label = txt, fill = fill, colour = col,
             fontface = "italic", size = 3.8, lineheight = 1.1,
             label.r = unit(0.4, "lines"), label.padding = unit(0.7, "lines"),
             label.size = 0) +
    xlim(0, 10) + ylim(0, 1) +
    theme_void() + theme(plot.background = element_rect(fill = paper, colour = NA))
}

# ============================================================
# SAMPLE DATA
# ============================================================

# ---- 1. Points progression (cumulative, GW1-GW10) -----------
pts_prog <- tibble(
  gw      = 1:10,
  Chelsea = cumsum(c(3, 1, 3, 1, 0, 0, 1, 0, 1, 1)),   # ends on 11
  `Man Utd` = cumsum(c(1, 3, 0, 1, 3, 0, 1, 1, 0, 1)), # ends on 11
  Spurs   = cumsum(c(0, 3, 1, 0, 1, 3, 0, 1, 0, 0))    # ends on 9
) |>
  pivot_longer(-gw, names_to = "team", values_to = "pts")

team_cols <- c(Chelsea = blue, `Man Utd` = red, Spurs = navy)
end_labels <- pts_prog |> filter(gw == 10)

# ---- 2. Contender credibility (xPts vs actual pts) ----------
set.seed(7)
others <- tibble(
  team = paste("Team", 1:18),
  xpts = runif(18, 12.5, 23.5)
) |>
  mutate(pts = pmin(pmax(xpts + rnorm(18, 0, 2.2), 13), 24))

cred <- bind_rows(
  others,
  tibble(team = "Brighton", xpts = 21.5, pts = 23),
  tibble(team = "Arsenal",  xpts = 24.0, pts = 27)
) |>
  mutate(hl = case_when(team == "Arsenal" ~ "Arsenal",
                        team == "Brighton" ~ "Brighton",
                        TRUE ~ "Other"))

# ---- 3. Title credentials ----------------------------------
title_cmp <- tribble(
  ~metric,                ~Arsenal, ~Brighton, ~`Man City`,
  "Pts/Game",              2.7,      2.2,       2.0,
  "GD/Game",               1.7,      1.4,       1.1,
  "xG Diff/Game",          1.5,      1.2,       0.9,
  "Goals Conceded\n/Game", 0.5,      0.7,       0.9
) |>
  pivot_longer(-metric, names_to = "team", values_to = "value") |>
  mutate(metric = factor(metric, levels = unique(metric)),
         team   = factor(team, levels = c("Arsenal", "Brighton", "Man City")),
         lab    = ifelse(metric %in% c("GD/Game", "xG Diff/Game"),
                         sprintf("+%.1f", value), sprintf("%.1f", value)))

# ---- 4. Man City recent form -------------------------------
city <- tibble(
  match      = factor(c("Aston Villa\n(L)", "Brighton\n(L)", "Nott'm Forest\n(L)"),
                      levels = c("Aston Villa\n(L)", "Brighton\n(L)", "Nott'm Forest\n(L)")),
  x          = 1:3,
  possession = c(63, 62, 79),
  goals      = c(1, 1, 0)
) |>
  mutate(goals_scaled = 40 + goals * 20)  # map 0-3 goals onto the 40-100% axis

# ============================================================
# PANELS
# ============================================================

# ---- Header -------------------------------------------------
header <- ggplot() +
  annotate("rect", xmin = 0, xmax = 10, ymin = 0, ymax = 1, fill = navy) +
  annotate("text", x = 0.25, y = 0.5, label = "Premier\nLeague", hjust = 0,
           colour = "white", fontface = "bold", size = 7, lineheight = 0.9) +
  annotate("segment", x = 1.75, xend = 1.75, y = 0.15, yend = 0.85,
           colour = "#6f6a9c", linewidth = 0.4) +
  annotate("text", x = 2.0, y = 0.68, label = "THE STATE OF THE PREMIER LEAGUE",
           hjust = 0, colour = "white", fontface = "bold", size = 11) +
  annotate("text", x = 2.0, y = 0.25,
           label = "10 games in. Four stories. One unpredictable season.",
           hjust = 0, colour = "white", fontface = "italic", size = 6) +
  scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1),  expand = c(0, 0)) +
  theme_void()

# ---- Panel 1 ------------------------------------------------
h1 <- panel_heading("1", "The Giants are Wobbling",
                    "Chelsea, Man United and Spurs are struggling for consistency\n- with just 11 points combined from the first 10 games.")

c1 <- ggplot(pts_prog, aes(gw, pts, colour = team)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  geom_label(data = end_labels, aes(label = pts, fill = team),
             colour = "white", fontface = "bold", size = 3.5, hjust = -0.2,
             label.size = 0, show.legend = FALSE,
             position = position_nudge(x = 0.15)) +
  scale_colour_manual(values = team_cols) +
  scale_fill_manual(values = team_cols) +
  scale_x_continuous(breaks = 1:10, labels = paste0("GW", 1:10),
                     expand = expansion(add = c(0.4, 1.2))) +
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, 2)) +
  labs(x = NULL, y = NULL) +
  theme_chart() +
  theme(axis.text.x = element_text(size = 7.5))

# Team cards (pts per game)
cards <- tibble(
  team = c("Chelsea", "Man Utd", "Spurs"),
  ppg  = sprintf("%.1f", c(11, 11, 9) / 10),
  x    = c(1.0, 3.2, 5.4)
)
c1_cards <- ggplot(cards) +
  geom_rect(aes(xmin = x - 1, xmax = x + 1, ymin = 0.05, ymax = 0.95),
            fill = "#e6e2da", colour = "#cfc9be") +
  geom_text(aes(x = x, y = 0.68, label = team), fontface = "bold",
            colour = navy, size = 3.6) +
  geom_text(aes(x = x, y = 0.38, label = ppg), fontface = "bold",
            colour = navy, size = 7) +
  geom_text(aes(x = x, y = 0.14, label = "Pts/Game"), size = 3, colour = navy) +
  annotate("text", x = 7.0, y = 0.88, label = "Key Issue?", hjust = 0,
           fontface = "bold", size = 4.5, colour = navy) +
  annotate("text", x = 7.0, y = 0.38, hjust = 0, size = 3.3, lineheight = 1.05,
           label = "Defensive frailties\nand missed chances\nare hurting the big clubs.") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 1)) +
  theme_void() + theme(plot.background = element_rect(fill = paper, colour = NA))

panel1 <- h1 / tag_label("Points Progression") / c1 / c1_cards +
  plot_layout(heights = c(1.5, 0.5, 3.6, 1.3))

# ---- Panel 2 ------------------------------------------------
h2 <- panel_heading("2", "Brighton: Genuine Contenders?",
                    "Brighton are second in the table, with a remarkable +19 goal difference.\nTheir underlying numbers suggest this isn't a fluke.")

c2 <- ggplot(cred, aes(xpts, pts)) +
  geom_vline(xintercept = 20, linetype = "dashed", colour = "#555555", linewidth = 0.3) +
  geom_hline(yintercept = 20, linetype = "dashed", colour = "#555555", linewidth = 0.3) +
  geom_point(data = filter(cred, hl == "Other"), colour = "#aaaaaa", size = 2.2) +
  geom_point(data = filter(cred, hl != "Other"), aes(colour = hl), size = 3.5) +
  geom_label_repel(data = filter(cred, hl != "Other"),
                   aes(label = hl, fill = hl), colour = "white",
                   fontface = "bold", size = 3.6, label.size = 0,
                   nudge_x = -3, nudge_y = 0.4, segment.colour = "#777777") +
  scale_colour_manual(values = c(Arsenal = red, Brighton = blue)) +
  scale_fill_manual(values   = c(Arsenal = red, Brighton = blue)) +
  scale_x_continuous(breaks = seq(12, 28, 4)) +
  scale_y_continuous(breaks = seq(12, 28, 4)) +
  coord_cartesian(xlim = c(12, 28), ylim = c(12, 28)) +
  labs(x = "Expected Points (xG based)", y = "Actual Points") +
  theme_chart() + theme(legend.position = "none")

side2 <- stat_block("26", "Goals Scored", "(most in PL)") /
  stat_block("7",  "Goals Conceded", "(2nd best)") /
  stat_block("+19", "Goal Difference") /
  callout("Are they the real deal?\nTheir numbers say yes.") +
  plot_layout(heights = c(1, 1, 1, 1.1))

body2 <- (c2 + side2 + plot_layout(widths = c(2.6, 1.4)))
panel2 <- h2 / tag_label("Contender Credibility Matrix") / body2 +
  plot_layout(heights = c(1.5, 0.5, 5.0))

# ---- Panel 3 ------------------------------------------------
h3 <- panel_heading("3", "Will This Finally Be Arsenal's Year?",
                    "With 27 points from 10 games and just 5 goals conceded,\nArsenal look like the strongest title contenders.")

c3 <- ggplot(title_cmp, aes(metric, value, fill = team)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = lab), position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3.2, fontface = "bold", colour = navy) +
  scale_fill_manual(values = c(Arsenal = red, Brighton = blue, `Man City` = lblue)) +
  scale_y_continuous(limits = c(0, 3.1), breaks = seq(0, 3, 0.5),
                     expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_chart() +
  theme(axis.text.x = element_text(size = 8.5))

side3 <- stat_block("27", "Points", "(1st)", col = red) /
  stat_block("5", "Goals Conceded", "(fewest)", col = red) /
  callout("Are they ready\nto go all the way?", fill = pink, col = "#8a1c2b") +
  plot_layout(heights = c(1, 1, 1.1))

body3 <- (c3 + side3 + plot_layout(widths = c(2.6, 1.4)))
panel3 <- h3 / tag_label("Title Credentials Comparison") / body3 +
  plot_layout(heights = c(1.5, 0.5, 5.0))

# ---- Panel 4 ------------------------------------------------
h4 <- panel_heading("4", "Maresca's Three-Game Nightmare",
                    "Manchester City have lost their last three matches despite\ncontinuing to dominate possession and create chances.")

c4 <- ggplot(city) +
  geom_rect(aes(xmin = x - 0.35, xmax = x + 0.35, ymin = 40, ymax = possession,
                fill = "Possession")) +
  geom_line(aes(x, goals_scaled, group = 1, colour = "Goals"), linewidth = 0.8) +
  geom_point(aes(x, goals_scaled, colour = "Goals"), size = 2.5) +
  geom_text(aes(x, goals_scaled, label = goals), vjust = -1, size = 3.2,
            fontface = "bold", colour = navy) +
  geom_text(data = filter(city, x == 3), aes(x, possession, label = "79%"),
            vjust = -0.6, size = 3.2, colour = navy) +
  scale_fill_manual(values = c(Possession = lblue)) +
  scale_colour_manual(values = c(Goals = navy)) +
  scale_x_continuous(breaks = 1:3, labels = levels(city$match),
                     limits = c(0.4, 3.6)) +
  scale_y_continuous(limits = c(40, 100), breaks = seq(40, 100, 20),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0),
                     sec.axis = sec_axis(~ (. - 40) / 20, breaks = 0:3)) +
  labs(x = NULL, y = NULL) +
  theme_chart()

# Right-hand call-out box in place of the photo
side4 <- ggplot() +
  annotate("rect", xmin = 0.3, xmax = 9.7, ymin = 0.1, ymax = 0.9, fill = navy) +
  annotate("text", x = 5, y = 0.62, label = "3 Consecutive\nDefeats", colour = "white",
           fontface = "bold", size = 6, lineheight = 0.95) +
  annotate("text", x = 5, y = 0.28, label = "Is it a tactical issue,\nor just poor finishing?",
           colour = "white", fontface = "italic", size = 3.4, lineheight = 1) +
  xlim(0, 10) + ylim(0, 1) + theme_void() +
  theme(plot.background = element_rect(fill = paper, colour = NA))

body4 <- (c4 + side4 + plot_layout(widths = c(2.2, 1.3)))
panel4 <- h4 / tag_label("City's Recent Form") / body4 +
  plot_layout(heights = c(1.5, 0.5, 5.0))

# ---- Footer -------------------------------------------------
footer <- ggplot() +
  annotate("rect", xmin = 0, xmax = 10, ymin = 0, ymax = 1, fill = navy) +
  annotate("text", x = 0.3, y = 0.68, label = "The Big Picture", hjust = 0,
           colour = "white", fontface = "bold", size = 8) +
  annotate("text", x = 0.3, y = 0.28, hjust = 0, colour = "white", size = 4,
           fontface = "italic",
           label = "A tight title race, resurgent Brighton, and unsettled giants - the 2026/27 Premier League is wide open.") +
  annotate("segment", x = 7.6, xend = 7.6, y = 0.15, yend = 0.85,
           colour = "#6f6a9c", linewidth = 0.4) +
  annotate("text", x = 7.8, y = 0.5, hjust = 0, colour = "#f2b705",
           fontface = "bold", size = 5, lineheight = 1,
           label = "Who will be top of the table\nat the end of the season?") +
  scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1),  expand = c(0, 0)) +
  theme_void()

# ============================================================
# ASSEMBLE & SAVE
# ============================================================
final <- header /
  (panel1 | panel2) /
  (panel3 | panel4) /
  footer +
  plot_layout(heights = c(1, 5.2, 5.2, 0.95)) 
# theme(plot.background = element_rect(fill = paper, colour = NA))

ggsave("premier_league_infographic.png", final,
       width = 13, height = 12.2, dpi = 150, device = ragg::agg_png)
message("Saved premier_league_infographic.png")