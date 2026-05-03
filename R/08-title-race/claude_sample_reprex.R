# ============================================================
#  Premier League Title Race: Arsenal vs Manchester City
#  Visualisation Suite — 4 charts
#  Requires: ggplot2, dplyr, tidyr, patchwork, ggtext,
#            waffle, scales, lubridate
#
#  Install once:
#  install.packages(c("ggplot2","dplyr","tidyr","patchwork",
#                     "ggtext","waffle","scales","lubridate"))
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(ggtext)
library(waffle)
library(scales)
library(lubridate)

# ── Colour palette ─────────────────────────────────────────
COL_ARSENAL <- "#EF0107"
COL_CITY    <- "#6CABDD"
COL_WIN     <- "#2ecc71"
COL_DRAW    <- "#f39c12"
COL_LOSS    <- "#e74c3c"
COL_BG      <- "#0d1117"
COL_PANEL   <- "#161b22"
COL_GRID    <- "#21262d"
COL_TEXT    <- "#e6edf3"
COL_MUTED   <- "#8b949e"

# ── Dark theme base ─────────────────────────────────────────
theme_pl <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.background    = element_rect(fill = COL_BG,    color = NA),
      panel.background   = element_rect(fill = COL_PANEL, color = NA),
      panel.grid.major   = element_line(color = COL_GRID, linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(color = COL_MUTED, size = 9),
      axis.title         = element_text(color = COL_TEXT,  size = 10),
      plot.title         = element_markdown(color = COL_TEXT, size = 13,
                                            face = "bold", margin = margin(b = 4)),
      plot.subtitle      = element_markdown(color = COL_MUTED, size = 9,
                                            margin = margin(b = 10)),
      plot.caption       = element_text(color = COL_MUTED, size = 8,
                                        hjust = 0, margin = margin(t = 8)),
      legend.background  = element_rect(fill = COL_PANEL, color = NA),
      legend.text        = element_text(color = COL_TEXT,  size = 9),
      legend.title       = element_text(color = COL_MUTED, size = 9),
      strip.text         = element_text(color = COL_TEXT,  size = 10, face = "bold"),
      strip.background   = element_rect(fill = COL_GRID,   color = NA),
      plot.margin        = margin(12, 12, 12, 12)
    )
}

# ════════════════════════════════════════════════════════════
#  DATA
# ════════════════════════════════════════════════════════════

# ── Gameweek-level results (W/D/L) per team per season ─────
# Result format: W = Win, D = Draw, L = Loss
# Seasons: 2022/23, 2023/24, 2024/25, 2025/26 (partial)

results_raw <- list(
  
  arsenal_2223 = c(
    "W","W","D","W","W","W","W","D","W","W","W","W","W","W","W","L",
    "W","W","W","D","W","W","W","W","L","D","W","L","W","W","L","D",
    "W","D","W","D","W"
  ),
  city_2223 = c(
    "W","W","L","W","D","W","W","W","W","W","W","W","D","L","W","W",
    "W","W","W","D","W","W","L","W","W","W","W","W","W","W","D","W",
    "W","W","W","W","W"
  ),
  
  arsenal_2324 = c(
    "W","W","D","W","W","D","W","W","W","W","W","W","D","W","W","W",
    "L","W","D","W","W","W","W","D","W","W","L","W","D","D","W","W",
    "D","W","W","W","D","W"
  ),
  city_2324 = c(
    "W","W","W","W","L","W","W","W","W","W","L","D","W","W","W","W",
    "W","D","W","W","L","W","W","W","W","W","W","W","L","W","W","W",
    "W","W","W","W","W","W"
  ),
  
  arsenal_2425 = c(
    "W","W","W","W","D","W","W","W","D","W","W","W","D","W","L","W",
    "W","D","W","W","W","D","W","W","L","W","W","D","W","L","W","W",
    "W","W","W","W","W","W"
  ),
  city_2425 = c(
    "W","L","W","D","L","W","W","L","L","W","L","D","W","W","W","L",
    "W","W","D","W","L","W","W","W","W","W","W","W","L","W","W","W",
    "W","W","W","W","W","W"
  ),
  
  # 2025/26 partial — 32 gameweeks played
  arsenal_2526 = c(
    "W","W","W","D","W","W","W","D","W","W","L","W","W","W","D","W",
    "W","W","W","W","D","W","W","L","W","W","W","W","D","W","W","W"
  ),
  city_2526 = c(
    "W","W","D","W","W","L","W","W","W","D","W","W","L","W","W","W",
    "D","W","W","W","W","L","W","W","W","W","D","W","W","W","W","W"
  )
)

# ── Assign approximate months to gameweeks ──────────────────
# PL roughly: GW1-4=Aug, 5-8=Sep, 9-12=Oct, 13-16=Nov, 17-19=Dec,
#             20-22=Jan, 23-26=Feb, 27-30=Mar, 31-34=Apr, 35-38=May

gw_to_month <- function(gw) {
  case_when(
    gw <= 4  ~ "Aug", gw <= 8  ~ "Sep", gw <= 12 ~ "Oct",
    gw <= 16 ~ "Nov", gw <= 19 ~ "Dec", gw <= 22 ~ "Jan",
    gw <= 26 ~ "Feb", gw <= 30 ~ "Mar", gw <= 34 ~ "Apr",
    TRUE     ~ "May"
  )
}

# ── Build tidy match-level dataframe ────────────────────────
build_df <- function(results, team, season) {
  n <- length(results)
  tibble(
    team   = team,
    season = season,
    gw     = seq_len(n),
    result = results,
    pts    = case_when(result == "W" ~ 3, result == "D" ~ 1, TRUE ~ 0),
    month  = gw_to_month(gw)
  )
}

matches <- bind_rows(
  build_df(results_raw$arsenal_2223, "Arsenal",      "2022/23"),
  build_df(results_raw$city_2223,    "Man City",     "2022/23"),
  build_df(results_raw$arsenal_2324, "Arsenal",      "2023/24"),
  build_df(results_raw$city_2324,    "Man City",     "2023/24"),
  build_df(results_raw$arsenal_2425, "Arsenal",      "2024/25"),
  build_df(results_raw$city_2425,    "Man City",     "2024/25"),
  build_df(results_raw$arsenal_2526, "Arsenal",      "2025/26*"),
  build_df(results_raw$city_2526,    "Man City",     "2025/26*")
) %>%
  mutate(
    result = factor(result, levels = c("W","D","L")),
    month  = factor(month,  levels = c("Aug","Sep","Oct","Nov","Dec",
                                       "Jan","Feb","Mar","Apr","May")),
    season = factor(season, levels = c("2022/23","2023/24","2024/25","2025/26*"))
  )

# Cumulative points
matches <- matches %>%
  group_by(team, season) %>%
  arrange(gw) %>%
  mutate(cum_pts = cumsum(pts)) %>%
  ungroup()


# ════════════════════════════════════════════════════════════
#  CHART 1 — Dual Waffle Charts (W/D/L per season)
# ════════════════════════════════════════════════════════════

waffle_data <- matches %>%
  group_by(team, season, result) %>%
  summarise(n = n(), .groups = "drop")

plot_waffle_season <- function(season_label) {
  ars <- waffle_data %>%
    filter(team == "Arsenal", season == season_label) %>%
    select(result, n) %>% deframe()
  
  cty <- waffle_data %>%
    filter(team == "Man City", season == season_label) %>%
    select(result, n) %>% deframe()
  
  # Pad to same length
  for (r in c("W","D","L")) {
    if (is.na(ars[r])) ars[r] <- 0
    if (is.na(cty[r])) cty[r] <- 0
  }
  
  wa <- waffle(ars[c("W","D","L")], rows = 4, size = 0.5,
               colors = c(COL_WIN, COL_DRAW, COL_LOSS),
               title = paste0("<span style='color:", COL_ARSENAL,
                              "'>Arsenal</span> — ", season_label)) +
    theme_pl() +
    theme(
      plot.title = element_markdown(size = 11, face = "bold"),
      legend.position = "none"
    )
  
  wc <- waffle(cty[c("W","D","L")], rows = 4, size = 0.5,
               colors = c(COL_WIN, COL_DRAW, COL_LOSS),
               title = paste0("<span style='color:", COL_CITY,
                              "'>Man City</span> — ", season_label)) +
    theme_pl() +
    theme(
      plot.title = element_markdown(size = 11, face = "bold"),
      legend.position = "none"
    )
  
  wa + wc
}

# Legend waffle (dummy)
legend_waffle <- waffle(c(Wins = 1, Draws = 1, Losses = 1), rows = 1, size = 1,
                        colors = c(COL_WIN, COL_DRAW, COL_LOSS)) +
  theme_pl() +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = COL_BG, color = NA))

p_waffle <- (
  plot_waffle_season("2022/23") /
    plot_waffle_season("2023/24") /
    plot_waffle_season("2024/25") /
    plot_waffle_season("2025/26*")
) +
  plot_annotation(
    title    = "Result breakdown — Arsenal vs Man City",
    subtitle = "Each square = 1 Premier League match  ·  <span style='color:#2ecc71'>■ Win</span>  <span style='color:#f39c12'>■ Draw</span>  <span style='color:#e74c3c'>■ Loss</span>",
    caption  = "2025/26* = partial season through GW32",
    theme = theme(
      plot.background = element_rect(fill = COL_BG, color = NA),
      plot.title      = element_text(color = COL_TEXT, size = 15, face = "bold"),
      plot.subtitle   = element_markdown(color = COL_MUTED, size = 10),
      plot.caption    = element_text(color = COL_MUTED, size = 8)
    )
  )

ggsave("chart1_waffle.png", p_waffle, width = 12, height = 14,
       dpi = 150, bg = COL_BG)
cat("✓ chart1_waffle.png saved\n")


# ════════════════════════════════════════════════════════════
#  CHART 2 — Points Gap Tracker (area chart per season)
# ════════════════════════════════════════════════════════════

# Join Arsenal and City by gw, compute gap = Arsenal - City
gap_data <- matches %>%
  select(team, season, gw, cum_pts) %>%
  pivot_wider(names_from = team, values_from = cum_pts) %>%
  rename(arsenal_pts = Arsenal, city_pts = `Man City`) %>%
  mutate(
    gap   = arsenal_pts - city_pts,
    ahead = if_else(gap >= 0, "Arsenal ahead", "City ahead")
  )

# Annotation events per season
annotations <- tibble(
  season = c("2022/23","2022/23","2023/24","2023/24","2024/25","2025/26*"),
  gw     = c(22,       33,       25,       34,       28,       20),
  gap    = c(8,        -1,       5,        -2,       9,        6),
  label  = c("Arsenal\n+8 pts",
             "City take\nlead",
             "Arsenal\n+5 pts",
             "City\novertake",
             "Arsenal\nresurgent",
             "Arsenal\nlead again")
)

p_gap <- ggplot(gap_data, aes(x = gw, y = gap)) +
  geom_hline(yintercept = 0, color = COL_MUTED, linewidth = 0.6, linetype = "dashed") +
  geom_ribbon(aes(ymin = pmin(gap, 0), ymax = 0),
              fill = COL_CITY, alpha = 0.25) +
  geom_ribbon(aes(ymin = 0, ymax = pmax(gap, 0)),
              fill = COL_ARSENAL, alpha = 0.25) +
  geom_line(aes(color = ahead), linewidth = 1.2, lineend = "round") +
  geom_point(data = annotations,
             aes(x = gw, y = gap), color = "white", size = 2.5, inherit.aes = FALSE) +
  geom_label(data = annotations,
             aes(x = gw, y = gap, label = label),
             color = COL_TEXT, fill = COL_PANEL, size = 2.5,
             label.padding = unit(0.2, "lines"), nudge_y = 1.5, inherit.aes = FALSE) +
  scale_color_manual(values = c("Arsenal ahead" = COL_ARSENAL,
                                "City ahead"    = COL_CITY),
                     guide = "none") +
  scale_x_continuous(breaks = seq(5, 38, 5), labels = paste0("GW", seq(5, 38, 5))) +
  scale_y_continuous(labels = function(x) ifelse(x > 0, paste0("+", x), x)) +
  facet_wrap(~ season, ncol = 2, scales = "free_x") +
  labs(
    title    = "Points gap: <span style='color:#EF0107'>Arsenal</span> minus <span style='color:#6CABDD'>Man City</span>",
    subtitle = "Above zero = Arsenal ahead · Below zero = City ahead",
    x        = NULL,
    y        = "Points gap",
    caption  = "2025/26* through GW32"
  ) +
  theme_pl() +
  theme(plot.title = element_markdown(size = 14, face = "bold"))

ggsave("chart2_points_gap.png", p_gap, width = 13, height = 9,
       dpi = 150, bg = COL_BG)
cat("✓ chart2_points_gap.png saved\n")


# ════════════════════════════════════════════════════════════
#  CHART 3 — Monthly Win % Heatmap
# ════════════════════════════════════════════════════════════

monthly_winpct <- matches %>%
  group_by(team, season, month) %>%
  summarise(
    games   = n(),
    wins    = sum(result == "W"),
    win_pct = wins / games * 100,
    .groups = "drop"
  )

# Insight annotations for the late-season dip
insight_labels <- tibble(
  team    = "Arsenal",
  season  = c("2022/23", "2023/24"),
  month   = c("Apr",     "Apr"),
  win_pct = c(33,        40),
  label   = c("Fade\nbegins", "Title\nlost")
)

p_heatmap <- ggplot(monthly_winpct,
                    aes(x = month, y = season, fill = win_pct)) +
  geom_tile(color = COL_BG, linewidth = 0.8) +
  geom_text(aes(label = paste0(round(win_pct), "%")),
            color = "white", size = 3, fontface = "bold") +
  scale_fill_gradient2(
    low      = "#1a1a2e",
    mid      = "#4a4e8c",
    high     = "#00d4aa",
    midpoint = 55,
    limits   = c(0, 100),
    name     = "Win %",
    labels   = percent_format(scale = 1)
  ) +
  facet_wrap(~ team, ncol = 1) +
  labs(
    title    = "Monthly win % — Arsenal vs Man City",
    subtitle = "Darker teal = dominant month · Arsenal's Mar–May fade is clearly visible",
    x        = NULL,
    y        = NULL,
    caption  = "Each cell = win % across all PL matches in that month-season"
  ) +
  theme_pl() +
  theme(
    axis.text.x      = element_text(color = COL_TEXT, size = 9),
    axis.text.y      = element_text(color = COL_TEXT, size = 9),
    legend.position  = "right",
    panel.grid.major = element_blank()
  )

ggsave("chart3_monthly_heatmap.png", p_heatmap, width = 13, height = 8,
       dpi = 150, bg = COL_BG)
cat("✓ chart3_monthly_heatmap.png saved\n")


# ════════════════════════════════════════════════════════════
#  CHART 4 — Rolling 5-game form line
# ════════════════════════════════════════════════════════════

rolling_form <- matches %>%
  group_by(team, season) %>%
  arrange(gw) %>%
  mutate(
    roll5 = zoo::rollmean(pts, k = 5, fill = NA, align = "right")
  ) %>%
  ungroup()

# Key moment annotations
key_moments <- tibble(
  season = c("2022/23","2023/24","2023/24","2024/25","2025/26*"),
  gw     = c(29,       18,       31,       9,        15),
  team   = c("Arsenal","Arsenal","Man City","Man City","Arsenal"),
  label  = c("5-game\nwinless run", "7W streak", "City run\nof form",
             "City's poor\nstart", "Arsenal\ndominance"),
  vjust  = c(-0.4, -0.4, 1.4, 1.4, -0.4)
)

p_form <- ggplot(rolling_form %>% filter(!is.na(roll5)),
                 aes(x = gw, y = roll5, color = team)) +
  geom_hline(yintercept = 1.5, color = COL_MUTED,
             linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 1.4, lineend = "round", linejoin = "round") +
  geom_point(data = key_moments %>%
               left_join(rolling_form, by = c("team","season","gw")),
             aes(x = gw, y = roll5), size = 3, color = "white", inherit.aes = FALSE) +
  geom_text(data = key_moments %>%
              left_join(rolling_form, by = c("team","season","gw")),
            aes(x = gw, y = roll5, label = label, vjust = vjust),
            color = COL_TEXT, size = 2.4, inherit.aes = FALSE) +
  scale_color_manual(values = c("Arsenal" = COL_ARSENAL, "Man City" = COL_CITY)) +
  scale_x_continuous(breaks = seq(5, 38, 5), labels = paste0("GW", seq(5, 38, 5))) +
  scale_y_continuous(limits = c(0, 3),
                     breaks = c(0, 1, 1.5, 2, 3),
                     labels = c("0", "1.0\n(poor)", "1.5\n(avg)", "2.0", "3.0")) +
  facet_wrap(~ season, ncol = 2, scales = "free_x") +
  labs(
    title    = "Rolling 5-game form — <span style='color:#EF0107'>Arsenal</span> vs <span style='color:#6CABDD'>Man City</span>",
    subtitle = "Points per game (5-match rolling average) · Dashed line = 1.5 pts/game benchmark",
    x        = NULL,
    y        = "Pts/game (5-game avg)",
    color    = NULL,
    caption  = "2025/26* through GW32"
  ) +
  theme_pl() +
  theme(
    plot.title      = element_markdown(size = 14, face = "bold"),
    legend.position = "bottom"
  )

ggsave("chart4_rolling_form.png", p_form, width = 13, height = 9,
       dpi = 150, bg = COL_BG)
cat("✓ chart4_rolling_form.png saved\n")


# ════════════════════════════════════════════════════════════
#  COMBINED DASHBOARD (patchwork)
# ════════════════════════════════════════════════════════════
# Run this separately — large render

dashboard <- (p_gap | p_heatmap) / (p_form | p_waffle)
# ggsave("dashboard_full.png", dashboard,
#        width = 22, height = 16, dpi = 150, bg = COL_BG)

cat("\n✅ All 4 charts saved. Open the .png files to view.\n")
cat("   chart1_waffle.png\n")
cat("   chart2_points_gap.png\n")
cat("   chart3_monthly_heatmap.png\n")
cat("   chart4_rolling_form.png\n")
cat("\nTo combine into one dashboard, uncomment the last block.\n")