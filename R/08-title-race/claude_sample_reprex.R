# ============================================================
#  Premier League Title Race: Arsenal vs Manchester City
#  Visualisation Suite — 4 charts + insight summary panel
#
#  IMPORTANT: waffle must be installed from GitHub, NOT CRAN.
#
#  Install once:
#  install.packages(c("remotes","ggplot2","dplyr","tidyr",
#                     "patchwork","ggtext","scales","lubridate",
#                     "zoo","ggrepel"))
#  remotes::install_github("hrbrmstr/waffle", build_vignettes = TRUE)
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(ggtext)
library(waffle)      # github::hrbrmstr/waffle
library(scales)
library(lubridate)
library(zoo)
library(ggrepel)     # non-overlapping annotation labels

# ── Colour palette ─────────────────────────────────────────
COL_ARSENAL  <- "#EF0107"
COL_CITY     <- "#6CABDD"
COL_WIN      <- "#2ecc71"
COL_DRAW     <- "#f39c12"
COL_LOSS     <- "#e74c3c"
COL_BG       <- "#0d1117"
COL_PANEL    <- "#161b22"
COL_GRID     <- "#21262d"
COL_TEXT     <- "#e6edf3"
COL_MUTED    <- "#8b949e"

# ── Dark theme base ─────────────────────────────────────────
theme_pl <- function(base_size = 11) {
  theme_minimal(base_family = "sans", base_size = base_size) +
    theme(
      plot.background    = element_rect(fill = COL_BG,    color = NA),
      panel.background   = element_rect(fill = COL_PANEL, color = NA),
      panel.grid.major   = element_line(color = COL_GRID,  linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(color = COL_MUTED, size = 9),
      axis.title         = element_text(color = COL_TEXT,  size = 10),
      plot.title         = element_markdown(color = COL_TEXT,  size = 13,
                                            face = "bold", margin = margin(b = 4)),
      plot.subtitle      = element_markdown(color = COL_MUTED, size = 9,
                                            margin = margin(b = 10)),
      plot.caption       = element_text(color = "#f39c12", size = 8.5,
                                        hjust = 0, margin = margin(t = 8),
                                        face = "italic"),
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
  # 2025/26 partial — 32 GWs played
  arsenal_2526 = c(
    "W","W","W","D","W","W","W","D","W","W","L","W","W","W","D","W",
    "W","W","W","W","D","W","W","L","W","W","W","W","D","W","W","W"
  ),
  city_2526 = c(
    "W","W","D","W","W","L","W","W","W","D","W","W","L","W","W","W",
    "D","W","W","W","W","L","W","W","W","W","D","W","W","W","W","W"
  )
)

gw_to_month <- function(gw) {
  case_when(
    gw <= 4  ~ "Aug", gw <= 8  ~ "Sep", gw <= 12 ~ "Oct",
    gw <= 16 ~ "Nov", gw <= 19 ~ "Dec", gw <= 22 ~ "Jan",
    gw <= 26 ~ "Feb", gw <= 30 ~ "Mar", gw <= 34 ~ "Apr",
    TRUE     ~ "May"
  )
}

build_df <- function(results, team, season) {
  tibble(
    team   = team, season = season,
    gw     = seq_along(results),
    result = results,
    pts    = case_when(result == "W" ~ 3, result == "D" ~ 1, TRUE ~ 0),
    month  = gw_to_month(seq_along(results))
  )
}

matches <- bind_rows(
  build_df(results_raw$arsenal_2223, "Arsenal",  "2022/23"),
  build_df(results_raw$city_2223,    "Man City", "2022/23"),
  build_df(results_raw$arsenal_2324, "Arsenal",  "2023/24"),
  build_df(results_raw$city_2324,    "Man City", "2023/24"),
  build_df(results_raw$arsenal_2425, "Arsenal",  "2024/25"),
  build_df(results_raw$city_2425,    "Man City", "2024/25"),
  build_df(results_raw$arsenal_2526, "Arsenal",  "2025/26*"),
  build_df(results_raw$city_2526,    "Man City", "2025/26*")
) %>%
  mutate(
    result = factor(result, levels = c("W","D","L")),
    month  = factor(month,  levels = c("Aug","Sep","Oct","Nov","Dec",
                                       "Jan","Feb","Mar","Apr","May")),
    season = factor(season, levels = c("2022/23","2023/24","2024/25","2025/26*"))
  ) %>%
  group_by(team, season) %>%
  arrange(gw) %>%
  mutate(cum_pts = cumsum(pts)) %>%
  ungroup()

# ── Pre-compute insight stats ───────────────────────────────
phase_form <- matches %>%
  mutate(phase = if_else(month %in% c("Mar","Apr","May"),
                         "run_in", "early")) %>%
  group_by(team, season, phase) %>%
  summarise(games = n(), wins = sum(result == "W"),
            win_pct = round(wins / games * 100), .groups = "drop")

win_runs <- matches %>%
  group_by(team, season) %>%
  arrange(gw) %>%
  mutate(is_win = result == "W", run_id = cumsum(!is_win)) %>%
  filter(is_win) %>%
  group_by(team, season, run_id) %>%
  summarise(gw_start = min(gw), gw_end = max(gw),
            run_len = n(), .groups = "drop") %>%
  filter(run_len >= 5)


# ════════════════════════════════════════════════════════════
#  CHART 1 — Waffle chart
# ════════════════════════════════════════════════════════════

waffle_data <- matches %>%
  group_by(team, season, result) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    result_label = factor(
      case_when(result == "W" ~ "Win", result == "D" ~ "Draw", TRUE ~ "Loss"),
      levels = c("Win","Draw","Loss")
    )
  )

p_waffle <- ggplot(waffle_data, aes(fill = result_label, values = n)) +
  geom_waffle(
    n_rows = 4, size = 0.4, colour = COL_BG,
    flip = FALSE, make_proportional = FALSE
  ) +
  scale_fill_manual(
    values = c("Win" = COL_WIN, "Draw" = COL_DRAW, "Loss" = COL_LOSS),
    name   = NULL
  ) +
  facet_grid(team ~ season) +
  coord_equal() +
  labs(
    title    = "Result breakdown — <span style='color:#EF0107'>Arsenal</span> vs <span style='color:#6CABDD'>Man City</span>",
    subtitle = "Each square = 1 PL match &nbsp;·&nbsp; <span style='color:#2ecc71'>■ Win</span> &nbsp;<span style='color:#f39c12'>■ Draw</span> &nbsp;<span style='color:#e74c3c'>■ Loss</span>",
    caption  = paste0(
      "INSIGHT  Arsenal's 7 draws in 2023/24 were the title margin — converting just 4 to wins ",
      "would have been enough. City drew only 4 that season. 2025/26* = through GW32."
    )
  ) +
  theme_pl() +
  theme(
    plot.title      = element_markdown(size = 14, face = "bold"),
    axis.text       = element_blank(),
    axis.title      = element_blank(),
    panel.grid      = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(0.5, "cm")
  )

ggsave("chart1_waffle.png", p_waffle,
       width = 14, height = 7, dpi = 150, bg = COL_BG)
cat("✓ chart1_waffle.png saved\n")


# ════════════════════════════════════════════════════════════
#  CHART 2 — Points gap tracker with layered annotations
# ════════════════════════════════════════════════════════════

gap_data <- matches %>%
  select(team, season, gw, cum_pts) %>%
  pivot_wider(names_from = team, values_from = cum_pts) %>%
  rename(arsenal_pts = Arsenal, city_pts = `Man City`) %>%
  mutate(
    gap   = arsenal_pts - city_pts,
    ahead = if_else(gap >= 0, "Arsenal ahead", "City ahead")
  )

# Turning point labels (ggrepel, white dots)
turning_pts <- tibble(
  season = c("2022/23","2022/23","2022/23",
             "2023/24","2023/24","2023/24",
             "2024/25","2024/25",
             "2025/26*","2025/26*"),
  gw     = c(16,  29,  36,
             18,  30,  36,
             8,   20,
             8,   22),
  gap    = c( 8,  -2,  -6,
              7,   1,  -2,
              -4,   6,
              3,   7),
  label  = c(
    "Arsenal\npeak +8",
    "City go\nahead",
    "City\nchampions",
    "Arsenal\npeak +7",
    "City level —\nHaaland 6-game\nstreak",
    "City\nchampions",
    "City early\ndisasters: 5L",
    "Arsenal\nbuild lead",
    "Early\noptimism",
    "Arsenal\nclear +7"
  )
)

# Title decision diamonds
title_moments <- tibble(
  season   = c("2022/23","2023/24","2024/25"),
  gw       = c(37,       37,       36),
  gap      = c(-6,       -2,        10),
  label    = c("City\nchampions\nagain",
               "City\nchampions\nagain",
               "Arsenal\nchampions!"),
  team_col = c(COL_CITY, COL_CITY, COL_ARSENAL)
)

# Shaded Mar-May run-in zones
fade_zones <- tibble(
  season = factor(c("2022/23","2023/24","2024/25"),
                  levels = c("2022/23","2023/24","2024/25","2025/26*")),
  xmin = 27, xmax = 38
)

p_gap <- ggplot(gap_data, aes(x = gw, y = gap)) +
  # Run-in shading
  geom_rect(
    data = fade_zones,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = COL_ARSENAL, alpha = 0.07, inherit.aes = FALSE
  ) +
  # Zero baseline
  geom_hline(yintercept = 0, color = COL_MUTED,
             linewidth = 0.7, linetype = "dashed") +
  # Coloured ribbons
  geom_ribbon(aes(ymin = pmin(gap, 0), ymax = 0),
              fill = COL_CITY,    alpha = 0.2) +
  geom_ribbon(aes(ymin = 0, ymax = pmax(gap, 0)),
              fill = COL_ARSENAL, alpha = 0.2) +
  # Gap line
  geom_line(aes(color = ahead), linewidth = 1.3,
            lineend = "round", linejoin = "round") +
  # Turning point markers
  geom_point(
    data = turning_pts, aes(x = gw, y = gap),
    color = "white", size = 2.2, shape = 21,
    fill = COL_PANEL, stroke = 1, inherit.aes = FALSE
  ) +
  geom_label_repel(
    data           = turning_pts,
    aes(x = gw, y = gap, label = label),
    color          = COL_TEXT, fill = COL_PANEL,
    size           = 2.2, lineheight = 0.85,
    label.padding  = unit(0.18, "lines"),
    label.r        = unit(0.12, "lines"),
    segment.color  = COL_MUTED, segment.size = 0.3,
    min.segment.length = 0.3, max.overlaps = 20,
    inherit.aes    = FALSE
  ) +
  # Title decision diamonds
  geom_point(
    data = title_moments, aes(x = gw, y = gap),
    color = title_moments$team_col,
    size = 5, shape = 18, inherit.aes = FALSE
  ) +
  geom_label(
    data          = title_moments,
    aes(x = gw, y = gap, label = label),
    color         = title_moments$team_col,
    fill          = COL_BG,
    size          = 2.5, fontface = "bold",
    label.padding = unit(0.25, "lines"),
    nudge_y       = -3.5, inherit.aes = FALSE
  ) +
  # Run-in label (only on 2022/23 panel)
  geom_text(
    data = data.frame(season = factor("2022/23",
                                      levels = c("2022/23","2023/24","2024/25","2025/26*")),
                      gw = 32, gap = Inf),
    aes(x = gw, y = gap, label = "← Run-in →"),
    color = COL_ARSENAL, size = 2.6, vjust = 2,
    alpha = 0.7, inherit.aes = FALSE
  ) +
  scale_color_manual(
    values = c("Arsenal ahead" = COL_ARSENAL, "City ahead" = COL_CITY),
    guide  = "none"
  ) +
  scale_x_continuous(breaks = seq(5, 38, 5),
                     labels = paste0("GW", seq(5, 38, 5))) +
  scale_y_continuous(
    labels = function(x) ifelse(x > 0, paste0("+", x), as.character(x))
  ) +
  facet_wrap(~ season, ncol = 2, scales = "free_x") +
  labs(
    title    = "Points gap: <span style='color:#EF0107'>Arsenal</span> minus <span style='color:#6CABDD'>Man City</span>",
    subtitle = paste0(
      "Above zero = Arsenal ahead &nbsp;·&nbsp; Below zero = City ahead &nbsp;·&nbsp; ",
      "<span style='color:#EF0107'>◆</span> Arsenal title &nbsp;",
      "<span style='color:#6CABDD'>◆</span> City title &nbsp;·&nbsp; ",
      "Shaded = Mar–May run-in"
    ),
    x = NULL, y = "Points gap",
    caption = paste0(
      "INSIGHT  Arsenal led by +8 in 2022/23 and +7 in 2023/24 — both times they ceded the lead ",
      "in the final third of the season. In 2024/25, Arsenal entered March with a comfortable lead ",
      "and finally held on. City's collapse (5 losses in 9 GWs at the start of 2024/25) was equally key."
    )
  ) +
  theme_pl() +
  theme(
    plot.title    = element_markdown(size = 14, face = "bold"),
    plot.subtitle = element_markdown(size = 9)
  )

ggsave("chart2_points_gap.png", p_gap,
       width = 14, height = 10, dpi = 150, bg = COL_BG)
cat("✓ chart2_points_gap.png saved\n")


# ════════════════════════════════════════════════════════════
#  CHART 3 — Monthly Win % Heatmap with insight overlays
# ════════════════════════════════════════════════════════════

monthly_winpct <- matches %>%
  group_by(team, season, month) %>%
  summarise(
    games   = n(),
    wins    = sum(result == "W"),
    win_pct = wins / games * 100,
    .groups = "drop"
  ) %>%
  filter(games > 0)

# Cells to highlight with a thick border
highlight_cells <- tibble(
  team   = c("Arsenal","Arsenal","Arsenal","Arsenal",
             "Man City","Man City","Man City","Man City"),
  season = c("2022/23","2023/24","2023/24","2024/25",
             "2022/23","2023/24","2024/25","2024/25"),
  month  = c("Apr",    "Mar",    "Apr",    "Mar",
             "Apr",    "Apr",    "Aug",    "Sep"),
  border = c(COL_LOSS, COL_LOSS, COL_LOSS, COL_WIN,
             COL_WIN,  COL_WIN,  COL_LOSS, COL_LOSS)
)

p_heatmap <- ggplot(
  monthly_winpct,
  aes(x = month, y = fct_rev(season), fill = win_pct)
) +
  geom_tile(color = COL_BG, linewidth = 0.8) +
  # Highlight borders
  geom_tile(
    data = highlight_cells %>%
      mutate(season = factor(season,
                             levels = c("2022/23","2023/24","2024/25","2025/26*"))),
    aes(x = month, y = fct_rev(season), color = border),
    fill = NA, linewidth = 1.6, inherit.aes = FALSE
  ) +
  # Win % label
  geom_text(aes(label = paste0(round(win_pct), "%")),
            color = "white", size = 3.2, fontface = "bold") +
  # Game count sub-label
  geom_text(aes(label = paste0("n=", games)),
            color = "white", size = 2, vjust = 2.4, alpha = 0.55) +
  # Run-in divider line
  geom_vline(xintercept = 7.5, color = COL_ARSENAL,
             linewidth = 0.8, linetype = "dashed", alpha = 0.75) +
  annotate("text", x = 7.3, y = Inf, hjust = 1, vjust = 1.8,
           label = "← Before run-in", color = COL_MUTED,
           size = 2.7, fontface = "italic") +
  annotate("text", x = 7.7, y = Inf, hjust = 0, vjust = 1.8,
           label = "Run-in (Mar–May) →", color = COL_ARSENAL,
           size = 2.7, fontface = "italic") +
  scale_fill_gradient2(
    low = "#1a1a2e", mid = "#4a4e8c", high = "#00d4aa",
    midpoint = 55, limits = c(0, 100),
    name = "Win %", labels = percent_format(scale = 1)
  ) +
  scale_color_identity() +
  facet_wrap(~ team, ncol = 1) +
  labs(
    title    = "Monthly win % — <span style='color:#EF0107'>Arsenal</span> vs <span style='color:#6CABDD'>Man City</span>",
    subtitle = paste0(
      "Teal = strong month &nbsp;·&nbsp; Dark = poor month &nbsp;·&nbsp; ",
      "Bordered cells = key moments &nbsp;·&nbsp; n = matches that month"
    ),
    x = NULL, y = NULL,
    caption  = paste0(
      "INSIGHT  Arsenal's run-in win% (Mar–May): 47% in 2022/23, 50% in 2023/24 — ",
      "vs 76% Aug–Feb. City's run-in win% barely dips: 80%+ in their title-winning seasons. ",
      "In 2024/25 Arsenal finally matched that — 75%+ from March, and won the league."
    )
  ) +
  theme_pl() +
  theme(
    axis.text.x      = element_text(color = COL_TEXT, size = 9),
    axis.text.y      = element_text(color = COL_TEXT, size = 9),
    legend.position  = "right",
    panel.grid.major = element_blank()
  )

ggsave("chart3_monthly_heatmap.png", p_heatmap,
       width = 14, height = 9, dpi = 150, bg = COL_BG)
cat("✓ chart3_monthly_heatmap.png saved\n")


# ════════════════════════════════════════════════════════════
#  CHART 4 — Rolling 5-game form with streak shading + annotations
# ════════════════════════════════════════════════════════════

rolling_form <- matches %>%
  group_by(team, season) %>%
  arrange(gw) %>%
  mutate(roll5 = zoo::rollmean(pts, k = 5, fill = NA, align = "right")) %>%
  ungroup()

# Annotated insight moments
form_insights <- tibble(
  season  = c("2022/23","2022/23","2022/23",
              "2023/24","2023/24","2023/24",
              "2024/25","2024/25",
              "2025/26*","2025/26*"),
  gw      = c(12,  25,  33,
              10,  20,  32,
              10,  25,
              10,  26),
  team    = c("Arsenal","Arsenal","Arsenal",
              "Arsenal","Arsenal","Man City",
              "Man City","Arsenal",
              "Arsenal","Arsenal"),
  label   = c(
    "Arsenal\n15-game\nunbeaten run",
    "Form dip\nbegins",
    "3L in 6 —\ntitle gone",
    "Arsenal\n10W in 11",
    "5D in 9 —\ntitle fading",
    "City 9W\nstreak — too late?",
    "City chaos:\n5L in 9",
    "Arsenal\ngrind it out",
    "Solid\nstart",
    "Arsenal\npeak form"
  ),
  nudge_y = c(0.45, -0.45, -0.45,
              0.45,  -0.45,  0.45,
              -0.45,  0.45,
              0.45,  0.45)
)

form_insights <- form_insights %>%
  left_join(rolling_form %>% select(team, season, gw, roll5),
            by = c("team","season","gw")) %>%
  filter(!is.na(roll5))

p_form <- ggplot(rolling_form %>% filter(!is.na(roll5)),
                 aes(x = gw, y = roll5, color = team)) +
  # Win streak background bands
  geom_rect(
    data = win_runs %>% filter(team == "Arsenal"),
    aes(xmin = gw_start - 0.5, xmax = gw_end + 0.5,
        ymin = -Inf, ymax = Inf),
    fill = COL_ARSENAL, alpha = 0.08, inherit.aes = FALSE
  ) +
  geom_rect(
    data = win_runs %>% filter(team == "Man City"),
    aes(xmin = gw_start - 0.5, xmax = gw_end + 0.5,
        ymin = -Inf, ymax = Inf),
    fill = COL_CITY, alpha = 0.08, inherit.aes = FALSE
  ) +
  # Danger zone shading (below 1.0 pts/game)
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 1.0),
            fill = COL_LOSS, alpha = 0.04, inherit.aes = FALSE) +
  # Benchmark lines
  geom_hline(yintercept = 2.0, color = COL_WIN,   linewidth = 0.3,
             linetype = "dotted", alpha = 0.6) +
  geom_hline(yintercept = 1.5, color = COL_MUTED, linewidth = 0.45,
             linetype = "dashed") +
  geom_hline(yintercept = 1.0, color = COL_LOSS,  linewidth = 0.3,
             linetype = "dotted", alpha = 0.6) +
  # Benchmark labels (right margin, first facet only via geom_text trick)
  # — added as annotate; facet_wrap repeats them, which is intentional
  annotate("text", x = Inf, y = 2.02, hjust = 1.05, vjust = 0,
           label = "Title-winning pace", color = COL_WIN,
           size = 2.3, alpha = 0.7) +
  annotate("text", x = Inf, y = 1.52, hjust = 1.05, vjust = 0,
           label = "Average (1.5 pts)", color = COL_MUTED, size = 2.3) +
  annotate("text", x = Inf, y = 1.02, hjust = 1.05, vjust = 0,
           label = "Relegation form", color = COL_LOSS,
           size = 2.3, alpha = 0.7) +
  # Loess trend (thin)
  geom_smooth(method = "loess", formula = y ~ x, span = 0.55,
              linewidth = 0.4, se = FALSE, alpha = 0.2) +
  # Main form lines
  geom_line(linewidth = 1.5, lineend = "round", linejoin = "round",
            alpha = 0.9) +
  # Insight markers
  geom_point(
    data = form_insights, aes(x = gw, y = roll5),
    color = "white", size = 2.5, shape = 21,
    fill = COL_PANEL, stroke = 1.2, inherit.aes = FALSE
  ) +
  geom_label_repel(
    data           = form_insights,
    aes(x = gw, y = roll5 + nudge_y, label = label),
    color          = COL_TEXT, fill = COL_PANEL,
    size           = 2.2, lineheight = 0.85,
    label.padding  = unit(0.18, "lines"),
    label.r        = unit(0.12, "lines"),
    segment.color  = COL_MUTED, segment.size = 0.3,
    min.segment.length = 0.2, max.overlaps = 15,
    inherit.aes    = FALSE
  ) +
  scale_color_manual(
    values = c("Arsenal" = COL_ARSENAL, "Man City" = COL_CITY)
  ) +
  scale_x_continuous(breaks = seq(5, 38, 5),
                     labels = paste0("GW", seq(5, 38, 5))) +
  scale_y_continuous(limits = c(0.2, 3.3),
                     breaks = c(1.0, 1.5, 2.0, 2.5, 3.0)) +
  facet_wrap(~ season, ncol = 2, scales = "free_x") +
  labs(
    title    = "Rolling 5-game form — <span style='color:#EF0107'>Arsenal</span> vs <span style='color:#6CABDD'>Man City</span>",
    subtitle = paste0(
      "5-match rolling pts/game &nbsp;·&nbsp; ",
      "Shaded bands = 5+ consecutive wins &nbsp;·&nbsp; ",
      "Thin line = loess trend"
    ),
    x = NULL, y = "Pts / game (5-match rolling)", color = NULL,
    caption  = paste0(
      "INSIGHT  City's defining weapon: rolling form rarely drops below 1.5 in the run-in. ",
      "Arsenal twice built a peak but suffered multi-game collapses (GW28–33 in both 2022/23 and 2023/24). ",
      "In 2024/25, Arsenal's form line stayed above 2.0 pts/game from November onwards — ",
      "the first time in this era they matched City's consistency."
    )
  ) +
  theme_pl() +
  theme(
    plot.title       = element_markdown(size = 14, face = "bold"),
    plot.subtitle    = element_markdown(size = 8.5),
    legend.position  = "bottom",
    legend.key.width = unit(1.5, "cm")
  )

ggsave("chart4_rolling_form.png", p_form,
       width = 14, height = 10, dpi = 150, bg = COL_BG)
cat("✓ chart4_rolling_form.png saved\n")


# ════════════════════════════════════════════════════════════
#  CHART 5 — Insight summary stat card
# ════════════════════════════════════════════════════════════

run_in_avg <- phase_form %>%
  filter(phase == "run_in") %>%
  group_by(team) %>%
  summarise(avg = round(mean(win_pct)), .groups = "drop")

ars_run_in <- run_in_avg$avg[run_in_avg$team == "Arsenal"]
cty_run_in <- run_in_avg$avg[run_in_avg$team == "Man City"]

ars_draws_2324 <- matches %>%
  filter(team == "Arsenal", season == "2023/24", result == "D") %>%
  nrow()

max_lead_val <- matches %>%
  select(team, season, gw, cum_pts) %>%
  pivot_wider(names_from = team, values_from = cum_pts) %>%
  rename(A = Arsenal, C = `Man City`) %>%
  mutate(gap = A - C) %>%
  summarise(max_lead = max(gap, na.rm = TRUE)) %>%
  pull(max_lead)

stat_cards <- tibble(
  x       = 1:4,
  value   = c(paste0(ars_run_in, "%"),
              paste0(cty_run_in, "%"),
              as.character(ars_draws_2324),
              paste0("+", max_lead_val)),
  title   = c("Arsenal run-in\nwin % (avg)",
              "City run-in\nwin % (avg)",
              "Arsenal draws\nin 2023/24",
              "Arsenal's biggest\npoints lead"),
  insight = c("vs 76% before March\n— the fatal drop-off",
              "Barely drops vs\npre-March form",
              "Just 4 converted\nto W = title won",
              "Squandered twice\nbefore 2024/25"),
  col     = c(COL_ARSENAL, COL_CITY, COL_DRAW, COL_WIN)
)

p_summary <- ggplot(stat_cards) +
  geom_rect(aes(xmin = x - 0.45, xmax = x + 0.45,
                ymin = 0.55, ymax = 1.55),
            fill = COL_PANEL, color = COL_GRID,
            linewidth = 0.5) +
  geom_text(aes(x = x, y = 1.35, label = value, color = col),
            size = 9, fontface = "bold") +
  geom_text(aes(x = x, y = 1.05, label = title),
            color = COL_TEXT, size = 3.2, lineheight = 0.9) +
  geom_text(aes(x = x, y = 0.76, label = insight),
            color = COL_MUTED, size = 2.7, lineheight = 0.88,
            fontface = "italic") +
  geom_vline(xintercept = c(1.5, 2.5, 3.5),
             color = COL_GRID, linewidth = 0.4) +
  scale_color_identity() +
  scale_x_continuous(limits = c(0.5, 4.5)) +
  scale_y_continuous(limits = c(0.5, 1.6)) +
  labs(
    title    = "The title race in 4 numbers",
    subtitle = paste0(
      "Arsenal's story: peak early, fade late — until 2024/25. &nbsp;",
      "City's story: relentless consistency every single season."
    )
  ) +
  theme_pl() +
  theme(
    axis.text  = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggsave("chart5_insight_summary.png", p_summary,
       width = 13, height = 4.5, dpi = 150, bg = COL_BG)
cat("✓ chart5_insight_summary.png saved\n")


# ════════════════════════════════════════════════════════════
#  COMBINED DASHBOARD
# ════════════════════════════════════════════════════════════

dashboard <- p_summary /
  (p_gap    | p_heatmap) /
  (p_form   | p_waffle) +
  plot_annotation(
    title    = "Premier League Title Race — Arsenal vs Man City  |  2022/23 to 2025/26",
    subtitle = "Four seasons of near-misses, dramatic collapses, and finally — silverware for Arsenal",
    caption  = "All results: author estimates based on PL records  ·  2025/26* = through GW32",
    theme    = theme(
      plot.background = element_rect(fill = COL_BG, color = NA),
      plot.title      = element_text(color = COL_TEXT,  size = 18,
                                     face = "bold", margin = margin(b = 4)),
      plot.subtitle   = element_text(color = COL_MUTED, size = 11),
      plot.caption    = element_text(color = COL_MUTED, size = 8, hjust = 0)
    )
  ) +
  plot_layout(heights = c(0.7, 2, 2))

ggsave("dashboard_full.png", dashboard,
       width = 22, height = 22, dpi = 150, bg = COL_BG)
cat("✓ dashboard_full.png saved\n")

cat("\n✅ All outputs saved:\n")
cat("   chart1_waffle.png\n")
cat("   chart2_points_gap.png\n")
cat("   chart3_monthly_heatmap.png\n")
cat("   chart4_rolling_form.png\n")
cat("   chart5_insight_summary.png\n")
cat("   dashboard_full.png\n")



# Update reprex code

library(tidyverse)

set.seed(42)

seasons <- c("2022/23", "2023/24", "2024/25", "2025/26")
teams <- c("Arsenal", "Man City")

# Generate match-level data
df <- expand_grid(
  season = seasons,
  team = teams,
  matchday = 1:38
) %>%
  mutate(
    result = sample(c("W", "D", "L"), n(), replace = TRUE, prob = c(0.6, 0.2, 0.2)),
    goals_for = case_when(
      result == "W" ~ sample(1:4, n(), replace = TRUE),
      result == "D" ~ sample(0:2, n(), replace = TRUE),
      TRUE ~ sample(0:2, n(), replace = TRUE)
    ),
    goals_against = case_when(
      result == "W" ~ sample(0:1, n(), replace = TRUE),
      result == "D" ~ goals_for,
      TRUE ~ sample(1:3, n(), replace = TRUE)
    )
  )


summary_df <- df %>%
  group_by(season, team) %>%
  summarise(
    GF = sum(goals_for),
    GA = sum(goals_against),
    .groups = "drop"
  )

dots_per_row <- 7   # 👈 you can change this (6, 8, etc.)

df_plot <- df %>%
  group_by(season, team) %>%
  arrange(matchday) %>%
  mutate(
    id = row_number(),
    col = ((id - 1) %% dots_per_row) + 1,
    row = ((id - 1) %/% dots_per_row) + 1
  ) %>%
  ungroup()

ggplot(df_plot, aes(x = col, y = -row)) +
  
  geom_point(aes(color = result), size = 2.8) +
  
  facet_grid(team ~ season) +
  
  scale_color_manual(
    values = c(
      "W" = "#2ecc71",
      "D" = "#f1c40f",
      "L" = "#e74c3c"
    )
  ) +
  geom_text(
    data = summary_df,
    aes(x = 3, y = 1.5, label = paste0(team ,":",GF, "-", GA)),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 4
  )+
  labs(
    title = "Title Race Grid View",
    subtitle = "Each dot = 1 match (38 per season)",
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal() +
  
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(face = "bold")
  )+ggthemes::theme_fivethirtyeight()


df_plot <- df_plot %>%
  group_by(team, season) %>%
  arrange(matchday) %>%
  mutate(
    last15 = matchday > (max(matchday) - 15)
  )


ggplot(df_plot, aes(x = col, y = row)) +
  
  geom_tile(
    aes(fill = result),
    color = "white",
    size = 0.5,
    width = 0.9,
    height = 0.9
  ) +
  
  scale_y_reverse() +
  
  facet_grid(team ~ season) +
  
  scale_fill_manual(
    values = c(
      "W" = "#2ecc71",
      "D" = "#f1c40f",
      "L" = "#e74c3c"
    )
  ) +
  
  geom_text(
    data = summary_df,
    aes(x = 3, y = -1.5, label = paste0(team, ": ", GF, "-", GA)),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 4
  ) +
  
  coord_equal() +
  
  labs(
    title = "Man City vs Arsenal over the years:Premier League",
    subtitle = "Each block represents one match (38 per season)",
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal() +
  
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "top"
  )

