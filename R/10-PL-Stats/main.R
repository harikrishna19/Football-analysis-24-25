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
  library(tidyr)
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
# source = "auto" fetches BOTH engsoccerdata and football-data.co.uk and keeps
# whichever has the most recent match (engsoccerdata is updated infrequently).
# You can also force one: "football-data" or "engsoccerdata".
get_pl_matchweeks <- function(season = 2026,      # 2026 = the 2026/27 season
                              teams  = NULL,
                              source = c("auto", "football-data", "engsoccerdata")) {
  source <- match.arg(source)
  
  name_map <- c("Man United"    = "Manchester United",
                "Man City"      = "Manchester City",
                "Tottenham"     = "Tottenham Hotspur",
                "Brighton"      = "Brighton & Hove Albion",
                "Newcastle"     = "Newcastle United",
                "West Ham"      = "West Ham United",
                "Wolves"        = "Wolverhampton Wanderers",
                "Leeds"         = "Leeds United",
                "Nott'm Forest" = "Nottingham Forest")
  
  from_engsoccerdata <- function() {
    if (!requireNamespace("engsoccerdata", quietly = TRUE)) return(NULL)
    engsoccerdata::england_current(Season = season) %>%
      filter(tier == 1) %>%
      transmute(date = as.Date(Date), home, away = visitor, hg = hgoal, ag = vgoal)
  }
  
  from_footballdata <- function() {
    code <- sprintf("%02d%02d", season %% 100, (season + 1) %% 100)
    url  <- getOption("pl_csv",
                      sprintf("https://www.football-data.co.uk/mmz4281/%s/E0.csv", code))
    read_csv(url, show_col_types = FALSE) %>%
      filter(!is.na(FTHG)) %>%                       # played matches only
      transmute(date = dmy(Date),
                home = recode(HomeTeam, !!!name_map),
                away = recode(AwayTeam, !!!name_map),
                hg = FTHG, ag = FTAG)
  }
  safe <- function(f) tryCatch(f(), error = function(e) {
    message("  source failed: ", conditionMessage(e)); NULL })
  
  cands <- list()
  if (source %in% c("auto", "football-data")) cands[["football-data.co.uk"]] <- safe(from_footballdata)
  if (source %in% c("auto", "engsoccerdata")) cands[["engsoccerdata"]]       <- safe(from_engsoccerdata)
  cands <- Filter(function(x) !is.null(x) && nrow(x) > 0, cands)
  if (length(cands) == 0) stop("No match data available from any source.")
  
  # keep the source with the latest match date (ties: most matches)
  score <- vapply(cands, function(x) as.numeric(max(x$date)) * 1e4 + nrow(x), numeric(1))
  raw   <- cands[[which.max(score)]]
  message("Using ", names(cands)[which.max(score)], ": ", nrow(raw),
          " matches, latest ", format(max(raw$date)))
  
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
# 2. Panel 1 - points progression + stats table + key issues
# ---------------------------------------------------------------------
trio  <- c("Chelsea", "Manchester United", "Tottenham Hotspur")
short <- c("Chelsea" = "Chelsea", "Manchester United" = "Man United",
           "Tottenham Hotspur" = "Spurs")
team_cols <- c(Chelsea = BLUE, "Manchester United" = RED,
               "Tottenham Hotspur" = NAVY, "League average" = "grey55")
ord <- function(n) paste0(n, ifelse(n %% 100 %in% 11:13, "th",
                                    c("th","st","nd","rd","th","th","th","th","th","th")[n %% 10 + 1]))

# league-average cumulative points (benchmark line)
avg_df <- pl_all %>% filter(matchweek <= GW) %>%
  group_by(matchweek) %>% summarise(cum_pts = mean(cum_pts), .groups = "drop") %>%
  mutate(team = "League average")
pl_plot    <- bind_rows(pl, avg_df) %>% mutate(team = factor(team, levels = names(team_cols)))
end_labels <- pl %>% filter(matchweek == GW)

# ---------------------------------------------------------------------
# 2. Panel 1 - points progression + stats table + key issues
# ---------------------------------------------------------------------
trio  <- c("Chelsea", "Manchester United", "Tottenham Hotspur")
short <- c("Chelsea" = "Chelsea", "Manchester United" = "Man United",
           "Tottenham Hotspur" = "Spurs")
team_cols <- c(Chelsea = BLUE, "Manchester United" = RED,
               "Tottenham Hotspur" = NAVY, "League average" = "grey55")
ord <- function(n) paste0(n, ifelse(n %% 100 %in% 11:13, "th",
                                    c("th","st","nd","rd","th","th","th","th","th","th")[n %% 10 + 1]))

# league-average cumulative points (benchmark line)
avg_df <- pl_all %>% filter(matchweek <= GW) %>%
  group_by(matchweek) %>% summarise(cum_pts = mean(cum_pts), .groups = "drop") %>%
  mutate(team = "League average")
pl_plot    <- bind_rows(pl, avg_df) %>% mutate(team = factor(team, levels = names(team_cols)))
end_labels <- pl_plot %>% filter(matchweek == GW) %>%
  mutate(lab = ifelse(team == "League average", sprintf("League avg  %.0f", cum_pts),
                      sprintf("%s  %.0f", short[as.character(team)], cum_pts)))

h1 <- panel_heading("1", "3 out of the Big 6 failing to find consistency",
                    "Chelsea, Man United and Spurs are all below the league-average pace\nin the new Premier League season.")

c1 <- ggplot(pl_plot, aes(matchweek, cum_pts, colour = team, linetype = team)) +
  geom_line(aes(linewidth = team), lineend = "round") +
  geom_point(data = filter(end_labels, team != "League average"), size = 4.2,
             show.legend = FALSE) +
  geom_label_repel(data = end_labels, aes(label = lab, fill = team), colour = "white",
                   fontface = "bold", size = mm(SZ$label), family = fam,
                   label.size = 0, show.legend = FALSE, label.padding = unit(0.28, "lines"),
                   direction = "y", nudge_x = 0.5, hjust = 0, segment.colour = NA,
                   box.padding = 0.2, seed = 1) +
  scale_colour_manual(values = team_cols, name = NULL) +
  scale_fill_manual(values = team_cols) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "22"), name = NULL) +
  scale_linewidth_manual(values = c(1.5, 1.5, 1.5, 1), name = NULL) +
  scale_x_continuous(breaks = 1:GW, labels = paste0("GW", 1:GW),
                     expand = expansion(add = c(0.4, 3.2))) +
  scale_y_continuous(limits = c(0, NA), breaks = pretty_breaks(6),
                     expand = expansion(mult = c(0.02, 0.08))) +
  labs(x = NULL, y = "Points") +
  theme_chart() +
  theme(panel.grid.major.x = element_blank(), legend.position = "none")

# ---- insight strips (text supplied by the author; not computed) -------
MAROON <- "#7a1f3d"
ins <- tibble(
  stat = c("8", "1959/60", "0", "0", "1"),
  sub  = c("pts from 11 games", "last time", "goals in first 5", "clean sheets", "win each"),
  col  = c(RED, BLUE, NAVY, RED, MAROON),
  txt  = c("Man United sit in the relegation zone with 8 points from 11 games, after a heavy loss at Anfield to Liverpool in matchweek 11.",
           "Chelsea have lost at home to both Spurs and Leeds in the same season, a first at Stamford Bridge since 1959/60.",
           "Spurs failed to score in their first 5 league games, despite spending close to 300M on transfers.",
           "Man United have no clean sheets this season and have conceded close to 34 goals in all competitions.",
           "Only Burnley and Man United have just one win from their games in the top flight this season.")
) %>% mutate(y = seq(0.90, 0.10, length.out = n()),
             txt = vapply(txt, function(t) paste(strwrap(t, 88), collapse = "\n"), ""))

c1_ins <- ggplot(ins) +
  geom_rect(aes(xmin = 0, xmax = 14, ymin = y - 0.082, ymax = y + 0.082),
            fill = CARD, colour = NA) +
  geom_rect(aes(xmin = 0, xmax = 0.16, ymin = y - 0.082, ymax = y + 0.082, fill = col),
            colour = NA) +
  geom_text(aes(0.55, y + 0.025, label = stat, colour = col), hjust = 0,
            fontface = "bold", size = mm(22), family = fam) +
  geom_text(aes(0.55, y - 0.052, label = sub), hjust = 0, colour = GREY,
            size = mm(SZ$label - 2), family = fam) +
  geom_text(aes(3.55, y, label = txt), hjust = 0, vjust = 0.5, colour = NAVY,
            lineheight = 1.1, size = mm(SZ$label), family = fam) +
  scale_fill_identity() + scale_colour_identity() +
  coord_cartesian(xlim = c(0, 14), ylim = c(0, 1), expand = FALSE) +
  theme_void() + bg

panel1 <- full(h1) / full(tag_label("Points progression and what's gone wrong")) / c1 / full(c1_ins) +
  plot_layout(heights = c(1.45, 0.35, 3.4, 3.6))



# Panel 2 code

# ---------------------------------------------------------------------
# 3. Panel 2 - Brighton credibility matrix
# ---------------------------------------------------------------------
pl_agg <- pl_all %>%
  group_by(team) %>%
  summarise(gf = sum(gf), ga = sum(ga), gd = gf - ga, pts = sum(points), .groups = "drop") %>%
  mutate(hl = ifelse(team %in% c("Arsenal", "Brighton & Hove Albion","Leeds United"), team, "Other"),
         rank_pts = min_rank(-pts))
med_gf <- median(pl_agg$gf); med_ga <- median(pl_agg$ga)
hl_cols <- c("Arsenal" = RED, "Brighton & Hove Albion" = BLUE, "Other" = "grey65","Leeds United"="#FFFFC5")
bri <- filter(pl_agg, team == "Brighton & Hove Albion")
ord <- function(n) paste0(n, ifelse(n %% 100 %in% 11:13, "th",
                                    c("th","st","nd","rd","th","th","th","th","th","th")[n %% 10 + 1]))

h2 <- panel_heading(
  "2",
  "Brighton: Genuine Title Contenders?",
  paste(
    "Can Brighton pull off a Leicester?",
    "Only Arsenal, Leeds and Brighton have conceded fewer than 10 goals.",
    "Hull remain unbeaten at home, while Brighton recorded the highest",
    "first-half xG by any PL team this season.",
    "Meanwhile, Arsenal dropped points at St James' Park.",
    "The managerial sack race adds another storyline. (TBD)",
    sep = "\n"
  )
)

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
                   aes(label = team, fill = hl), colour = "black", fontface = "bold",
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
panel2

#FFFFC5 
# (panel1 | spacer() | panel2 | plot_layout(widths = c(1, 0.06, 1))) 






