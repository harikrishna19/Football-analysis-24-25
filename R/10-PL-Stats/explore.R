library(engsoccerdata)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(magrittr)
library(patchwork)


# Get Points for each match week Unites,Tottenham & Chelsea


get_pl_matchweeks <- function(season = 2026,   # 2026 = the 2026/27 season
                              teams = c("Manchester United",
                                        "Tottenham Hotspur",
                                        "Chelsea")) {
  
  # 1. Try engsoccerdata (it uses the start year in Season, tier 1 = Premier League)
  raw <- NULL
  if (requireNamespace("engsoccerdata", quietly = TRUE)) {
    raw <- engsoccerdata::england_current(Season = season) %>% filter(tier==1) %>% 
      transmute(date = as.Date(Date), home, away = visitor,
                hg = hgoal, ag = vgoal)
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
pl <- get_pl_matchweeks()
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

