
# Loading required packages for analysis ----------------------------------

library(worldfootballR)
library(tidyverse)
library(tidyr)

# Fetch Match URLS for pl 2024/25 -------------------------------------------
# function to extract match lineups
pl_2025<-worldfootballR::fb_match_urls("ENG",gender = "M",season_end_year = 2025,tier = "1st")


#fetch only tottenham matches

spurs_games<- grep(pattern = "Tottenham-Hotspur",x = pl_2025,value=T)



# Fetch playing XI of all games of Tottenham ------------------------------

match_line_ups <- fb_match_lineups(match_url = spurs_games)

# first half 19 gmaes urls and data fetched
match_line_ups_2nd_half<-fb_match_lineups(match_url = spurs_games[20:38])

# scrape remaing last 4 game sof the season
last_4<-fb_match_lineups(match_url = spurs_games[35:38])


# All matches data --------------------------------------------------------

all_data<-rbind(match_line_ups,match_line_ups_2nd_half,last_4)

all_data$MatchURL %>% unique() %>% length()


# Saving all_data for further reference -----------------------------------

write.csv(all_data,"tot_all_matches.csv",row.names = F)



# Data for league-Pulled from blog 2 --------------------------------------

# Extract results data for 2025 season ------------------------------------

pl_2025<-worldfootballR::fb_match_results("ENG",gender = "M",season_end_year = 2025,tier = "1st")



# 1. Create long format for easier processing for PL data based on points
matches_long <- pl_2025 %>%
  mutate(
    HomePoints = case_when(
      HomeGoals > AwayGoals ~ 3,
      HomeGoals == AwayGoals ~ 1,
      TRUE ~ 0
    ),
    AwayPoints = case_when(
      AwayGoals > HomeGoals ~ 3,
      AwayGoals == HomeGoals ~ 1,
      TRUE ~ 0
    )
  ) %>%
  select(Wk, Home, Away, HomeGoals, AwayGoals, HomePoints, AwayPoints) %>%
  pivot_longer(cols = c(Home, Away),
               names_to = "HomeAway", values_to = "Team") %>%
  mutate(
    GoalsFor = ifelse(HomeAway == "Home", HomeGoals, AwayGoals),
    GoalsAgainst = ifelse(HomeAway == "Home", AwayGoals, HomeGoals),
    Points = ifelse(HomeAway == "Home", HomePoints, AwayPoints),
    Won = ifelse(Points == 3, 1, 0),
    Played=1,
    Drawn = ifelse(Points == 1, 1, 0),
    Lost = ifelse(Points == 0, 1, 0)
  ) %>%
  select(Wk, Team,Played, Won, Drawn, Lost, GoalsFor, GoalsAgainst, Points)

# 2. Cumulative stats after each matchweek
league_table <- matches_long %>%
  group_by(Team, Wk) %>%
  summarise(
    Played = sum(Played),
    Won = sum(Won),
    Drawn = sum(Drawn),
    Lost = sum(Lost),
    GF = sum(GoalsFor),
    GA = sum(GoalsAgainst),
    Points = sum(Points),
    .groups = "drop"
  ) %>%
  arrange(as.numeric(Wk)) %>%
  group_by(Team) %>%
  mutate(
    cPlayed = cumsum(Played),
    cWon = cumsum(Won),
    cDrawn = cumsum(Drawn),
    cLost = cumsum(Lost),
    cGF = cumsum(GF),
    cGA = cumsum(GA),
    cPoints = cumsum(Points),
    cGD = cGF - cGA
  ) %>%
  ungroup()

# 3. Create league table per week with ranking
league_positions <- league_table %>%
  group_by(Wk) %>%
  arrange(Wk, desc(cPoints), desc(cGD), desc(cGF),desc(cGA), Team) %>%
  mutate(Position = row_number()) %>%
  select(Wk, Team, Position, cPlayed, cWon, cDrawn, cLost, cGF, cGA, cGD, cPoints) %>%
  ungroup()

write.csv(league_positions,"league_positions.csv",row.names = FALSE)





