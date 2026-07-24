# Loading local function. using devtools for fetching data
library(engsoccerdata)
library(tidyverse)

# Load england championship data

eng_cship<-engsoccerdata::england_current()
eng_cship<-eng_cship %>% dplyr::filter(division==2)


# Make league table


# Data explore ------------------------------------------------------------

league_table<- engsoccerdata::maketable_eng(eng_cship,2025,tier = 2,division = 2)


# samplew plot
ggplot(league_table,aes(team,Pts))+geom_col()+coord_flip()


#HT analysis


# View HT and FT breakdowns between two teams
games_ht_ft_breakdown(eng_cship, "Southampton", "Millwall")

# Get team's half-time stats
ht_team_stats(eng_cship, "Coventry City")

# Compare 1st half vs 2nd half performance
ht_vs_ft_comparison(eng_cship, "Coventry City")

# Access columns directly
eng_current[, c("Date", "home", "visitor", "HT", "FT", "ht_hgoal", "h2_hgoal")]


# Team analysis

cc<-eng_cship %>% filter(home=="Coventry City"| visitor=="Coventry City")
View(cc)








