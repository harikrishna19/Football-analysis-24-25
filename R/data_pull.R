#Pulll data from worlkdfootball r


library(worldfootballR)
library(magrittr)
# function to extract Serie A match results data
pl_2024 <- worldfootballR::fb_match_results(country = "ENG", gender = "M", season_end_year = 2025, tier = "1st")

#filtering all the liverpool URLS to analyze liverpool data
pl_2024 %>% colnames()
liverpool_match_urls<-pl_2024 %>% dplyr::filter(Home =="Liverpool" | Away=="Liverpool")


#fetch_match_report

# function to extract match report data
match_urls<-grep("/matches",liverpool_match_urls$MatchURL,value=TRUE)


match_urls <- fb_match_urls(
  country = "ENG",
  gender = "M",
  season_end_year = 2025,
  tier = "1st"
)
df <- fb_match_report(match_url = match_urls)



liv_match_reports<-list()
for(match_repo in 1:length(match_urls)){
  print(match_repo)
liv_match_reports[[match_repo]] <- worldfootballR::fb_match_report(match_url=match_urls[1])
}


liv_match_reports

# function to extract match summary data
liv_mci_2020_summary <-worldfootballR::fb_match_summary(match_url = "https://fbref.com/en/matches/47880eb7/Liverpool-Manchester-City-November-10-2019-Premier-League")
dplyr::glimpse(liv_mci_2020_summary)
