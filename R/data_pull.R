#Pulll data from worlkdfootball r


library(worldfootballR)
library(magrittr)
# function to extract Serie A match results data
pl_2025 <- worldfootballR::fb_match_results(country = "ENG", gender = "M", season_end_year = 2025, tier = "1st")

#filtering all the liverpool URLS to analyze liverpool data
pl_2025 %>% colnames()
liverpool_match_urls<-pl_2025 %>% dplyr::filter(Home =="Liverpool" | Away=="Liverpool")


#fetch_match_report

# function to extract match report data
match_urls<-grep("/matches",liverpool_match_urls$MatchURL,value=TRUE)


# function to extract match summary data
liv_match_summaries <-worldfootballR::fb_match_summary(match_url = match_urls)

# fetch other data from 22:29
liv_match_summaries2 <-worldfootballR::fb_match_summary(match_url = match_urls[22:29])


liv_match_summaries
liv_match_summaries<-rbind(liv_match_summaries,liv_match_summaries2)
write.csv(liv_match_summaries,"match_summaries.csv")



