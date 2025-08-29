
# Loading required packages for analysis ----------------------------------

library(worldfootballR)
library(tidyverse)


# Fetch Match URLS for pl 2024/25 -------------------------------------------
# function to extract match lineups
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









