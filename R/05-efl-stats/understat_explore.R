remotes::install_github('ewenme/understatr')
library(understatR)


#Checking season data availability
understatr::get_league_seasons("EPL")




#Get sample league data table
df<-understatr::get_league_teams_stats("EPL",2024)


df %>% dplyr::filter(team_name=="Tottenham") %>% dplyr::summarise(pts=sum(pts))

df %>% View()


