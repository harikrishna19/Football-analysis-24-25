remotes::install_github('ewenme/understatr')
library(understatR)
understatr::get_league_seasons("EPL")



understatr::get_leagues_meta()

understatr::get_league_teams_stats("EPL",2025) %>% View()



understatr::get_team_players_stats("EPL",2025,"Arsenal") %>% View()
