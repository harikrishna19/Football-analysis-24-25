



# Get Data for all the requirted season for the teams ---------------------



pl_data <- list()

get_data <- function(comp, years) {
  pl_data <- list()
  for (i in years) {
    message("Fetching: ", i)
    data <- understatr::get_league_teams_stats(comp, i)
    pl_data[[as.character(i)]] <- data
  }
  return(pl_data)
}

# Example:
years <- c(2023, 2024, 2025)
out <- get_data("EPL", years)


# Sample Analysis Data clean

sd<-do.call(rbind,out[2:3])

sd<-sd %>% group_by(team_name) %>% mutate("MatchWeek"=row_number())
sd$agg_pts<-ave(sd$pts, sd$team_name, FUN=cumsum)

#Relegated Teams 2024-25 season & promoted: Lei,Ips,Sou

ggplotly(
sd %>% filter(team_name %in% c("Ipswich", "Southampton", "Leicester","Burnley","Leeds","Sunderland")) %>%
  ggplot2::ggplot(aes(MatchWeek,agg_pts,color=team_name))+geom_point()+geom_line()+ggthemes::theme_economist() 
)
















