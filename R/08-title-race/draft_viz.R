

# Load data

data<-read.csv("data/pl_data.csv")


metrics_df<-data  %>% 
  group_by(season,title) %>% 
  select(season,title,wins,draws,loses,pts) %>% 
  summarise(
    "Wins"=sum(wins),
    "Draws"=sum(draws),
    "Losses"=sum(loses),
    "Points"=sum(pts)
  )
make_waffle(metrics_df$Wins,metrics_df$Draws,metrics_df$Losses,season = metrics_df$season,metrics_df$title,period = "",metrics_df$Points)
