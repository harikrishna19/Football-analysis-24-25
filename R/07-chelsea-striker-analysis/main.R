
#Loading Required libraries in R
library(ggplot2)
library(ggthemes)
library(sysfonts)
library(showtext)

team_data<-read.csv("data/team_data.csv")
chelsea_data<-read.csv("data/chelsea_data.csv")
chelsea_data$season<-as.character(chelsea_data$season)

systemfonts::system_fonts()
font_add_google("Open Sans", "open-sans")
ggplot(chelsea_data, aes(season, goals)) +
  
  geom_col(alpha = .35, width = .6) +
  
  geom_line(color = "#4ea5ff", linewidth = 1.5) +
  
  geom_point(aes(fill = position),
             size = 6,
             shape = 21,
             color = "white")+ggthemes::theme_tufte()+
  labs(title="Chelsea No.9 since 2016/17",
       subtitle = "Since Conte won the title in 2016/17",caption = "Plot by HK")+coord_flip()+
theme(legend.position = "none")

