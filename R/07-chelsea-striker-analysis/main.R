
#Loading Required libraries in R
library(ggplot2)
library(ggthemes)
library(sysfonts)
library(showtext)
library(purrr)
library(jsonlite)
library(tidyr)
library(dplyr)

team_data<-read.csv("data/team_data.csv")
chelsea_data<-read.csv("data/chelsea_data.csv")
man_city_data<-read.csv("data/man_city_players.csv")
all_players<-read.csv("data/all_players_data.csv")
fil_data<-all_players %>% filter(team_title=="Chelsea,Manchester City")
chelsea_data<-rbind(chelsea_data,fil_data)
chelsea_data$season<-as.character(chelsea_data$season)

systemfonts::system_fonts()
font_add_google("Open Sans", "open-sans")

cols_to_parse <- c("h", "a", "goals", "xG","forecast")

team_data <- team_data %>% filter(as.Date(datetime)<Sys.Date() & result!= "") %>%
  mutate(across(all_of(cols_to_parse),
                ~ gsub("'", '"', .))) %>%
  mutate(across(all_of(cols_to_parse),
                ~ map(.x, fromJSON))) %>%
  { 
    reduce(cols_to_parse,
           ~ unnest_wider(.x, all_of(.y), names_sep = "_"),
           .init = .)
  }


total_goals <- team_data %>%
  filter(h_title == "Chelsea" | a_title == "Chelsea") %>%
  mutate(goals_scored = ifelse(h_title == "Chelsea", goals_h, goals_a)) %>%
  group_by(season) %>% 
  summarise(total_goals = sum(as.numeric(goals_scored), na.rm = TRUE))



total_goals |> ggplot(aes(as.character(season),total_goals))+geom_col()+coord_flip()
total_goals$season<-as.numeric(total_goals$season)
chelsea_data$season<-as.numeric(chelsea_data$season)
# calculate percentage
Team_g<-chelsea_data  %>% group_by(season) %>%
  slice_max(goals, n = 1, with_ties = FALSE) %>%
  ungroup() |> inner_join(total_goals,by="season") |> 
  mutate(percentage=goals/total_goals)

max_goals <- max(Team_g$percentage)
# Scaling factor
scale_factor <- max(Team_g$goals) / max(Team_g$percentage)
p1<-ggplot(Team_g, aes(as.character(season), goals)) +
    geom_point(aes(fill = position),
             size = 6,
             shape = 21,
             color = "white",
             stroke = 1.2) +
geom_line(aes(group = 1), color = "#d6a66b", linewidth = 1.5)+
  #geom_text(aes(label = paste(player_name, goals)),
   #         vjust = -4.2,
    #        color = "white",
     #       fontface = "bold",
      #      size = 3) +
  labs(title="Chelsea No.9 since 2016/17",x="Season",
       subtitle = "Since Conte won the title in 2016/17",caption = "Plot by HK")+
  scale_y_continuous(
    name = "Goals",
    sec.axis = sec_axis(~ . * 100 / scale_factor, name = "Percentage (%)")
  )+
  theme_minimal(base_size = 14) +
#   annotate("text", x = "2018", y = 18,
#          label = "Post-Costa dip\nNo consistent No.9",
#          color = "#ff6b6b",
#          size = 4,
#          fontface = "bold",
#          hjust = 0) +
# 
# annotate("segment",
#          x = "2017", xend = "2018",
#          y = 20, yend = 18,
#          color = "#ff6b6b",
#          linewidth = 0.8,
#          arrow = arrow(length = unit(0.2, "cm"))) +
# 
# annotate("text", x = "2025", y = max(Team_g$goals),
#          label = "New hope: João Pedro",
#          color = "#6be675",
#          size = 4,
#          fontface = "bold",
#          hjust = 1) +
  theme(
    plot.background = element_rect(fill = "#081633", color = NA),
    panel.background = element_rect(fill = "#081633", color = NA),
    panel.grid = element_line(color = "#1c355e"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    legend.title = element_blank(),
    legend.text = element_text(color = "white"),
    plot.title = element_text(color = "white", size = 22, face = "bold"),
    plot.subtitle = element_text(color = "white", size = 14),
    legend.position = "bottom"
  )




library(ggplot2)
# install.packages("showtext")
library(showtext)
font_add_google("Montserrat", "mont")
showtext_auto()
p1 <- ggplot(Team_g, aes(as.character(season), goals, group = 1)) +
  scale_y_continuous(
    name = "Goals",
    sec.axis = sec_axis(~ . * 100 / scale_factor, name = "Percentage (%)")
  )+
  # 🔴 Highlight "Post-Costa Dip"
  # geom_rect(aes(xmin = 2.5, xmax = 6.5, ymin = -Inf, ymax = Inf),
  #           fill = "#ff6b6b", alpha = 0.08, inherit.aes = FALSE) +
  
  # ✨ Trend line
  geom_line(color = "#d6a66b", linewidth = 1.5) +
  
  # ⚽ Points (top scorer each season)
  geom_point(aes(fill = position),
             size = 6,
             shape = 21,
             color = "white",
             stroke = 1.2) +
  
  # 🏷 Player labels (clean + non-overlapping)
  geom_text_repel(
    aes(label = player_name),
    color = "white",
    size = 3.5,
    fontface = "bold",
    nudge_y = 3,
    direction = "y",
    segment.color = "#aaaaaa",
    segment.alpha = 0.4,
    box.padding = 0.3,
    max.overlaps = Inf
  ) +
  
  # 📉 Annotation: Dip explanation
  # annotate("text",
  #          x = 4.5, y = max(Team_g$goals)*0.9,
  #          label = "Post Costa Era\nNo consistent No.9",
  #          color = "#ff6b6b",
  #          size = 4,
  #          fontface = "bold") +
  # 
  # # 🚀 Future hope
  # annotate("text",
  #          x = length(unique(Team_g$season)),
  #          y = max(Team_g$goals),
  #          label = "New hope: João Pedro",
  #          color = "#6be675",
  #          size = 4,
  #          fontface = "bold",
  #          hjust = 1) +
  
  labs(
    title = "Chelsea's No.9 Problem",
    subtitle = "Top scorer each season since 2016/17",
    x = NULL,
    y = "Goals",
    caption = "Data: Understat | Viz: HK"
  ) +
  
  # scale_y_continuous(expand = c(0.05, 0)) +
  
  theme_minimal(base_family = "mont", base_size = 14)+
  theme(
    plot.background = element_rect(fill = "#081633", color = NA),
    panel.background = element_rect(fill = "#081633", color = NA),
    
    panel.grid.major.y = element_line(color = "#1c355e"),
    panel.grid.major.x = element_blank(),
    
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    
    legend.title = element_blank(),
    legend.text = element_text(color = "white"),
    legend.position = "bottom",
    
    plot.title = element_text(color = "white", size = 22, face = "bold"),
    plot.subtitle = element_text(color = "#d6a66b", size = 13),
    plot.caption = element_text(color = "gray70"),
    
    plot.margin = margin(15, 20, 15, 20)
  )



p2<-ggplot(total_goals, aes(x = as.character(season), y = total_goals)) +
  
  # Glow effect (using lighter color instead of alpha)
  geom_col(fill = adjustcolor("#d6a66b" ), width = 0.6) +
  
  # Main bars
  geom_col(fill = "#d6a66b", width = 0.45) +
  
  # Labels inside bars
  geom_text(aes(label = total_goals),
            vjust = 1.4,
            color = "black",
            size = 4,
            fontface = "bold") +
  
  labs(
    title = "Chelsea Goals by Season",
    subtitle = "Drop in output post Costa era",
    x = NULL,
    y = NULL
  ) +
  
  scale_y_continuous(expand = c(0, 0)) +
  
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#081633", color = NA),
    panel.background = element_rect(fill = "#081633", color = NA),
    
    panel.grid.major.y = element_line(color = "white"),
    panel.grid.major.x = element_blank(),
    
    axis.text = element_text(color = "white"),
    axis.ticks = element_blank(),
    
    plot.title = element_text(color = "white", size = 16, face = "bold"),
    plot.subtitle = element_text(color = "#d6a66b", size = 11)
  )

 p1 / p2 + plot_layout(heights = c(3, 2))


