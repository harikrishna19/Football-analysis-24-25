
#Loading Required libraries in R
library(ggplot2)
library(ggthemes)
library(sysfonts)
library(showtext)
library(purrr)
library(jsonlite)
library(tidyr)

team_data<-read.csv("data/team_data.csv")
chelsea_data<-read.csv("data/chelsea_data.csv")
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
  annotate("text", x = "2018", y = 18,
         label = "Post-Costa dip\nNo consistent No.9",
         color = "#ff6b6b",
         size = 4,
         fontface = "bold",
         hjust = 0) +

annotate("segment",
         x = "2017", xend = "2018",
         y = 20, yend = 18,
         color = "#ff6b6b",
         linewidth = 0.8,
         arrow = arrow(length = unit(0.2, "cm"))) +

annotate("text", x = "2025", y = max(Team_g$goals),
         label = "New hope: João Pedro",
         color = "#6be675",
         size = 4,
         fontface = "bold",
         hjust = 1) +
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

p2 <- ggplot(total_goals, aes(x = as.character(season), y = total_goals)) +
  geom_col(fill = "#d6a66b", width = 0.6) +
  geom_text(aes(label = total_goals),
            vjust = -0.5,
            color = "white",
            size = 3.5,
            fontface = "bold") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#081633", color = NA),
    panel.background = element_rect(fill = "#081633", color = NA),
    panel.grid = element_blank(),
    axis.text = element_text(color = "white"),
    axis.ticks = element_blank()
  )
library(patchwork)


p3 <- ggplot(total_goals, aes(x = as.character(season), y = 1)) +
  geom_point(size = 6, color = "#d6a66b") +
  geom_text(aes(label = total_goals), vjust = -1, color = "white") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#081633", color = NA))
p1 / p2 + plot_layout(heights = c(3, 2))


library(ggplot2)
library(dplyr)

# Sample data
df <- data.frame(
  category = c("A","B","C","D","E"),
  col1 = c(100, 80, 60, 40, 20),   # bars
  col2 = c(50, 40, 30, 20, 10)     # line (second axis)
)

# Scaling factor
scale_factor <- max(df$col1) / max(df$col2)

ggplot(df, aes(x = category)) +
  geom_bar(aes(y = col1), stat = "identity", fill = "steelblue") +
  
  # scale col2 to match primary axis
  geom_line(aes(y = col2 * scale_factor, group = 1), color = "red", size = 1) +
  geom_point(aes(y = col2 * scale_factor), color = "red") +
  
  scale_y_continuous(
    name = "Column 1",
    sec.axis = sec_axis(~ . / scale_factor, name = "Column 2")
  ) +
  theme_minimal()
