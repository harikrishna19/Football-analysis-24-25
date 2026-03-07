

library(understatr)


understatr::get_league_seasons("EPL")



library(tidyverse)

chelsea <- tibble(
  season = c("16/17","17/18","18/19","19/20","20/21","21/22","22/23","23/24"),
  goals  = c(20,16,14,15,12,11,14,22),
  player = c("Diego Costa",
             "Eden Hazard",
             "Eden Hazard",
             "Tammy Abraham",
             "Jorginho",
             "Mason Mount",
             "Kai Havertz",
             "Cole Palmer"),
  position = c("Striker","Winger","Winger","Striker","Midfielder","Midfielder","Forward","Att Mid")
)
pos_cols <- c(
  "Striker" = "#1f77ff",
  "Winger" = "#9b59ff",
  "Midfielder" = "#2ecc71",
  "Forward" = "#f39c12",
  "Att Mid" = "#27ae60"
)
ggplot(chelsea, aes(season, goals, group = 1)) +
  
  geom_line(color = "#6baed6", linewidth = 1.5) +
  
  geom_point(aes(fill = position),
             size = 6,
             shape = 21,
             color = "white",
             stroke = 1.2) +
  
  geom_text(aes(label = paste(player, goals)),
            vjust = -1.2,
            color = "white",
            fontface = "bold",
            size = 4) +
  
  scale_fill_manual(values = pos_cols) +
  
  labs(
    title = "Breaking the Curse?",
    subtitle = "Chelsea's Premier League Top Scorer by Season",
    x = NULL,
    y = "Goals"
  ) +
  
  theme_minimal(base_size = 14) +
  
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
  )+
  annotate("text", x = 2.5, y = 22,
           label = "Post-Costa Era",
           color = "white",
           size = 5,
           fontface = "bold")+
annotate("text", x = 7.5, y = 25,
         label = "Palmer Breakout",
         color = "#6baed6",
         size = 5,
         fontface = "bold")
