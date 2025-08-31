

# Load required librares --------------------------------------------------
# library(R6)
# library(ggplot2)
# library(dplyr)
# library(magrittr)
#

# Load required data for analysis -----------------------------------------
# tot_data<-read.csv("tot_all_matches.csv",check.names = FALSE)
#
# # analyze one game
# tot_data<-tot_data %>%
#   mutate(matchday = match(MatchURL, unique(MatchURL)) ) %>% filter(
#     Team =="Tottenham Hotspur")

# Declaring R6 class ------------------------------------------------------



# Season Plot -------------------------------------------------------------
plot_5<-league_positions %>% 
  filter(Team %in% c("Tottenham")) %>%
  mutate(Wk = as.numeric(Wk)) %>%
  ggplot(aes(x = Wk, y = Position, group = Team, color = Team)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  # add matchweek labels on points
  geom_text(aes(label = Position), hjust="inward",vjust = -0.7, size = 4, color = "red")+
  scale_x_continuous(breaks = 1:38, guide = guide_axis(n.dodge = 1)) +
  scale_color_manual(values=c("#1BA1E2")) +
  scale_y_reverse(breaks = 1:max(league_positions$Position)) +
  labs(
    title = "Tottenham 2024/25 season position by MatchWeek",
    subtitle = paste("Even though after winning the Europa League", 
    "they struggled in the top flight last season",
    "by finishing 17th just above the relegation zone",
    sep="\n"),
    caption = "Graphic:By Hari Krishna,Data Source:{worldfooballR}",
    x = "Matchweek", y = "Position"
  ) +
  ggthemes::theme_clean() +
  theme(
    axis.text.x = element_blank(),    # hide tick labels
    axis.ticks.x = element_blank(),   # hide tick marks
    axis.title.x = element_blank(),   # hide axis title
    axis.line.x = element_blank(),    # hide axis line
    axis.text.y = element_blank(),    # hide tick labels
    axis.ticks.y = element_blank(),   # hide tick marks
    axis.title.y = element_blank(),   # hide axis title
    axis.line.y = element_blank(),    # hide axis line
    legend.position = "none"
  )


plot_5


PlayerAvailabilityPlot <- R6::R6Class(
  "PlayerAvailabilityPlot",
  
  public = list(
    data = NULL,
    player_summary = NULL,
    player_order = NULL,
    
    initialize = function(data) {
      stopifnot(all(c("Player_Name", "matchday", "Starting") %in% colnames(data)))
      self$data <- data
      self$compute_summary()
    },
    
    # --- Compute player summaries & order ---
    compute_summary = function() {
      self$player_summary <- self$data %>%
        group_by(Player_Name) %>%
        summarise(
          Starts = sum(Starting == "Pitch"),
          Bench  = sum(Starting == "Bench"),
          Injured = sum(Starting == "Injured"),
          .groups = "drop"
        ) %>%
        mutate(
          Label = paste0(round((Starts/38)*100,1),"%"),
          Bold  = Label >= 70
        )
      
      self$player_order <- self$player_summary %>%
        arrange(desc(Starts)) %>%
        pull(Player_Name)
      
      # Apply ordering
      self$data <- self$data %>%
        mutate(Player_Name = factor(Player_Name, levels = self$player_order))
      
      self$player_summary <- self$player_summary %>%
        mutate(Player_Name = factor(Player_Name, levels = self$player_order))
    },
    
    # --- Generate plot ---
    plot = function() {
      ggplot(self$data, aes(x = matchday, y = reorder(Player_Name,desc(Player_Name)),  color = Starting)) +
        geom_point(size = 3, alpha = 0.8) +
        scale_color_manual(values = c("Pitch"="#2ecc71", 
                                      "Bench"="#f1c40f", 
                                      "Injured"="#e74c3c")) +
        scale_x_continuous(
          breaks = 1:38,   # show only 1–38
          limits = c(1, 40)  # axis extends to 40, no tick at 39 or 40
        )+
        # Season context lines
        geom_vline(xintercept = 19, linetype = "dashed", color = "black") +
        annotate("text", x = 19, y = 0.5, label = "Mid Season", 
                 angle = 90, vjust = -0.5, hjust = 0, color = "gray50")+
        
      #Add summary (just number of starts)
      geom_text(data = self$player_summary, 
                aes(x = 39, y = Player_Name, label = Label),
                inherit.aes = FALSE, hjust = 0, size = 3.5, fontface = "bold") +
        
        # Title above summary column
        annotate("text", x = 39, y = length(self$player_order) + 1,
                  label = "Start % by Player", hjust = 0, size = 4, fontface = "bold") +
        
        labs(
          title = "Tottenham Player Availability (Premier League 2024/25)",
          subtitle = paste(
            "• Most of the Tottenham starting XI in Premier League have played less than 70% of games",
            "• Center Backs mainly Van de Van and Romero have played less than 50% of matches",
            "• Overall incosistency in the playing XI due to injuries and lack of depth in the bench",
            sep = "\n"
          ),
          caption = "Graphic: By Hari Krishna, Data Source: {worldfootballR}",
          x = "Match Week", 
          y = "Player"
        )+
        theme_minimal(base_size = 12) +
         coord_cartesian(clip = "off")+
        theme(axis.text.y = element_text(size=5),
              axis.text.x = element_text(
                angle = 90, hjust = 1),
              legend.position = "bottom",
              plot.margin = margin(10, 80, 10, 10))+ggthemes::theme_clean()
    }
  )
)



# Create object
viz <- PlayerAvailabilityPlot$new(tot_data)

# Plot
library(patchwork)
viz$plot() + plot_5 + plot_layout(widths = c(6, 4))

