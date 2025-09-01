

# Load required librares --------------------------------------------------
library(R6)
library(ggplot2)
library(dplyr)
library(magrittr)
library(patchwork)


# Load required data for analysis -----------------------------------------
tot_data<-read.csv("tot_all_matches.csv",check.names = FALSE)


# analyze one game
tot_data<-tot_data %>%
  mutate(matchday = match(MatchURL, unique(MatchURL)) ) %>% filter(
    Team =="Tottenham Hotspur")

# Declaring R6 class ------------------------------------------------------



# Season Plot pulled from previous project -------------------------------------------------------------
tot_season_analysis<-league_positions %>% 
  filter(Team %in% c("Tottenham")) %>%
  mutate(Wk = as.numeric(Wk)) %>%
  ggplot(aes(x = Wk, y = Position, group = Team, color = Team)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  # add matchweek labels on points
  #geom_text(aes(label = Position), hjust="inward",vjust = -0.7, size = 4, color = "red")+
  scale_x_continuous(breaks = 1:38, guide = guide_axis(n.dodge = 1)) +
  scale_color_manual(values=c("#1BA1E2")) +
  scale_y_reverse(breaks = 1:max(league_positions$Position)) +
  labs(
    title = "Tottenham season position by MatchWeek",
    subtitle = paste("Even though after winning the Europa League", 
    "they struggled in the top flight last season",
    "by finishing 17th just above the relegation zone",
    sep="\n"),
    caption = "Graphic:By Hari Krishna,Data Source:{worldfootballR}",
    x = "Matchweek", y = "Position"
  ) +
  ggthemes::theme_clean() +
  theme(
    axis.text.x = element_blank(),    # hide tick labels
    axis.ticks.x = element_blank(),   # hide tick marks
    axis.title.x = element_blank(),   # hide axis title
    axis.line.x = element_blank(),    # hide axis line
    #axis.text.y = element_blank(),    # hide tick labels
    #axis.ticks.y = element_blank(),   # hide tick marks
    #axis.title.y = element_blank(),   # hide axis title
    #axis.line.y = element_blank(),    # hide axis line
    legend.position = "none"
  )





# R6 class for player avalilability for the entire season -----------------

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
          Label = paste0(
            "<span style='color:#006400;'>", round((Starts/38)*100,1), "</span>",
            "/",
            "<span style='color:#1BA1E2;'>", round((Bench/38)*100,1), "</span>"
          ),
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
      ggplot(self$data, aes(x = matchday, y = reorder(Player_Name,desc(Player_Name)),color = Starting)) +
        geom_point(size = 3, alpha = 0.8) +
        scale_color_manual(values = c("Pitch"="#006400", 
                                      "Bench"="#1BA1E2", 
                                      "Injured"="#e74c3c"),name="PlayingXI") +
        scale_x_continuous(
          breaks = 1:38,   # show only 1–38
          limits = c(1, 40)  # axis extends to 40, no tick at 39 or 40
        )+
        # Season context lines
        geom_vline(xintercept = 19, linetype = "dashed", color = "black") +
        annotate("text", x = 19, y = 0.5, label = "Mid Season", 
                 angle = 90, vjust = -0.5, hjust = 0, color = "black")+
        
      #Add summary (just number of starts)
        ggtext::geom_richtext(
          data = self$player_summary, 
          aes(x = 39, y = Player_Name, label = Label),
          inherit.aes = FALSE, 
          hjust = 0, 
          size = 3, 
          fontface = "bold",
          fill = NA,          # remove background fill
          label.color = NA,   # remove border
          label.padding = grid::unit(rep(0, 4), "pt") # no padding inside
        )+
        
        # Title above summary column
        annotate("text", x = 39, y = length(self$player_order) + 1,
                  label = "Start%:Bench%", hjust = 0, size = 4, fontface = "bold") +
        theme(axis.text.y = element_text(size=5),
              axis.text.x = element_text(
                angle = 90, hjust = 1),
              legend.position = c(0.85, 0.2),  # inside plot (x,y from 0–1)
              legend.background = element_rect(fill = "white", color = "black"),
              legend.title = element_text(face = "bold"),
              legend.key.size = unit(0.4, "cm"),  # smaller legend boxes
              plot.margin = margin(10, 80, 10, 10))+ggthemes::theme_clean()+
        
        labs(
          title = "Tottenham Player Availability (Premier League 2024/25) by Match Week",
          subtitle = paste(
            "• Most of the Tottenham key starting XI in Premier League have played less than 70% of games",
            "• Center Backs mainly Van de Ven and Romero have played less than 50% of matches",
            "• Overall inconsistency in the playing XI due to injuries and lack of depth in the bench",
            sep = "\n"
          ),
          caption = "Graphic: By Hari Krishna, Data Source: {worldfootballR}",
          x = "Match Week", 
          y = "Player"
        )+
         coord_cartesian(clip = "off")
    }
  )
)



# Create object
viz <- PlayerAvailabilityPlot$new(tot_data)

# Plot
viz$plot() + tot_season_analysis + plot_layout(widths = c(6, 4),ncol=2)

