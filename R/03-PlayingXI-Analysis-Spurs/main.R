

# Load required librares --------------------------------------------------
library(R6)
library(ggplot2)
library(dplyr)


# Load required data for analysis -----------------------------------------
tot_data<-read.csv("tot_all_matches.csv",check.names = FALSE)

# analyze one game
tot_data<-tot_data %>% 
  mutate(matchday = match(MatchURL, unique(MatchURL)) ) %>% filter(  
    Team =="Tottenham Hotspur")

# Declaring R6 class ------------------------------------------------------

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
          Label = paste0("Starts:", Starts),
          Bold  = Starts >= 25
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
        scale_x_continuous(breaks = seq(1, 38, 1), limits = c(1, 40)) +
        
        # Season context lines
        geom_vline(xintercept = 19, linetype = "dashed", color = "gray50") +
        geom_vline(xintercept = 30, linetype = "dashed", color = "gray50") +
        
        # Labels on the right
        geom_text(data = self$player_summary, 
                  aes(x = 40, y = Player_Name, label = Label,
                      fontface = ifelse(Bold, "bold", "plain")),
                  inherit.aes = FALSE, hjust = 0, size = 3.2, color = "black") +
        
        labs(title = "Tottenham Player Availability (Premier League 2024/25)",
             subtitle = "",
             x = "Match Week", y = "Player") +
        theme_minimal(base_size = 12) +
        theme(axis.text.y = element_text(size=7),
              legend.position = "top",
              plot.margin = margin(10, 80, 10, 10))
    }
  )
)



# Create object
viz <- PlayerAvailabilityPlot$new(tot_data)

# Plot
viz$plot()

