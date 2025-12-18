



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

out$`2023`$team_name[out$`2023`$team_name=="Burnley"] <- "Burnley23/24"
# Sample Analysis Data clean

sd<-do.call(rbind,out[1:3])

sd<-sd %>% group_by(team_name) %>% mutate("MatchWeek"=row_number())
sd$agg_pts<-ave(sd$pts, sd$team_name, FUN=cumsum)
#Relegated Teams 2024-25 season & promoted: Lei,Ips,Sou
sd<-sd %>%  filter(team_name %in% c("Luton","Sheffield United","Burnley","Burnley 23/24 ",
                                    "Ipswich", "Southampton", "Leicester","Leeds","Sunderland"))

club_cols <- c(
  "Burnley" = "#6C1D45",
  "Luton" = "#F78F1E",
  "Sheffield Utd" = "#EE2737",
  "Southampton" = "#D71920",
  "Leicester" = "#003090",
  "Ipswich" = "#0057B8",
  "Sunderland" = "#EB172B",
  "Leeds" = "#1D428A",
  "Burnley 23/24 "="#6C1D45"
)


# ---------------------------
# Create combined facet variable (KEY FIX)
# ---------------------------
df <- sd %>%
  mutate(
    facet = paste(year, team_name, sep = " — ")
  )

# ---------------------------
# Plot (NO EMPTY PANELS)
# ---------------------------
ggplot(df, aes(MatchWeek, agg_pts, color = team_name)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2) +
  
  # Survival pace (~38 pts)
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  
  facet_wrap(
    ~ facet,
    scales = "free_x",
    ncol = 3
  ) +
  
  scale_color_manual(values = club_cols) +
  
  labs(
    title = "How Do Promoted Teams Perform in the Premier League?",
    subtitle = "Cumulative points vs survival pace (~38 points)",
    x = "Match Week",
    y = "Cumulative Points"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )









# p <- sd %>% 
#   filter(team_name %in% c("Luton","Sheffield United","Burnley","Burnley 23/24",
#                           "Ipswich", "Southampton", "Leicester","Leeds","Sunderland")) %>%
#   ggplot(aes(MatchWeek, agg_pts, color = team_name)) +
#   geom_point() +
#   geom_line() +
#   ggthemes::theme_calc() 
# #facet_wrap(~ year)   # ✅ facet by year
# 
# ggplotly(p)















