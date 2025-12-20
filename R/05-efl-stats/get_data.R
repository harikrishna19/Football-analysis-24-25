



# Get Data for all the requirted season for the teams ---------------------
#reading 2025-26 data csv



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
  "Luton" = "#F78F1E",
  "Burnley" = "#6C1D45",
  "Sheffield United" = "red",
  "Leicester" = "#003090",
  "Ipswich" = "#0057B8",
  "Southampton" = "#D71920",
  "Burnley 23/24 "="#6C1D45",
  "Leeds" = "#1D428A",
  "Sunderland" = "#EB172B"
)
# Adding match week 16 results agg for leeds,burnley,sunderland
order_teams<-  c("Luton",
"Burnley 23/24 ",
"Sheffield United",
"Leicester",
"Ipswich",
"Southampton",
"Sunderland",
"Leeds",
"Burnley")
new_rows <- tibble::tibble(
  team_name = c("Leeds", "Burnley", "Sunderland"),
  MatchWeek = 16,
  agg_pts = c(16, 10, 26),
  year=2025
)
sd <- dplyr::bind_rows(sd, new_rows)


# ---------------------------

# Create combined facet variable (KEY FIX)
# ---------------------------
df <- sd %>%
  mutate(
    facet = paste(year, team_name, sep = " — ")
  )

font_add_google("Inter", "inter")
showtext_auto()

label_df <- df %>%
  group_by(team_name) %>%
  slice_max(MatchWeek, n = 1, with_ties = FALSE)

df<-df %>%
  arrange(match(team_name, order_teams))


ggplot(df, aes(MatchWeek, agg_pts, color = team_name)) +
    geom_line(linewidth = 1.3,show.legend = F) +
    geom_point(size = 2,show.legend = F) +
    
    # 🔹 Final-point labels
    geom_text(
      data = label_df,
      aes(label = agg_pts),
      vjust = 1.8,hjust = -0.1,
      # ⬆️ moves label above the point
      size = 3.5,color="red",
      show.legend = FALSE
    ) +
    
    facet_wrap(
      ~ factor(facet,levels = unique(df$facet)),
      scales = "free_x",
      ncol = 3
    ) +
    scale_color_manual(values = club_cols) +
  labs(
    title = "Promoted Teams Usually Struggle — But 2025/26 Is Different",
    subtitle =paste0(
      "• Cumulative points vs survival pace (≈38 points)<br>",
      "• <span style='color:#EB172B; font-weight:bold;'>Sunderland</span> already tracking top-6 pace — historically rare for promoted teams<br>",
      "• Leeds showing early signs of comfortable survival"
    ),
    x = "Match Week",
    y = "Cumulative Points",
    caption = "Dashed line shows 1.0 point per game. 2025/26 data through Matchweek 16."
  ) +
theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    
    # 🔑 Enable markdown for subtitle
    plot.subtitle = element_markdown(size = 12),
    plot.title = element_text(face = "bold")
  )  + coord_cartesian(clip = "off")

  















