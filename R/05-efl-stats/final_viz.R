


# Loading libraries -------------------------------------------------------

library(understatr)
library(magrittr)
library(tidyverse)
library(sysfonts)
library(ggtext)

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

# Getting the hex codes for the required teams included in analysis -------


club_cols <- c(
  "Luton" = "#F78F1E",            # Orange
  "Burnley" = "#6C1D45",          # Dark Burgundy
  "Sheffield United" = "#FF0000", # Bright Red
  "Leicester" = "#003090",        # Dark Blue
  "Ipswich" = "#00AEEF",          # Cyan-ish Blue
  "Southampton" = "#D71920",      # Deep Red
  "Burnley 23/24 " = "#FF69B4",   # Hot Pink (distinct from regular Burnley)
  "Leeds" = "#1D428A",            # Navy Blue
  "Sunderland" = "#EB172B"        # Bright Red/Scarlet
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

# Adding required fonts for the pllot -------------------------------------
font_add_google("Poppins", "poppins")
showtext_auto()




# Labels for final match week plot ----------------------------------------
label_df <- df %>%
  group_by(team_name) %>%
  slice_max(MatchWeek, n = 1, with_ties = FALSE)

df<-df %>%
  arrange(match(team_name, order_teams))


#Final Plot code
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
  geom_vline(
    data = df %>% filter(year == "2025"),
    aes(xintercept = 16),
    color = "red",
    linewidth = 1,
    linetype = "dashed"
  )+
    facet_wrap(
      ~ factor(facet,levels = unique(df$facet)),
      scales = "free_x",
      ncol = 3
    ) +
  scale_color_manual(values = club_cols) +
  labs(
    title = "Promoted Teams Usually Struggle — But Premier League 2025/26 is different",
     subtitle = glue::glue(
      "All three promoted sides were relegated to the Championship in the last 2 seasons.\n",
      "Sunderland have already beaten Chelsea & Newcastle and sit strong in the top half\n",
      "Leeds and Burnley still need to catch up—only 16 match weeks played so far."
    ),
    x = "Match Week",
    y = "Cumulative Points",
    caption = glue::glue(
      "Survival benchmarks have shifted: ~32 points were enough in 2023/24, rising to ~38 points in 2024/25.\n",
      "Data Source:understatr, Viz By : Hari Krishna")
  ) +
theme_minimal() +
  theme(
    text=element_text(family="poppins",size=15),
    legend.position = "none",
    strip.text = element_text(family="poppins",face = "bold"),
    panel.grid.minor = element_blank(),
    
    # 🔑 Enable markdown for subtitle
    plot.subtitle = element_text(
      lineheight = 0.9,       # smaller than default
      margin = margin(t = -2, b = 8)  # pulls subtitle closer to title
    ),
    plot.title = element_text(face = "bold",size=15),
  )  + coord_cartesian(clip = "off")

  















