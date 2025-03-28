

#----- for single teams: -----#
library(reactable)

liv_2025_url <- "https://fbref.com/en/squads/822bd0ba/Liverpool-Stats"
liv_2025_results <- worldfootballR::fb_team_match_results(liv_2025_url)

table_df<-liv_2025_results %>% 
  dplyr::filter(Comp == "Premier League") %>% 
  dplyr::slice(1:29) %>% 
  dplyr::group_by(Venue) %>% 
  dplyr::summarise(
    Matches = dplyr::n(),  # Count of matches played at each venue
    Wins = sum(Result == "W", na.rm = TRUE),  # Count of Wins
    Losses = sum(Result == "L", na.rm = TRUE),  # Count of Losses
    Draw=sum(Result=="D",na.rm = T),
    Points = (Wins * 3) + (Draw * 1),  # Calculating total points
    "Goals Forward" = sum(as.numeric(GF), na.rm = TRUE),
    "Goals Conceded" = sum(as.numeric(GA), na.rm = TRUE)
  ) %>% reactable(columns = list(
                               #Venue = colDef(html = TRUE),  # Allow HTML rendering for icons
                               Matches = colDef(name = "Matches Played"),
                               Wins = colDef(name = "Wins"),
                               Losses = colDef(name = "Losses"),
                               Draws = colDef(name = "Draws"),
                               Points = colDef(name = "Total Points"),
                               "Goals Forward" = colDef(name = "Goals Scored"),
                               "Goals Conceded" = colDef(name = "Goals Conceded")
                             ),
                             bordered = TRUE,
                             highlight = TRUE,
                             striped = TRUE,
                             defaultColDef = colDef(align = "center")
)
