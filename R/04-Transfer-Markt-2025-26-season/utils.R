
# Load required libraries -------------------------------------------------

library(rvest)
library(reactable)
library(reactablefmtr)
library(magrittr)
library(readr)
library(janitor)
library(dplyr)


# Teams in 2025/26 Premier League season ----------------------------------

teams<-c("Arsenal","Aston Villa","AFC Bournemouth","Brentford","Brighton","Burnley",
         "Chelsea","Crystal Palace","Everton","Fulham","Leeds United","Liverpool",
         "Manchester City","Manchester United","NewCastle United","Nottingham Forest",
         "Sunderland AFC","Tottenham HotSpur","West Ham","Wolves")

team_colors <- c(
  "Arsenal"           = "#f08080",  # lighter red
  "Aston Villa"       = "#b2a1c7",  
  "AFC Bournemouth"   = "#f5a1a1",  
  "Brentford"         = "#f7a8a8",  
  "Brighton"          = "#7abaff",  
  "Burnley"           = "#d4a6a6",  
  "Chelsea"           = "#6aa5ff",  
  "Crystal Palace"    = "#9dbdf2",  
  "Everton"           = "#77aadd",  
  "Fulham"            = "#d9d9d9",  
  "Leeds United"      = "#f6f6b2",  
  "Liverpool"         = "#ff8c8c",  
  "Manchester City"   = "#b0e0ff",  
  "Manchester United" = "#ff9d9d",  
  "Newcastle United"  = "#d0d0d0",  
  "Nottingham Forest" = "#ff9b9b",  
  "Sunderland AFC"    = "#f59c9c",  
  "Tottenham Hotspur" = "#c2c2ff",  
  "West Ham"          = "#d2a6c7",  
  "Wolves"            = "#ffd27f"   
)


# Clean  data ---------------------------------------
clean_dataset <- function(data, keep_order = NULL, drop = NULL,player_name_col=NULL) {
  data<-data %>%
    # Reorder if given
    { if (!is.null(keep_order)) dplyr::select(., all_of(keep_order), everything()) else . } %>%
    # Drop if given
    { if (!is.null(drop)) dplyr::select(., -all_of(drop)) else . }
  
  data[[player_name_col]]<-data[[player_name_col]] %>% janitor::make_clean_names(case = "title")
  data[[player_name_col]]<-sub(" [a-z] .*", "", data[[player_name_col]]) %>%  sub("^(.+?) \\1$", "\\1", .)
  return(data)
}
