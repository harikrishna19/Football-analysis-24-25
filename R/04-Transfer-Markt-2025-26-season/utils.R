
# Load required libraries -------------------------------------------------

library(rvest)
library(reactable)
library(reactablefmtr)
library(magrittr)
library(readr)
library(janitor)


# Teams in 2025/26 Premier League season ----------------------------------

teams<-c("Arsenal","Aston Villa","AFC Bournemouth","Brentford","Brighton","Burnley",
         "Chelsea","Crystal Palace","Everton","Fulham","Leeds United","Liverpool",
         "Manchester City","Manchester United","NewCastle United","Nottingham Forest",
         "Sunderland AFC","Tottenham HotSpur","West Ham","Wolves")


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
