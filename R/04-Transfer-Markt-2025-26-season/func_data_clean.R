
# Function to convert values to Numeric -----------------------------------

convert_value <- function(x) {
  x <- toupper(x)
  if (grepl("M$", x)) {
    as.numeric(readr::parse_number(x)) * 1e6
  } else if (grepl("K$", x)) {
    as.numeric(readr::parse_number(x)) * 1e3
  } else if (suppressWarnings(!is.na(as.numeric(x)))) {
    as.numeric(x)
  } else {
    NA_real_
  }
}


# Format result with M/K suffix
format_value <- function(val) {
  if (val >= 1e6) {
    paste0(round(val / 1e6, 1), "M")
  } else if (val >= 1e3) {
    paste0(round(val / 1e3, 1), "K")
  } else {
    as.character(val)
  }
}

# Clean  data ---------------------------------------
clean_dataset <- function(data, keep_order = NULL, drop = NULL,player_name_col=NULL) {
  data<-data %>%
    # Reorder if given
    { if (!is.null(keep_order)) dplyr::select(., all_of(keep_order), everything()) else . } %>%
    # Drop if given
    { if (!is.null(drop)) dplyr::select(., -all_of(drop)) else . }
  
  data[[player_name_col]]<-data[[player_name_col]] %>% janitor::make_clean_names(case = "title")
  data[[player_name_col]]<-sub(" [a-z] .*", "", data[[player_name_col]]) %>%  sub("^(.+?) \\1$", "\\1", .)
  
  colnames(data)[3]<-"Player"
  colnames(data)[3]<-"Player"
  
  return(data)
}



# Loan/Euro Logic ---------------------------------------------------------
# pl_incomings <- pl_incomings %>%
#   filter(grepl("€", Fee, ignore.case = TRUE) | grepl("transfer", Fee, ignore.case = TRUE))
# 
# pl_outgoings <- pl_outgoings %>%
#   filter(grepl("€", Fee, ignore.case = TRUE) | grepl("transfer", Fee, ignore.case = TRUE))


