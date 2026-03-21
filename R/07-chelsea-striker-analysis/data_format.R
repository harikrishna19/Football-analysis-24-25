
library(dplyr)
library(jsonlite)
library(tidyr)
library(purrr)


team_data<-read.csv("data/team_data.csv")
chelsea_data<-read.csv("data/chelsea_data.csv")


cols_to_parse <- c("h", "a", "goals", "xG","forecast")

team_data <- team_data %>% filter(as.Date(datetime)<Sys.Date()) %>%
  mutate(across(all_of(cols_to_parse),
                ~ gsub("'", '"', .))) %>%
  mutate(across(all_of(cols_to_parse),
                ~ map(.x, fromJSON))) %>%
  { 
    reduce(cols_to_parse,
           ~ unnest_wider(.x, all_of(.y), names_sep = "_"),
           .init = .)
  }


team_data |> dplyr::filter(h_short_title=="CHE"|season==2016) |> nrow()

