

library(engsoccerdata)


library(dplyr)
library(purrr)
library(engsoccerdata)

get_league_tables <- function(leagues = c("england", "spain", "italy", "scotland","france","germany"),
                              division = 1) {
  
  # Map league names to functions
  league_functions <- list(
    england = england_current,
    spain = spain_current,
    italy = italy_current,
    scotland = scotland_current,
    france= france_current,
    germany=germany_current
  )
  
  # Validate input
  invalid <- setdiff(leagues, names(league_functions))
  if (length(invalid) > 0) {
    stop(paste("Invalid leagues:", paste(invalid, collapse = ", ")))
  }
  
  # Pull and combine data
  data <- map_df(leagues, function(lg) {
    print("lg")
    df <- league_functions[[lg]]()
    
    df %>%
      filter(division == division) %>%   # division 1 only
      mutate(league = lg)
  })
  
  return(data)
}
