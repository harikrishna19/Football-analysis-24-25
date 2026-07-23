# Loading local function. using devtools for fetching data
library(engsoccerdata)

# Load england championship data

eng_cship<-engsoccerdata::england_current()
eng_cship<-eng_cship %>% dplyr::filter(division==2)



