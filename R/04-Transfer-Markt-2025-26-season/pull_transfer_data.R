

# Load Libraries ----------------------------------------------------------
library(rvest)
library(tidyverse)
library(ggforce)



# Pull overall transfer expenditure data 2025/26 season fromm transfer markt.co.uk
read_url <- rvest::read_html(
  "https://www.transfermarkt.co.uk/premier-league/einnahmenausgaben/wettbewerb/GB1/plus/0?ids=a&sa=&saison_id=2025&saison_id_bis=2025&nat=&pos=&altersklasse=&w_s=w&leihe=&intern=0"
)


scrape <- read_url %>% html_table() %>% .[[2]]
colnames(scrape) <- c("SNo",
                      "NA",
                      "club",
                      "exp",
                      "players",
                      "arrivals",
                      "income",
                      "departures",
                      "bal")
scrape$exp <- as.numeric(gsub("[^0-9.]", "", scrape$exp))
scrape$exp[is.na(scrape$exp)] <- 0


# Pull Indivudual Data
ind_transfers<-rvest::read_html("https://www.transfermarkt.co.uk/premier-league/transfers/wettbewerb/GB1/s_w/w") %>% html_table()



