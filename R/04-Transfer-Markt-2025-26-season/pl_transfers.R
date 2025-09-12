

# Load liobraries ---------------------------------------------------------


tm_data<-rvest::read_html("https://www.transfermarkt.com/premier-league/transfers/wettbewerb/GB1") %>% html_elements("table") %>% html_table() 
scrape_img<-rvest::read_html("https://www.transfermarkt.com/premier-league/transfers/wettbewerb/GB1") %>% html_elements(".content-box-headline--logo img") %>% html_attr("src") 
pl_transfers<- tm_data[2:41]


# Add team name column to each data frame
pl_transfers <- Map(function(df, nm,img) {
  df$Team <- nm
  df$team_logo <- img
  df
}, pl_transfers, rep(teams,each=2),rep(scrape_img,each=2))

pl_outgoings <- pl_transfers[seq(2, 41, by = 2)]
pl_incomings  <- pl_transfers[seq(3, 41, by = 2)]

pl_outgoings <- do.call(rbind, pl_outgoings)
pl_incomings  <- do.call(rbind, pl_incomings)





