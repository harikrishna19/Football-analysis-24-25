

#Getiing HEX codes for teams all 20 teams



hex_codes=data.frame("Squad"=c("Arsenal",
                               "Aston Villa","Bournemouth",
                               "Brentford","Brighton & Hove Albion",
                               "Burnley","Chelsea","Crystal Palace","Everton","Fulham","Liverpool","Luton Town","Manchester City","Manchester United",
                               "Newcastle United","Nottingham Forest","Sheffield United","Tottenham Hotspur","West Ham United","Wolverhampton Wanderers"))
hex_codes['Codes']<-c("#EF0107",
                      "#94BEE5","#E62333",
                      "#D20000",
                      "#0055a9",
                      "#53162F",
                      "#034694",
                      "#1B458F","#274488","grey",
                      "#D00027","#F78F1E","#98c5e9","#DA020E","#241f20","#DD0000","#ED1A3B","#001C58","#60223B","#FBEE23")




# # # #Adding images

team_urls <- "https://www.transfermarkt.co.in/premier-league/tabelle/wettbewerb/GB1/saison_id/2024"
team_names<-read_html(team_urls) %>% html_elements("a title") %>% html_text()
team_imgs=read_html(team_urls) %>% html_elements("#yw1 .no-border-rechts img") %>% html_attr("src")
