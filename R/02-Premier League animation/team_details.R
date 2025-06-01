

#Getiing HEX codes for teams all 20 teams



hex_codes=data.frame("Team"=c("Arsenal",
                               "Aston Villa","Bournemouth",
                               "Brentford","Brighton",
                               "Leicester City","Chelsea","Crystal Palace","Everton","Fulham","Liverpool","Ipswich Town","Manchester City","Manchester Utd",
                               "Newcastle Utd","Nott'ham Forest","Southampton","Tottenham","West Ham","Wolves"))
hex_codes['Codes']<-c("#EF0107",
                      "#94BEE5","#E62333",
                      "#D20000",
                      "#0055a9",
                      "blue",
                      "#034694",
                      "#1B458F","#274488","grey",
                      "#D00027","blue","#98c5e9","#DA020E","#241f20","#DD0000","#ED1A3B","pink","#60223B","#FBEE23")




# # # #Adding images

team_urls <- "https://www.transfermarkt.co.in/premier-league/tabelle/wettbewerb/GB1/saison_id/2024"

hex_codes['team_imgs']=read_html(team_urls) %>% html_elements("#yw1 .no-border-rechts img") %>% html_attr("src")

hex_codes$team_imgs<-gsub("tiny","head",hex_codes$team_imgs)


#Merging images to the original data
league_positions<-merge(league_positions,hex_codes,by="Team")
league_positions$team_imgs.y<- sub("\\.png.*$", ".png", league_positions$team_imgs.y)





