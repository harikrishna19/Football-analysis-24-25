

#Getiing HEX codes for teams all 20 teams


tmp<-league_positions %>% filter(Wk==38)
hex_codes=data.frame("Team"=tmp$Team)
hex_codes$Codes <- c(
  "#C8102E",  # Liverpool
  "#EF0107",  # Arsenal
  "#6CABDD",  # Manchester City
  "#034694",  # Chelsea
  "#241F20",  # Newcastle Utd
  "#670E36",  # Aston Villa
  "#E53233",  # Nott'ham Forest
  "#0057B8",  # Brighton
  "#DA291C",  # Bournemouth
  "#E30613",  # Brentford
  "#000000",  # Fulham
  "#1B458F",  # Crystal Palace
  "#003399",  # Everton
  "#7A263A",  # West Ham
  "#DA291C",  # Manchester Utd
  "#FDB913",  # Wolves
  "#132257",  # Tottenham
  "#003090",  # Leicester City
  "#005BAA",  # Ipswich Town
  "#D71920"   # Southampton
)




# # # #Adding images

team_urls <- "https://www.transfermarkt.co.in/premier-league/tabelle/wettbewerb/GB1/saison_id/2024"

hex_codes['team_imgs']=read_html(team_urls) %>% html_elements("#yw1 .no-border-rechts img") %>% html_attr("src")

hex_codes$team_imgs<-gsub("tiny","head",hex_codes$team_imgs)


#Merging images to the original data
league_positions<-merge(league_positions,hex_codes,by="Team")
league_positions$team_imgs<- sub("\\.png.*$", ".png", league_positions$team_imgs)

hex_codes$team_imgs<- sub("\\.png.*$", ".png", hex_codes$team_imgs)


