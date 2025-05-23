
library(ggplot2)
library(dplyr)
library(stringr)

df_filtered <- liv_match_summaries %>%
  filter(str_detect(Event_Players, "Mohamed Salah$",),Event_Type %in% c("Goal","Penalty"))

df_filtered<-liv_match_summaries %>%
  dplyr::filter(Team == "Liverpool", Event_Type %in% c("Goal", "Penalty"),str_detect(Event_Players, "Mohamed Salah"),) %>%
  dplyr::select(Team,Home_Team,Away_Team,Matchweek,Event_Half, Event_Type, Event_Players, Event_Time)

df_filtered$Event_Players[df_filtered$Event_Players == "Alexis Mac Allister Assist: Mohamed Salah"] <- "Mohamed Salah Assist:Alexis Mac Allister"

df_filtered[19, "Event_Players"] <- "Mohamed Salah Assist:Luiz Diaz"






# Keywords to filter and match order
players1 <- c("Mohamed Salah Assist", "Mohamed Salah Penalty Kick", "Mohamed Salah")








df_filtered_ordered <- df_filtered %>%
  mutate(selected_players = str_extract(Event_Players, str_c(players1, collapse = "|"))) %>%
  filter(!is.na(selected_players)) %>%  # Keep only matched values
  arrange(Matchweek,match(selected_players, players1))  # Order by players1 sequence

df_filtered_ordered$Matchweek<-parse_number(df_filtered_ordered$Matchweek)

df_filtered_ordered <- df_filtered_ordered %>%
  mutate(Team_Names = if_else(Home_Team != "Liverpool", toupper(substr(Home_Team,1,3)), if_else(Away_Team != "Liverpool", toupper(substr(Away_Team,1,3)), NA_character_)))

df_filtered_ordered<-df_filtered_ordered %>% arrange(Matchweek)




# Define category counts
category_counts <- c("Goals & Assists" = 35,  # Combined Non-Penalty Goals & Assists
                     "Penalty Goals" = 9)  # Separate Penalty Goals

# Create an expanded data frame where each row represents a tile
df <- 
df_filtered_ordered %>%  mutate(category = case_when(
    selected_players %in% c("Mohamed Salah","Mohamed Salah Assist") ~ "Goals & Assists",
    selected_players =="Mohamed Salah Penalty Kick"~ "Penalty Goals"
  )
)

# Compute grid positions, ensuring "Penalty Goals" is at the bottom
df <- df %>%
  group_by(category) %>%
  mutate(
    x = (row_number() - 1) %% 10 + 1,  # Max 10 columns per row
    y = -((row_number() - 1) %/% 10) +  
      case_when(  
        category == "Goals & Assists" ~ 0,  # Top section
        category == "Penalty Goals" ~ -4  # Bottom section (green)
      )
  ) %>%
  ungroup()

# Define text labels
df$label <- paste(df_filtered_ordered$Team_Names,df_filtered_ordered$Matchweek,sep=",")   # You can modify this to actual team names if available


# Plot with ggplot2
final_plot <- ggplot(df, aes(x, y, fill = category)) +
  geom_tile(color = "black", size = 2.5) +  # Creates grid squares
  geom_text(aes(label = label), size = 3, color = "black") +  # Add text inside each tile
  scale_fill_manual(values = c(
    "Goals & Assists" = "green",  # Blue (Combined Non-Penalty Goals & Assists)
    "Penalty Goals" = "red"  # Green (Bottom)
  )) +
  theme_void() + 
  theme(
    plot.caption = element_text(hjust = 0.5, size = 10, face = "bold.italic", color = "black")  # Align left, italicized, and readable
  )+
  coord_fixed() +  # Ensures square aspect ratio
  labs(title="Salah Goals & Assists by in the 24/25 Premier League Season",
       caption="***Text in the box denotes Team Scored/Assisted and Match Week")

# Display the plot
print(final_plot)











