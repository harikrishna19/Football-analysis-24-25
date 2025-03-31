


# Libraries to load -------------------------------------------------------

library(ggplot2)
library(ggthemes)
library(patchwork)
library(dplyr)
library(magrittr)



# Graphs Liverpool Analysis-Time Ranges -----------------------------------------------


goal_ranges<- liv_match_summaries %>%
  dplyr::filter(Team == "Liverpool", Event_Type %in% c("Goal", "Penalty")) %>%
  dplyr::select(Team, Event_Half, Event_Type, Event_Players, Event_Time) %>%
  dplyr::mutate(Time_Range = factor(dplyr::case_when(
    Event_Time >= 0  & Event_Time <= 15  ~ "0-15",
    Event_Time >= 16 & Event_Time <= 30  ~ "16-30",
    Event_Time >= 31 & Event_Time <= 45  ~ "31-45",
    Event_Time >= 46 & Event_Time <= 60  ~ "46-60",
    Event_Time >= 61 & Event_Time <= 75  ~ "61-75",
    Event_Time >= 76 & Event_Time <= 90  ~ "76-90",
    Event_Time >= 91                     ~ "90+"
  ), levels = c("90+", "76-90", "61-75", "46-60", "31-45", "16-30", "0-15"))) %>% 
  dplyr::count(Time_Range, name = "Count") %>%
  ggplot(aes(x=Time_Range, y=Count)) +
  geom_col(fill="red") +
  coord_flip()+ labs(
    title = "Liverpool Goals and Penalties by Time Range",
    subtitle = "Distribution of Goals and Penalties Across Match Minutes",
    x = "Time Range (Minutes)",
    y = "Goal/Penalty Count",
    caption ="Built by: Hari Krishna"
  ) +theme_clean()  # Clean theme for better visuals




# Goal Scorers for Liverpool ----------------------------------------------
#unique goal scoresers for liverpool
Goal_Scorers=liv_match_summaries %>%
  dplyr::filter(Team == "Liverpool", Event_Type %in% c("Goal", "Penalty")) %>%
  dplyr::select(Team, Event_Half, Event_Type, Event_Players, Event_Time) %>% 
  dplyr::count(Event_Players, name = "Goals Scored") %>%
  dplyr::arrange(desc(`Goals Scored`))  # Sort in descending order

liv_match_summaries<-read.csv("match_summaries.csv")

# Clean text: Remove numbers and "&rsquor;"


# Split into a vector based on "·"
players_vector <- str_split(str_replace_all(liv_match_summaries$Home_Goals, "\\s*\\d+&rsquor;", ""), " · ",simplify = F)


# Convert list of vectors into a clean output
output <- map(str_replace_all(liv_match_summaries$Home_Goals, "\\s*\\d+&rsquor;", ""), ~ paste("[1]", paste(.x, collapse = " ")))

# Print the output
output



#new logic for goal scorers calculation

liv_match_summaries$Home_Goals<-map(str_replace_all(liv_match_summaries$Home_Goals, "\\s*\\d+&rsquor;", ""), ~ paste("[1]", paste(.x, collapse = " "))) %>% as.vector()
liv_match_summaries$Away_Goals<-map(str_replace_all(liv_match_summaries$Home_Goals, "\\s*\\d+&rsquor;", ""), ~ paste("[1]", paste(.x, collapse = " "))) %>% as.vector()


library(dplyr)
library(stringr)

# Sample data
df <- data.frame(A = c(
  "Salah. Jones .",
  "Jones. Salah.",
  "Kane. Williamson.",
  "Salah. Diaz."
))

# Define names to count
names_to_count <- c("Salah", "Diaz", "Gapko")

# Count occurrences of each name
name_counts <- sapply(names_to_count, function(name) sum(str_count(liv_match_summaries$Home_Goals, paste0("\\b", name, "\\b"))))

# Convert to a data frame
name_counts_df <- data.frame(Name = names_to_count, Count = name_counts)

# Print results
print(name_counts_df)



new_logic <- liv_match_summaries %>%
  mutate(Team_Names = if_else(Home_Team != "Liverpool", str_split(str_replace_all(Away_Goals, "\\s*\\d+&rsquor;", ""), " · ", simplify = TRUE), if_else(Away_Team != "Liverpool", str_split(str_replace_all(Home_Goals, "\\s*\\d+&rsquor;", ""), " · ", simplify = TRUE), NA_character_)))






Goal_Scorers$Event_Players1 <- gsub(".*Assist: | [-—]" , "", Goal_Scorers$Event_Players)




goal_scorers<-Goal_Scorers %>% 
  group_by(Event_Players1) %>% 
  summarise(GS = sum(`Goals Scored`)) %>% 
  arrange(desc(GS)) %>%
  mutate(Event_Players1 = factor(Event_Players1, levels = rev(Event_Players1))) %>%  # Preserve order
  ggplot(aes(x = Event_Players1, y = GS, fill = ifelse(Event_Players1 %in%
                                                       c("Mohamed Salah","Mohamed Salah Penalty Kick"),  "gold", "red"))) +
  geom_col() +  # No need to manually specify fill in geom_col()
  coord_flip() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +  # Ensure integer breaks
  scale_fill_identity() +  # Ensure colors are used directly
  labs(
    title = "Top Goal Scorers for Liverpool 2024/25 season",
    x = "Players",
    y = "Goals Scored",
    caption = "Built by: Hari Krishna"
  ) +
  theme_clean()  # Assuming you want a clean theme

goal_ranges/goal_scorers



#Liverpool Home/Away Record


liv_match_results<-worldfootballR::fb_match_results(country = "ENG",
                                                    gender = "M",season_end_year = "2025",tier = "1st")

aa=liv_match_results %>% dplyr::filter(Home=="Liverpool") %>% dplyr::summarise(HomeGoals=sum(HomeGoals,na.rm = T),
                                                                            AwayGoals=sum(AwayGoals,na.rm = T),
                                                                            GD=HomeGoals-AwayGoals)

bb=liv_match_results %>% dplyr::filter(Away=="Liverpool") %>% dplyr::summarise(HomeGoals=sum(HomeGoals,na.rm = T),
                                                                            AwayGoals=sum(AwayGoals,na.rm = T),
                                                                            GD=AwayGoals-HomeGoals)



tab=reactable(rbind(aa,bb))




goal_ranges/goal_scorers

grid.arrange(goal_ranges, goal_scorers, ncol = 1)


#----- for single teams: -----#
liv_2025_url <- "https://fbref.com/en/squads/822bd0ba/Liverpool-Stats"
liv_2025_results <- worldfootballR::fb_team_match_results(liv_2025_url)

liv_2025_results %>% 
  dplyr::filter(Comp == "Premier League") %>% 
  dplyr::slice(1:29) %>% 
  dplyr::group_by(Venue) %>% 
  dplyr::summarise(
    Matches = dplyr::n(),  # Count of matches played at each venue
    Wins = sum(Result == "W", na.rm = TRUE),  # Count of Wins
    Losses = sum(Result == "L", na.rm = TRUE),  # Count of Losses
    Draw=sum(Result=="D",na.rm = T),
    Points = (Wins * 3) + (Draw * 1),  # Calculating total points
    "Goals Forward" = sum(as.numeric(GF), na.rm = TRUE),
    "Goals Conceded" = sum(as.numeric(GA), na.rm = TRUE)
  )

#----- get all team URLs for a league: -----#
# epl_2021_team_urls <- fb_teams_urls("https://fbref.com/en/comps/9/Premier-League-Stats")
# epl_2021_team_results <- fb_team_match_results(team_url = team_urls)



#Calculate assists ratio

# Add goals conced as well in the range chart

#Introduce new charts if necessary

build_pitch+goal_ranges+test/test2



goal_scorers<-Goal_Scorers %>% 
  group_by(Event_Players1) %>% 
  summarise(GS = sum(`Goals Scored`)) %>% 
  arrange(desc(GS)) %>%
  mutate(Event_Players1 = factor(Event_Players1, levels = rev(Event_Players1))) %>%  # Preserve order
  ggplot(aes(x = Event_Players1, y = GS, fill = ifelse(Event_Players1 %in%
                                                         c("Mohamed Salah","Mohamed Salah Penalty Kick"),  "gold", "red"))) +
  geom_col() +  # No need to manually specify fill in geom_col()
  coord_flip() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +  # Ensure integer breaks
  scale_fill_identity() +  # Ensure colors are used directly
  labs(
    title = "Top Goal Scorers for Liverpool 2024/25 season",
    x = "Players",
    y = "Goals Scored",
    caption = "Built by: Hari Krishna"
  ) +
  theme_clean()  # Assuming you want a clean theme











library(dplyr)
library(reactable)

# Process data
summary_table <- liv_2025_results %>% 
  dplyr::filter(Comp == "Premier League") %>% 
  dplyr::slice(1:29) %>% 
  dplyr::group_by(Venue) %>% 
  dplyr::summarise(
    Matches = dplyr::n(),  # Count of matches played at each venue
    Wins = sum(Result == "W", na.rm = TRUE),  # Count of Wins
    Losses = sum(Result == "L", na.rm = TRUE),  # Count of Losses
    Draws = sum(Result == "D", na.rm = TRUE),  # Count of Draws
    Points = (Wins * 3) + (Draws * 1),  # Calculating total points
    "Goals Forward" = sum(as.numeric(GF), na.rm = TRUE),
    "Goals Conceded" = sum(as.numeric(GA), na.rm = TRUE)
  ) 
# Create an interactive table with reactable




# ✍️ Reactable Table (Converted to Grid)
reactable_table <- reactable(summary_table, 
                            columns = list(
                              #Venue = colDef(html = TRUE),  # Allow HTML rendering for icons
                              Matches = colDef(name = "Matches Played"),
                              Wins = colDef(name = "Wins"),
                              Losses = colDef(name = "Losses"),
                              Draws = colDef(name = "Draws"),
                              Points = colDef(name = "Total Points"),
                              "Goals Forward" = colDef(name = "Goals Scored"),
                              "Goals Conceded" = colDef(name = "Goals Conceded")
                            ),
                            bordered = TRUE,
                            highlight = TRUE,
                            striped = TRUE,
                            defaultColDef = colDef(align = "center")
)
table_grob <- gridExtra::tableGrob(summary_table)  # Convert to grob

# 🏗️ Combine Everything with Patchwork
final_plot <- (build_pitch+test/test2+goal_ranges) / wrap_elements(full = table_grob)

# 🎉 Display Final Layout
print(final_plot)








library(dplyr)
library(stringr)

# Example data
df <- data.frame(
  events = c(
    "Salah goal",
    "Alisson assist:mane goal",
    "diaz goal:firmino assist",
    "Van Dijk header",
    "Jota scores, Alisson saves"
  )
)

# Define player names
players <- c("Mohamed Salah", " Luis Díaz", "Diogo Jota ", "Cody Gakpo", "Dominik Szoboszlai","Trent Alexander-Arnold","Alexis Mac Allister")

# Create new column with extracted player names
df <- Goal_Scorers %>%
  mutate(selected_players = str_extract_all(Event_Players, str_c(players, collapse = "|")),
         # Extract text after "Assist:"
         assist_info = if_else(str_detect(Event_Players, "Assist:"), 
                               str_extract(Event_Players, "(?<=Assist:).*"), 
                               NA_character_))

# Convert list column to character for readability
df$selected_players <- sapply(df$selected_players, function(x) paste(x, collapse = ", "))

# Print result
print(df)
df %>% group_by(selected_players) %>% summarise(sum(`Goals Scored`))




Goal_Scorers


df<-df %>%
  mutate(column = if_else(str_detect(selected_players, "Mohamed Salah"), "Mohamed Salah", ""))

df %>% group_by(column) %>% summarise(sum(`Goals Scored`))


library(ggplot2)
library(dplyr)

# Define category proportions
data <- c("Non-Penalty Goals" = 18, "Penalty Goals" = 9, "Assists" = 17)

# Create a dataframe with expanded grid ensuring new rows for each category
df <- data.frame(
  category = rep(names(data), times = data),
  index = 1:sum(data)
)

# Adjust tile positioning to start new rows for "Penalty Goals" and "Assists"
df <- df %>%
  mutate(
    adjusted_index = case_when(
      category == "Penalty Goals" ~ index + (10 - (18 %% 10)),  # Move "Penalty Goals" to new row
      category == "Assists" ~ index + (10 - (18 %% 10)) + (10 - (9 %% 10)),  # Move "Assists" to another new row
      TRUE ~ index
    )
  ) %>%
  arrange(adjusted_index)  # Reorder after adjustments

# Define grid size
df$x <- (df$adjusted_index - 1) %% 10 + 1  # Columns
df$y <- 10 - (df$adjusted_index - 1) %/% 10  # Rows (inverted for layout)

# Plot with ggplot2
plot<-ggplot(df, aes(x, y, fill = category)) +
  geom_tile(color = "black") +  # Creates grid squares
  scale_fill_manual(values = c(
    "Non-Penalty Goals" = "#FF5733",  # Red
    "Penalty Goals" = "#33FF57",  # Green
    "Assists" = "#3357FF"  # Blue
  )) +
  theme_void() +  # Removes axes and background
  coord_fixed() +  # Ensures square aspect ratio
  ggtitle("Mohammed Salah scoring in 24/25 season of the Premier League")


















