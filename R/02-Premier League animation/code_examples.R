# Don't show this plot as animation 
p1<-league_positions %>% 
  filter(Team %in% c("Tottenham", "Southampton", "Ipswich Town", "Manchester Utd","Wolves","Leicester City")) %>%
  mutate(Wk = as.numeric(Wk)) %>%
  ggplot(aes(x = Wk, y = Position, group = Team, color = Team)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept =19)+
  scale_x_continuous(breaks = 1:38) +
  scale_color_manual(values=c("blue", "lightblue", "red","red", "grey", "yellow"))+
  scale_y_reverse(breaks = 1:max(league_positions$Position)) +
  labs(
    title = "Team Positions by Matchweek-Bottom 6 teams",
    subtitle = "This year bottom 6 includes Tottenham and Manchester United",
    caption = "By Hari Krishna",
    x = "Matchweek", y = "Position"
  ) +
  theme_minimal()+
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p1



#Plot animatioon example



all_teams<-# Assign each team a vertical offset
  team_offsets <- league_positions %>%
  distinct(Team) %>%
  mutate(offset = (row_number() - 1) * 25)  # Adjust spacing as needed

# Join with main data
plot_data <- league_positions %>%
  mutate(Wk = as.numeric(Wk)) %>%
  left_join(team_offsets, by = "Team") %>%
  mutate(Position_offset = offset - Position)  # Invert so 1st is still top

all_teams<-ggplot(plot_data, aes(x = Wk, y = offset, group = Team, color = Team)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_text(aes(label = as.integer(Position)), vjust = -1, size = 3, color = "black") + 
    labs(
    title = "Team Positions Over Time (Separated by Team)",
    x = "Matchweek", y = NULL
  ) +
  scale_x_continuous(breaks = 1:38)+
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  )+
  ggthemes::theme_solarized()+
transition_reveal(Wk)


animate(
  fps = 10,
  duration = 10,
  width = 1200,   # Increase width here
  height = 600,
  all_teams + enter_fade() + exit_fly(y_loc = 1),
  renderer = av_renderer()
)


# Image exploring code




library(ggplot2)
library(gganimate)
library(dplyr)
library(tidyr)
library(ggimage)
library(patchwork)
library(magick)

df<-league_positions
df<- df %>% filter(Team %in% c("Arsenal","Liverpool"))

# Image mapping
img_files <- c(
  #Brighton = "https://a.espncdn.com/combiner/i?img=/i/leaguelogos/soccer/500/23.png&transparent=true&w=30&h=30",
  Arsenal = "https://a.espncdn.com/combiner/i?img=/i/leaguelogos/soccer/500/23.png&transparent=true&w=30&h=30",
  Liverpool = "https://a.espncdn.com/combiner/i?img=/i/leaguelogos/soccer/500/23.png&transparent=true&w=30&h=30"
  #Tottenham = "https://a.espncdn.com/combiner/i?img=/i/leaguelogos/soccer/500/23.png&transparent=true&w=30&h=30",
  #Everton = "https://a.espncdn.com/combiner/i?img=/i/leaguelogos/soccer/500/23.png&transparent=true&w=30&h=30"
)

# Add image to df
df$image <- img_files[df$Team]

# # Function to generate the plot with image on Y-axis
# create_plot_with_image_yaxis <- function(frame_df) {
#   ggplot(frame_df, aes(x = Wk, y = Position, group = Team, color = Team)) +
#     geom_line(linewidth = 1.2) +
#     geom_point(size = 2) +
#     geom_text(aes(label = as.integer(Position)), vjust = -1, size = 3, color = "black") + 
#     scale_y_discrete(expand = expansion(mult = c(0.1, 0.1))) +
#     xlim(-10, 100) +
#     labs(title = paste("Match Week:", df$Wk), x = "cPoints", y = NULL) +
#     theme_minimal() +
#     theme(
#       axis.text.y = element_blank(),
#       axis.ticks.y = element_blank(),
#       legend.position = "none"
#     )
# }
# 
# # Function to create table-style plot
# create_table_plot <- function(data) {
#   data <- data %>% arrange(as.numeric(Wk)) %>% 
#     mutate(row = factor(Team, levels = rev(Team[1:2]))) %>%
#     mutate(fill_color = ifelse(cPoints >= 60, "highlight", "normal"))
#   
#   ggplot(data, aes(x = "cPoints", y = row)) +
#     geom_tile(aes(fill = fill_color), width = 1, height = 0.9) +
#     geom_text(aes(label = round(cPoints, 1)), color = "black") +
#     scale_fill_manual(values = c("highlight" = "lightgreen", "normal" = "white")) +
#     theme_minimal() +
#     theme(
#       axis.text.x = element_blank(),
#       axis.ticks.x = element_blank(),
#       axis.title = element_blank(),
#       panel.grid = element_blank(),
#       legend.position = "none"
#     )
# }

df$Wk<-as.numeric(df$Wk)
my_list<-df  %>% 
  arrange(as.numeric(Wk),.by_group = T) %>% 
  group_by(Wk) %>%
  group_split() 


# Reorder by Wk
sorted_list <- my_list[order(sapply(my_list, function(x) unique(x$Wk)))]

# Generate frames and save using magick

cumulative_frames <- lapply(seq_len(max(df$Wk)), function(i) {
  df %>% filter(Wk <= i)
})
plot_frames <- df  %>% 
  arrange(as.numeric(Wk),.by_group = T) %>% 
  group_by(Wk) %>%
  group_split() %>%
  lapply(function(frame_df) {
    p1 <- create_plot_with_image_yaxis(frame_df)
    p2 <- create_table_plot(frame_df)
    p1 + p2 + plot_layout(ncol = 2, widths = c(2.5, 1))
  })

# Save frames as animated GIF
tmp_images <- lapply(seq_along(plot_frames), function(i) {
  file <- tempfile(fileext = ".png")
  ggsave(file, plot_frames[[i]], width = 10, height = 4)
  magick::image_read(file)
})

animated <- magick::image_animate(image_join(tmp_images), fps = 2)
magick::image_write(animated, "gganimate_with_image_axis1.gif")


create_plot_with_image_yaxis <- function(frame_df) {
  ggplot(frame_df, aes(x = Wk, y = -Position, group = Team, color = Team)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    geom_text(aes(label = as.integer(Position)), vjust = -1, size = 3, color = "black") + 
    labs(
      title = paste("Match Week:", max(frame_df$Wk)),
      x = "Matchweek", y = NULL
    ) +
    #scale_x_continuous(breaks = 1:38) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank()
    )
}

create_table_plot <- function(data) {
  data <- data %>% arrange(as.numeric(Wk)) %>% 
    mutate(row = factor(Team, levels = rev(Team[1:2]))) %>%
    mutate(fill_color = ifelse(cPoints >= 60, "highlight", "normal"))
  
  ggplot(data, aes(x = "cPoints", y = row)) +
    geom_tile(aes(fill = fill_color), width = 1, height = 0.9) +
    geom_text(aes(label = round(cPoints, 1)), color = "black") +
    scale_fill_manual(values = c("highlight" = "lightgreen", "normal" = "white")) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )
}
df <- df %>% arrange(Wk)

cumulative_frames <- lapply(seq_len(max(df$Wk)), function(i) {
  df %>% filter(Wk <= i)
})
plot_frames <- lapply(cumulative_frames, function(frame_df) {
  current_week_df <- frame_df %>% filter(Wk == max(Wk))  # for table only
  p1 <- create_plot_with_image_yaxis(frame_df)
  p2 <- create_table_plot(current_week_df)
  p1 + p2 + patchwork::plot_layout(ncol = 2, widths = c(3, 1))
})

library(magick)

# Convert plots to images
images <- lapply(plot_frames, function(p) {
  img_file <- tempfile(fileext = ".png")
  ggsave(img_file, plot = p, width = 10, height = 6, dpi = 150)
  image_read(img_file)
})

# Combine and animate
animation <- image_animate(image_join(images), fps = 4)
image_write(animation, "team_positions.gif")




