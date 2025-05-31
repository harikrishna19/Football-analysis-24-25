
library(ggplot2)
library(gganimate)
library(dplyr)
library(tidyr)
library(ggimage)
library(patchwork)
library(magick)

# Sample data
set.seed(123)
df <- data.frame(
  time = rep(1:10, each = 5),
  group = rep(LETTERS[1:5], times = 10),
  value = runif(50, 0, 100)
)

# Image mapping
img_files <- c(
  A = "https://a.espncdn.com/combiner/i?img=/i/leaguelogos/soccer/500/23.png&transparent=true&w=30&h=30",
  B = "https://a.espncdn.com/combiner/i?img=/i/leaguelogos/soccer/500/23.png&transparent=true&w=30&h=30",
  C = "https://a.espncdn.com/combiner/i?img=/i/leaguelogos/soccer/500/23.png&transparent=true&w=30&h=30",
  D = "https://a.espncdn.com/combiner/i?img=/i/leaguelogos/soccer/500/23.png&transparent=true&w=30&h=30",
  E = "https://a.espncdn.com/combiner/i?img=/i/leaguelogos/soccer/500/23.png&transparent=true&w=30&h=30"
)

# Add image to df
df$image <- img_files[df$group]

# Function to generate the plot with image on Y-axis
create_plot_with_image_yaxis <- function(frame_df) {
  frame_df <- frame_df %>%
    mutate(y_pos = factor(group, levels = rev(LETTERS[1:5]))) # reverse order for plot
  
  ggplot(frame_df, aes(x = value, y = y_pos)) +
    geom_col(aes(fill = group), width = 0.6) +
    ggimage::geom_image(aes(image = image), x = -5, size = 0.05) +
    scale_y_discrete(expand = expansion(mult = c(0.1, 0.1))) +
    xlim(-10, 100) +
    labs(title = paste("Time:", frame_df$time[1]), x = "Value", y = NULL) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )
}
create_plot_with_image_yaxis(df)
# Function to create table-style plot
create_table_plot <- function(data) {
  data <- data %>%
    mutate(row = factor(group, levels = rev(LETTERS[1:5]))) %>%
    mutate(fill_color = ifelse(value >= 60, "highlight", "normal"))
  
  ggplot(data, aes(x = "Value", y = row)) +
    geom_tile(aes(fill = fill_color), width = 1, height = 0.9) +
    geom_text(aes(label = round(value, 1)), color = "black") +
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
create_table_plot(df)
# Generate frames and save using magick
plot_frames <- df %>%
  group_by(time) %>%
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
magick::image_write(animated, "gganimate_with_image_axis.gif")

