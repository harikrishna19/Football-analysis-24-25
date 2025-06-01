library(ggplot2)
library(ggimage)
library(patchwork)
library(dplyr)
library(tibble)
library(stringr)
df<-league_positions
df$Wk<-as.numeric(df$Wk)
create_plot_with_image_yaxis <- function(frame_df) {
  ggplot(frame_df, aes(x = Wk, y = Position, group = Team, color = Team)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    geom_text(aes(label = as.numeric(Position)), vjust = -1, size = 3, color = "black") + 
    labs(
      title = paste("Match Week:", max(frame_df$Wk)),
      x = "Matchweek", y = NULL
    ) +
    scale_x_continuous(breaks = 1:38) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank()
    )
}



create_table_plot <- function(data) {
  data <- data %>% arrange(as.numeric(Wk)) %>% 
    mutate(row = factor(Team, levels = rev(Team[1:38]))) %>%
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
cumulative_frames<-head(cumulative_frames,19)
plot_frames <- lapply(cumulative_frames, function(frame_df) {
  current_week_df <- frame_df %>% filter(Wk == max(Wk))  # for table only
  p1 <- create_plot_with_image_yaxis(frame_df)
  p2 <- create_table_plot(current_week_df)
  p1 + p2 + patchwork::plot_layout(ncol = 2, widths = c(3, 1))
  #p1/p2
})

library(magick)

# Convert plots to images
images <- lapply(plot_frames, function(p) {
  img_file <- tempfile(fileext = ".png")
  ggsave(img_file, plot = p, width = 10, height = 6, dpi = 300)
  image_read(img_file)
})

# Combine and animate
animation <- image_animate(image_join(images),fps=1)
image_write(animation, "team_positions.gif")


