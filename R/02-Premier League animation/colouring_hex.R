ggplot(league_positions, aes(x = Wk, y = Position, color = Team)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_text(aes(label = PositionLabel), vjust = -0.8, size = 5) +
  labs(
    title = "Team Positions Over Time",
    subtitle = 'Matchweek: {as.integer(frame_along)}',
    x = "Matchweek", y = NULL
  ) +
  scale_x_continuous(breaks = 1:38) +
  scale_colour_manual(values = setNames(league_positions$Codes, league_positions$Team)) +
  scale_y_reverse(breaks = 1:max(league_positions$Position)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  transition_reveal(along = Wk, keep_last = TRUE)






library(ggplot2)

# Sample data
df <- data.frame(
  group = c("B", "A", "C", "B", "A", "C"),
  value = c(3, 4, 5, 2, 3, 4)
)

# Desired legend order
custom_order <- c("C", "A", "B")

# Apply custom order via factor levels
df$group <- factor(df$group, levels = custom_order)

# Plot with custom legend order
ggplot(df, aes(x = value, fill = group)) +
  geom_histogram(position = "dodge", bins = 5) 
