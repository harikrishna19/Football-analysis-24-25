

scraped_data <-
  scrape %>%
  arrange(exp) %>%
  mutate(
    id   = row_number(),
    frac = exp / 200 ,
    # scale to 0–1
    frac_plot = ifelse(frac == 0, 0.005, frac),
    zero_flag = frac == 0,
    label_check = ifelse(exp!=0,paste0(club, "-", exp, "M"),club)
  )

club_cols <- c(
  "Brighton & Hove Albion"  = "#0057B8",
  # Blue
  "Nottingham Forest"       = "#DD0000",
  # Red
  "Wolverhampton Wanderers" = "#FDB913",
  # Old Gold
  "Brentford FC"            = "#E30613",
  # Red
  "Sunderland AFC"          = "#EB172B",
  # Red
  "Fulham FC"               = "#000000",
  # Black
  "Aston Villa"             = "#95BFE5",
  # Claret & Blue (blue tone)
  "AFC Bournemouth"         = "#DA291C",
  # Red
  "Tottenham Hotspur"       = "#132257",
  # Navy
  "West Ham United"         = "#7A263A",
  # Claret
  "Crystal Palace"          = "#1B458F",
  # Blue
  "Manchester City"         = "#6CABDD"   # Sky Blue
)


ggplot(scraped_data) +
  geom_arc_bar(
    aes(
      x0 = 0,
      y0 = 0,
      r0 = id - 0.35,
      r  = id + 0.25,
      start = pi * 0.01,
      end   = pi * 0.01 + 2 * pi * frac_plot,
      fill  = club
      # alpha = zero_flag
    ),
    color = NA
  ) +
  annotate(
    "rect",
    xmin = -0.5,
    xmax = -7.5,
    ymin = 8.5,
    ymax = 0.5,
    fill = "red",
    colour = "black",
    linewidth = 0.5,
    alpha=0.08
  ) +
  geom_text(aes(x = -4.0, y = id, label = label_check),
            size = 2,
            color = "black",fontface="bold") +
  annotate(
    "text",
    x = 5.5,
    y = -0.5,
    label = "Premier League:~453 Million",
    size = 5,
    color = "grey30"
  ) +
  annotate(
    "text",
    x = -12.5,
    y = 3.5,
    label = "Teams with no spends",
    size = 3,
    color = "red",fontface="bold"
  ) +
  coord_fixed(clip = "off") +
  scale_fill_manual(values = club_cols) +
  theme_void() +
  theme(legend.position = "none") +
  labs(
    title = "Premier League Winter Spending of 2025/26",
    subtitle = "Manchester City have spent most of the money in 2025/26 season",
    caption = "Viz by Hari Krishna"
  )
