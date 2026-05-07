# =========================================================
# FULL FT-STYLE INFOGRAPHIC
# UK COMMUNICATION COVERAGE
# COMPLETE VERSION (NO OVERLAPS)
# =========================================================

# =========================
# LIBRARIES
# =========================

library(tidyverse)
library(waffle)
library(patchwork)
library(cowplot)
library(grid)
library(gridExtra)

# =========================================================
# DATA
# =========================================================

df <- tribble(
  ~region, ~metric, ~type, ~value,
  
  "UK Average", "Mobile Calls", "Urban", 99,
  "UK Average", "Mobile Calls", "Rural", 72,
  "UK Average", "4G Speeds", "Urban", 51,
  "UK Average", "4G Speeds", "Rural", 4,
  
  "Northern Ireland", "Mobile Calls", "Urban", 95,
  "Northern Ireland", "Mobile Calls", "Rural", 69,
  "Northern Ireland", "4G Speeds", "Urban", 0,
  "Northern Ireland", "4G Speeds", "Rural", 0,
  
  "Wales", "Mobile Calls", "Urban", 84,
  "Wales", "Mobile Calls", "Rural", 54,
  "Wales", "4G Speeds", "Urban", 24,
  "Wales", "4G Speeds", "Rural", 1,
  
  "Scotland", "Mobile Calls", "Urban", 99,
  "Scotland", "Mobile Calls", "Rural", 64,
  "Scotland", "4G Speeds", "Urban", 44,
  "Scotland", "4G Speeds", "Rural", 4,
  
  "England", "Mobile Calls", "Urban", 99,
  "England", "Mobile Calls", "Rural", 76,
  "England", "4G Speeds", "Urban", 54,
  "England", "4G Speeds", "Rural", 6
)

# =========================================================
# COLORS
# =========================================================

urban_col <- "#2F6DB3"
rural_col <- "#A32652"

bg_col    <- "#F7F2EA"
empty_col <- "#EFE5D7"

text_col  <- "#222222"
muted_col <- "#666666"

# =========================================================
# THEME
# =========================================================

theme_set(
  theme_minimal(base_family = "sans")
)

# =========================================================
# WAFFLE FUNCTION
# =========================================================

make_waffle <- function(value, fill_col) {
  
  filled <- round(value)
  empty  <- 100 - filled
  
  waffle_data <- c(
    Filled = filled,
    Empty = empty
  )
  
  waffle(
    waffle_data,
    rows = 10,
    size = 0.22,
    colors = c(fill_col, empty_col),
    flip = TRUE
  ) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = bg_col, color = NA),
      plot.margin = margin(0,0,0,0)
    ) +
    labs(title = paste0(value, "%")) +
    theme(
      plot.title = element_text(
        size = 16,
        face = "bold",
        hjust = 0.5,
        colour = fill_col
      )
    )
}

# =========================================================
# REGIONAL INSIGHTS
# =========================================================

insight_text <- tribble(
  ~region, ~text,
  
  "UK Average",
  "Calls coverage is high,\nbut 4G speeds drop\nsharply in rural areas.",
  
  "Northern Ireland",
  "Strong call coverage\nin rural areas,\nbut no 4G\ncoverage detected.",
  
  "Wales",
  "Rural coverage\nis lower than\nUK average\nfor both calls\nand 4G.",
  
  "Scotland",
  "Call coverage\nis high, but\n4G speeds\nremain limited\nin rural areas.",
  
  "England",
  "Best rural\nperformance\noverall, but\nstill a wide gap\nfor 4G speeds."
)

# =========================================================
# RIGHT SIDE CALLOUTS
# =========================================================

callouts <- tribble(
  ~region, ~text, ~color,
  
  "UK Average",
  "Urban 4G\ncoverage is\n12x higher\nthan rural\n(51% vs 4%)",
  rural_col,
  
  "Northern Ireland",
  "No 4G speed\ncoverage in\nrural Northern\nIreland.",
  "#E67E22",
  
  "Wales",
  "Rural 4G\ncoverage in\nWales is just\n1%.",
  rural_col,
  
  "Scotland",
  "Scotland mirrors\nthe UK average\ntrend.",
  urban_col,
  
  "England",
  "England has\nthe highest\nrural 4G\ncoverage\n(6%).",
  "#2E8B57"
)

# =========================================================
# REGION ROW BUILDER
# =========================================================

build_row <- function(region_name) {
  
  temp <- df %>%
    filter(region == region_name)
  
  insight <- insight_text %>%
    filter(region == region_name) %>%
    pull(text)
  
  callout <- callouts %>%
    filter(region == region_name)
  
  # ---------------------------------
  # LEFT TEXT
  # ---------------------------------
  
  left_text <- ggplot() +
    
    annotate(
      "text",
      x = 0,
      y = 1,
      label = toupper(region_name),
      hjust = 0,
      vjust = 1,
      fontface = "bold",
      size = 6,
      colour = text_col
    ) +
    
    annotate(
      "text",
      x = 0,
      y = 0.55,
      label = insight,
      hjust = 0,
      vjust = 1,
      lineheight = 1.2,
      size = 4.2,
      colour = muted_col
    ) +
    
    xlim(0,1) +
    ylim(0,1) +
    
    theme_void() +
    
    theme(
      plot.background = element_rect(fill = bg_col, color = NA)
    )
  
  # ---------------------------------
  # WAFFLES
  # ---------------------------------
  
  p1 <- make_waffle(
    temp %>% filter(metric == "Mobile Calls", type == "Urban") %>% pull(value),
    urban_col
  )
  
  p2 <- make_waffle(
    temp %>% filter(metric == "Mobile Calls", type == "Rural") %>% pull(value),
    rural_col
  )
  
  p3 <- make_waffle(
    temp %>% filter(metric == "4G Speeds", type == "Urban") %>% pull(value),
    urban_col
  )
  
  p4 <- make_waffle(
    temp %>% filter(metric == "4G Speeds", type == "Rural") %>% pull(value),
    rural_col
  )
  
  # ---------------------------------
  # RIGHT CALLOUT
  # ---------------------------------
  
  right_callout <- ggplot() +
    
    annotate(
      "label",
      x = 0.5,
      y = 0.5,
      label = callout$text,
      fill = "white",
      colour = callout$color,
      size = 4.5,
      label.size = 0,
      fontface = "bold",
      lineheight = 1.2
    ) +
    
    xlim(0,1) +
    ylim(0,1) +
    
    theme_void() +
    
    theme(
      plot.background = element_rect(fill = bg_col, color = NA)
    )
  
  # ---------------------------------
  # COMBINE
  # ---------------------------------
  
  row_plot <-
    left_text +
    p1 +
    p2 +
    p3 +
    p4 +
    right_callout +
    
    plot_layout(
      widths = c(1.8,1,1,1,1,1.3)
    )
  
  row_plot
}

# =========================================================
# HEADER
# =========================================================

header <- ggplot() +
  
  annotate(
    "text",
    x = 0,
    y = 1,
    label = "UK coverage of communication services",
    hjust = 0,
    size = 7,
    fontface = "bold",
    colour = text_col
  ) +
  
  annotate(
    "text",
    x = 0,
    y = 0.76,
    label = "Urban vs rural divide remains stark across the UK",
    hjust = 0,
    size = 6,
    colour = muted_col
  ) +
  
  # legend
  annotate(
    "rect",
    xmin = 0,
    xmax = 0.02,
    ymin = 0.45,
    ymax = 0.53,
    fill = urban_col
  ) +
  
  annotate(
    "text",
    x = 0.03,
    y = 0.49,
    label = "Urban",
    hjust = 0,
    size = 5
  ) +
  
  annotate(
    "rect",
    xmin = 0.13,
    xmax = 0.15,
    ymin = 0.45,
    ymax = 0.53,
    fill = rural_col
  ) +
  
  annotate(
    "text",
    x = 0.16,
    y = 0.49,
    label = "Rural",
    hjust = 0,
    size = 5
  ) +
  
  annotate(
    "text",
    x = 0.43,
    y = 0.48,
    label = "Mobile phone calls",
    size = 6,
    fontface = "bold"
  ) +
  
  annotate(
    "text",
    x = 0.71,
    y = 0.48,
    label = "Mobile 4G speeds",
    size = 6,
    fontface = "bold"
  ) +
  
  annotate(
    "text",
    x = 0.43,
    y = 0.35,
    label = "% population covered",
    size = 4.3,
    colour = muted_col
  ) +
  
  annotate(
    "text",
    x = 0.71,
    y = 0.35,
    label = "% population covered",
    size = 4.3,
    colour = muted_col
  ) +
  
  xlim(0,1) +
  ylim(0,1) +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(fill = bg_col, color = NA)
  )

# =========================================================
# BUILD ROWS
# =========================================================

uk_row <- build_row("UK Average")
ni_row <- build_row("Northern Ireland")
wa_row <- build_row("Wales")
sc_row <- build_row("Scotland")
en_row <- build_row("England")

# =========================================================
# DIVIDER
# =========================================================

divider <- ggplot() +
  
  geom_hline(
    yintercept = 0.5,
    colour = "#CFC7BA",
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(fill = bg_col, color = NA)
  )

# =========================================================
# BOTTOM SUMMARY SECTION
# =========================================================

# -----------------------------
# RANKING CHART
# -----------------------------

ranking <- tribble(
  ~region, ~value,
  "England", 6,
  "Scotland", 4,
  "Wales", 1,
  "Northern Ireland", 0
)

ranking_plot <- ggplot(
  ranking,
  aes(
    x = reorder(region, value),
    y = value
  )
) +
  
  geom_col(
    fill = rural_col,
    width = 0.55
  ) +
  
  coord_flip() +
  
  geom_text(
    aes(label = paste0(value, "%")),
    hjust = -0.2,
    size = 4
  ) +
  
  labs(
    title = "RURAL 4G COVERAGE RANKING"
  ) +
  
  ylim(0,7) +
  
  theme_minimal() +
  
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(
      face = "bold",
      size = 12
    ),
    plot.background = element_rect(fill = bg_col, color = NA),
    panel.background = element_rect(fill = bg_col, color = NA)
  )

# -----------------------------
# GAP SUMMARY
# -----------------------------

gap_plot <- ggplot() +
  
  annotate(
    "text",
    x = 0,
    y = 1,
    label = "URBAN vs RURAL GAP (UK AVERAGE)",
    hjust = 0,
    fontface = "bold",
    size = 5
  ) +
  
  annotate(
    "text",
    x = 0,
    y = 0.65,
    label = "Mobile phone calls",
    hjust = 0,
    size = 4.2
  ) +
  
  annotate(
    "text",
    x = 0.5,
    y = 0.65,
    label = "Mobile 4G speeds",
    hjust = 0,
    size = 4.2
  ) +
  
  annotate(
    "text",
    x = 0.05,
    y = 0.35,
    label = "27\npp gap",
    colour = urban_col,
    fontface = "bold",
    size = 8
  ) +
  
  annotate(
    "text",
    x = 0.55,
    y = 0.35,
    label = "47\npp gap",
    colour = rural_col,
    fontface = "bold",
    size = 8
  ) +
  
  xlim(0,1) +
  ylim(0,1) +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(fill = bg_col, color = NA)
  )

# -----------------------------
# TAKEAWAYS
# -----------------------------

takeaways <- ggplot() +
  
  annotate(
    "text",
    x = 0,
    y = 1,
    label = "TOP TAKEAWAYS",
    hjust = 0,
    fontface = "bold",
    size = 5.5,
    colour = urban_col
  ) +
  
  annotate(
    "text",
    x = 0,
    y = 0.68,
    label = "• Call coverage is strong across the UK\n\n• Rural areas face a major 4G gap\n\n• Investment in rural connectivity remains critical",
    hjust = 0,
    size = 4.4,
    lineheight = 1.2
  ) +
  
  xlim(0,1) +
  ylim(0,1) +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(fill = bg_col, color = NA)
  )

bottom_panel <-
  takeaways +
  gap_plot +
  ranking_plot +
  
  plot_layout(widths = c(1.4,1.2,1.2))

# =========================================================
# FINAL ASSEMBLY
# =========================================================

final_plot <-
  
  header /
  
  uk_row /
  divider /
  
  ni_row /
  divider /
  
  wa_row /
  divider /
  
  sc_row /
  divider /
  
  # en_row /
  # divider /
  
  # bottom_panel +
  
  plot_layout(
    heights = c(
      10,
      6,0.08,
      6,0.08,
      6,0.08,
      6,0.08,
      6,0.08,
      1.8
    )
  ) &
  
  theme(
    plot.background = element_rect(
      fill = bg_col,
      color = NA
    )
  )

# =========================================================
# EXPORT
# =========================================================

ggsave(
  "FT_style_communication_coverage.png",
  final_plot,
  width = 16,
  height = 22,
  dpi = 350,
  bg = bg_col
)

# =========================================================
# DISPLAY
# =========================================================

final_plot