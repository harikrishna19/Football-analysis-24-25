# =========================
# 📦 Libraries
# =========================
library(ggplot2)
library(dplyr)
library(ggrepel)
library(patchwork)
library(showtext)

# =========================
# 🔤 Fonts
# =========================
font_add_google("Montserrat", "mont")
showtext_auto()

# =========================
# 🧮 Data prep
# =========================
# Assuming Team_g has: season, goals, player_name, position, percentage
# and total_goals has: season, total_goals

Team_g <- Team_g %>%
  mutate(
    season = as.character(season)
  )

total_goals <- total_goals %>%
  mutate(season = as.character(season))

# Scaling factor for secondary axis
scale_factor <- max(Team_g$goals, na.rm = TRUE) / max(Team_g$percentage, na.rm = TRUE)

# =========================
# 📊 PLOT 1 — Top Scorer + %
# =========================
p1 <- ggplot(Team_g, aes(x = season, group = 1)) +
  
  # ✨ Goals trend
  geom_line(aes(y = goals),
            color = "#d6a66b",
            linewidth = 1.5) +
  
  # ⚽ Points
  geom_point(aes(y = goals, fill = position),
             size = 5.5,
             shape = 21,
             fill = "blue",
             stroke = 1.1) +
  
  # 🟢 Percentage trend (secondary axis)
  # geom_line(aes(y = percentage * scale_factor),
  #           color = "#6be675",
  #           linewidth = 1.2,
  #           linetype = "dashed") +
  # 
  # geom_point(aes(y = percentage * scale_factor),
  #            color = "#6be675",
  #            size = 2.5) +
  # 
  # 🏷 Labels
  geom_text_repel(
    aes(y = goals, label = player_name),
    color = "white",
    size = 3.5,
    fontface = "bold",
    nudge_y = 3,
    segment.color = "#aaaaaa",
    segment.alpha = 0.4,
    box.padding = 0.3,
    max.overlaps = Inf
  ) +
  
  # 🎯 Key insights
  annotate("text",
           x = "2017",
           y = max(Team_g$goals),
           label = "Peak: Costa era",
           color = "#f4c430",
           fontface = "bold",
           size = 4) +
  
  annotate("text",
           x = "2020",
           y = max(Team_g$goals)*0.85,
           label = "No consistent No.9\nShared scoring",
           color = "#ff6b6b",
           fontface = "bold",
           size = 4) +
  
  annotate("text",
           x = "2023",
           y = max(Team_g$goals)*1.1,
           label = "Top scorer contribution declined",
           color = "white",
           size = 4,
           fontface = "bold") +
  
  # 🎨 Scales
  scale_fill_manual(values = c(
    "FWD" = "#f4c430",
    "MID" = "#4a6fa5",
    "DEF" = "#2e3f5b"
  )) +
  
  scale_y_continuous(
    name = "Goals",
    sec.axis = sec_axis(~ . / scale_factor, name = "Contribution (%)")
  ) +
  
  labs(
    title = "Top Scorer by Season",
    subtitle = "Goals vs % Contribution",
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(base_family = "mont", base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#081633", color = NA),
    panel.background = element_rect(fill = "#081633", color = NA),
    
    panel.grid.major.y = element_line(color = "#1c355e", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    
    legend.title = element_blank(),
    legend.text = element_text(color = "white"),
    legend.position = "bottom",
    
    plot.title = element_text(color = "white", size = 18, face = "bold"),
    plot.subtitle = element_text(color = "#d6a66b", size = 12)
  )

# =========================
# 📊 PLOT 2 — Total Goals
# =========================
p2 <- ggplot(total_goals, aes(x = as.character(season), y = total_goals)) +
  
  # Glow effect (using lighter color instead of alpha)
  geom_col(fill = adjustcolor("#d6a66b" ), width = 0.6) +
  
  # Main bars
  geom_col(fill = "#d6a66b", width = 0.45) +
  
  # Labels inside bars
  geom_text(aes(label = total_goals),
            vjust = 1.4,
            color = "black",
            size = 4,
            fontface = "bold") +
  
  labs(
    title = "Chelsea Goals by Season",
    subtitle = "Drop in output post Costa era",
    x = NULL,
    y = NULL
  ) +
  
  scale_y_continuous(expand = c(0, 0)) +
  
  theme_minimal(base_family = "mont", base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#081633", color = NA),
    panel.background = element_rect(fill = "#081633", color = NA),
    
    panel.grid.major.y = element_line(color = "white"),
    panel.grid.major.x = element_blank(),
    
    axis.text = element_text(color = "white"),
    axis.ticks = element_blank(),
    
    plot.title = element_text(color = "white", size = 16, face = "bold")
  )
# 🧩 Combine (Patchwork)
# =========================
final_plot <- (p1 / p2) +
  plot_layout(heights = c(3, 2)) +
  plot_annotation(
    title = "Chelsea's No.9 Problem (2016–2025)",
    
    subtitle = paste0(
      "• No striker has contributed <span style='color:#ff6b6b'><b>>30%</b></span> of team goals<br>",
      "• <span style='color:#f4c430'><b>2022:</b></span> Only <span style='color:#f4c430'><b>38 goals</b></span> in 28 games<br>",
      "• Last title: <span style='color:#6be675'><b>2016/17 (Conte)</b></span>"
    ),
    
    theme = theme(
      plot.title = element_text(
        size = 22,
        face = "bold",
        color = "white",
        family = "mont",
        hjust = 0.5
      ),
      
      # 🔥 THIS IS THE KEY
      plot.subtitle = ggtext::element_markdown(
        size = 13,
        color = "#d6a66b",
        family = "mont",
        lineheight = 1.4,
        hjust = 0.5,
        margin = margin(t = 10, b = 10)
      ),
      
      plot.background = element_rect(fill = "#081633", color = NA)
    )
  )

# =========================
# 📌 Show plot
# =========================
final_plot
