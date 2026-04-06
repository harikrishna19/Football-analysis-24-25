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
# font_add_google("Montserrat", "mont")
font_add_google("Oswald", "oswald")
font_add_google("Inter", "inter")
# font_add_google("libre", "libre")
# font_add_google("Libre Franklin", "libre")
showtext_auto()


# =========================
# 🧮 Data prep
# =========================
# Assuming Team_g has: season, goals, player_name, position, percentage
# and total_goals has: season, total_goals
# Team_g$season<-as.character(Team_g$season)
# Team_g <- Team_g %>%
#   mutate(season = as.numeric(season),
#          season = paste0(season, "/", substr(season + 1, 3, 4)))

  total_goals <- total_goals %>%
  mutate(season = as.character(season))
  
  season_levels <- c("2015/16","2016/17","2017/18","2018/19",
                     "2019/20","2020/21","2021/22","2022/23",
                     "2023/24","2024/25","2025/26")
  
  # df$season <- factor(df$season, levels = season_levels)

  Team_g$season <- factor(Team_g$season, levels = season_levels)
  
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
             fill = "gold",
             stroke = 1.1) +
  
  # 🟢 Percentage trend (secondary axis)
  geom_line(aes(y = percentage * scale_factor),
            color = "#6be675",
            linewidth = 1.2,
            linetype = "dashed") +

  geom_point(aes(y = percentage * scale_factor),
             color = "#6be675",
             size = 2.5) +

  # 🏷 Labels
  geom_text_repel(
    aes(y = goals, label = player_name),
    color = "white",
    size = 5,
    fontface = "bold",
    family = "roboto",
    
    nudge_y = 0.5,              # 👈 reduce distance
    box.padding = 0.2,          # 👈 tighter label spacing
    point.padding = 0.1,        # 👈 closer to point
    
    segment.color = "#aaaaaa",
    segment.alpha = 0.5,
    segment.size = 0.3,
    min.segment.length = 0,     # 👈 allows very short lines
    
    max.overlaps = Inf
  )+
  # 🎯 Key insights
  annotate("text",
           x = "2018/19",
           y = 22,
           label = "Hazard was the amongst the goals post Costa",
           family = "roboto",   # 👈 add this
           color = "#f4c430",
           fontface = "bold",
           size = 6) +
  annotate("text",
           x = "2021/22",
           y = max(Team_g$goals)*0.85,
           label = "No consistent No.9:\nDip in Goals from 2019-2022",
           family = "roboto",   # 👈 add this
           color = "#ff6b6b",
           fontface = "bold",
           size = 6) +

  annotate("text",
           x = "2024/25",
           y = 23,
           label = "Rise of Cole Palmer",
           family = "roboto",   # 👈 add this
           color = "green",
           size = 6,
           fontface = "bold") +
  
  annotate("text",
           x = "2025/26",
           family = "roboto",   # 👈 add this
           y = 10,
           label = "Current Season***\nPedro,Nkuku,Delap\nas forward options",
           color = "cyan",
           size = 4.5,
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
    title = NULL,
    # subtitle = "Goals vs % Contribution by top scorer",
    x = "Top Scorer by Season",
    y = NULL
  ) +
  
  theme_minimal(base_family = "roboto", base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#081633", color = NA),
    panel.background = element_rect(fill = "#081633", color = NA),
    
    panel.grid.major.y = element_line(color = "#1c355e", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text.x = element_text(color = "white", face = "bold", size = 18),
    axis.text.y = element_text(color = "white", face = "bold", size = 18),
    
    axis.title.x = element_text(color = "white", face = "bold", size = 18),
    axis.title.y = element_text(color = "white", face = "bold", size = 18),
    legend.title = element_blank(),
    legend.text = element_text(color = "white"),
    legend.position = "bottom",
    
    plot.title = element_text(family = "oswald", color = "white", size = 18, face = "bold"),
    plot.subtitle = element_text(family = "oswald",color = "#d6a66b", size = 12)
  )

# =========================
# 📊 PLOT 2 — Total Goals
# =========================
p2 <- ggplot(total_goals, aes(x = as.character(season), y = total_goals)) +
  
  # Glow effect (using lighter color instead of alpha)
  geom_col(aes(fill = ifelse(season == 2022, "highlight", "normal")),
           width = 0.6,
           alpha = 0.5) +
  
  # Main bars
  geom_col(aes(fill = ifelse(season == 2022, "highlight", "normal")),
           width = 0.45) +
  
  scale_fill_manual(values = c(
    "normal" = "#c9a55c",
    "highlight" = "#ff4d4d"
  ), guide = "none") +
  
  # Labels inside bars
  geom_text(aes(label = total_goals),
            vjust = 1.4,
            color = "black",
            family="inter",
            size = 7,
            fontface = "bold") +
  annotate("text",
           x = "2022",
           y = 45,
           family="inter",
           label = "Major Dip",
           color = "#00f5d4",
           fontface = "bold",
           size = 5)+
  
  labs(
    title = "Chelsea Goals Premier League(2016-2026)",
    # subtitle = "Drop in output post Costa era",
    x = NULL,
    y = NULL
  ) +
  
  scale_y_continuous(expand = c(0, 0)) +
  
  theme_minimal(base_family = "oswald", base_size = 14) +
  theme(
    text = element_text(family = "inter"),
    plot.background = element_rect(fill = "#081633", color = NA),
    panel.background = element_rect(fill = "#081633", color = NA),
    
    panel.grid.major.y = element_line(color = "white"),
    panel.grid.major.x = element_blank(),
    
    axis.text = element_text(color = "white"),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    plot.title = element_text(color = "white", size = 16, face = "bold",family="oswald"),
    plot.subtitle  = element_text(color = "white", size = 16, face = "bold")
  ) 
# 🧩 Combine (Patchwork)
# =========================
final_plot <- (p1 / p2) +
  plot_layout(heights = c(4, 2)) +
  plot_annotation(
    title = "From Costa to Palmer: A Decade of Chelsea Goals in the Premier League",
    caption="Data: UnderStatAPI",
    
    theme = theme(
      plot.title = element_text(
        
        size = 22,
        face = "bold",
        color = "gold",
        family = "oswald",
        hjust = 0.5   # center align
      ),
      plot.caption = element_text(color="white",size=20,family="oswald"),
      plot.subtitle = element_text(
        size = 20,
        color = "#d6a66b",
        family = "oswald",
        lineheight = 1.4,   # spacing between lines
        margin = margin(t = 10, b = 10)
      ),
      
      plot.background = element_rect(fill = "#081633", color = NA)
    )
  )

# =========================
# 📌 Show plot
# =========================
final_plot
