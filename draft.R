
# Load required libraries for analysis ------------------------------------
library(tidyverse)
library(engsoccerdata)




# Load engsoccerdata from devtools ----------------------------------------
eng_cship<-engsoccerdata::england_current()
eng_cship<-eng_cship %>% dplyr::filter(division==2)

# get league table for the season

league_table<-engsoccerdata::maketable_eng(eng_cship,2025,2,2)



# Data Processing ---------------------------------------------------------

# filter out only the top 6
top_6<-head(league_table,6)
teams <- unique(top_6$team)
eng_cship_h <- eng_cship %>% filter(home %in% teams)
eng_cship_v <- eng_cship %>% filter(visitor %in% teams)
eng_cship<-bind_rows(eng_cship_h,eng_cship_v)
eng_cship<-eng_cship %>% distinct()


# Home Away Table ---------------------------------------------------------
away<-maketable_all(eng_cship,2025,2,3,type="away") %>% filter(team %in% teams)
home<-maketable_all(eng_cship,2025,2,3,type="home") %>% filter(team %in% teams)

# Home/Away Data ----------------------------------------------------------
plot_df <-
  home %>%
  rename(Home = Pts) %>%
  left_join(
    away %>% rename(Away = Pts),
    by = "team"
  ) %>%
  mutate(
    Home = -Home,
    Difference = Away - abs(Home)
  ) %>%
  arrange(abs(Home)) %>%
  mutate(
    team = factor(team, levels = team)
  )



# Colours for the plots ---------------------------------------------------

home_col <- "#7DB7E8"
away_col <- "#F4A261"

bg_col <- "#F8FAFC"

grid_col <- "#E2E8F0"

text_col <- "#1E293B"


# Plot --------------------------------------------------------------------

sysfonts::font_add_google("Inter", "Inter")
sysfonts::font_add_google("Bebas Neue","Bebas")
sysfonts::font_add_google("Manrope","Manrope")
sysfonts::font_add_google("Oswald","Oswald")
showtext::showtext_auto()
# library(tibble)
# 
# logos <- tribble(
#   ~team, ~logo,
#   "Coventry City", "https://upload.wikimedia.org/wikipedia/en/5/54/Leeds_United_F.C._logo.svg",
#   "Ipswich Town", "https://upload.wikimedia.org/wikipedia/en/4/43/Ipswich_Town.svg",
#   "Southampton", "https://upload.wikimedia.org/wikipedia/en/c/c9/FC_Southampton.svg",
#   "Middlesbrough", "https://upload.wikimedia.org/wikipedia/en/2/2c/Middlesbrough_FC_crest.svg",
#   "Millwall", "https://upload.wikimedia.org/wikipedia/en/a/a7/Millwall_FC_logo.svg",
#   "Hull City", "https://upload.wikimedia.org/wikipedia/en/5/54/Hull_City_A.F.C._logo.svg"
# )
# 
# plot_df <- plot_df %>%
#   left_join(logos, by = "team")



library(tidyverse)
library(patchwork)

#-------------------------------------------------
# Left panel (Summary)
#-------------------------------------------------
library(ggplot2)
library(ggtext)

summary_plot <-
  
  ggplot() +
  
  # Background
  annotate(
    "rect",
    xmin = 0,
    xmax = 1,
    ymin = 0,
    ymax = 1,
    fill = bg_col,
    colour = NA
  ) +
  
  # Main title
  annotate(
    "text",
    x = 0.05,
    y = 0.95,
    label = "EFL CHAMPIONSHIP",
    family = "Oswald",
    fontface = "bold",
    colour = text_col,
    size = 8,
    hjust = 0
  ) +
  
  # Subtitle
  annotate(
    "text",
    x = 0.05,
    y = 0.90,
    label = "2025/26 Season Review",
    family = "Inter",
    colour = "#64748B",
    size = 4,
    hjust = 0
  ) +
  
  # Accent line
  annotate(
    "segment",
    x = 0.05,
    xend = 0.38,
    y = 0.865,
    yend = 0.865,
    colour = home_col,
    linewidth = 1.2
  ) +
  
  # Rich text block
  geom_richtext(
    aes(x = 0.05, y = 0.81),
    hjust = 0,
    vjust = 1,
    fill = NA,
    label.color = NA,
    family = "Inter",
    size = 4,
    lineheight = 1.35,
    label = paste0(
      
      "The <b>2025/26 EFL Championship</b> featured 24 clubs ",
      "competing across 46 league matches. Promotion, playoff ",
      "qualification and survival were all decided after a highly ",
      "competitive campaign.<br><br>",
      
      "<span style='color:#F4A261;font-size:13pt'><b>SEASON HIGHLIGHTS</b></span><br><br>",
      
      "• Coventry City secured the league title.<br>",
      "• Ipswich Town earned automatic promotion.<br>",
      "• Home teams collected over <b>55%</b> of league points.<br>",
      "• Millwall recorded the strongest away performance.<br>",
      "• More than <b>1,400 goals</b> were scored during the season.<br>",
      "• Second-half performances shaped the playoff race."
      
    )
  ) +
  
  # Footer
  annotate(
    "text",
    x = 0.05,
    y = 0.04,
    label = "Source: engsoccerdata • Visualisation: ggplot2",
    family = "Inter",
    colour = "#94A3B8",
    size = 3,
    hjust = 0
  ) +
  
  coord_cartesian(clip="on") +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(fill = bg_col, colour = NA),
    panel.background = element_rect(fill = bg_col, colour = NA),
    plot.margin = margin(1,1,1,1)
  )


chart<-ggplot(plot_df) +
  # geom_image(
  #   aes(
  #     x = 0,
  #     y = team,
  #     image = logo
  #   ),
  #   size = 0.06
  # )+
  ## Home bars
  geom_col(
    aes(Home, team),
    fill = home_col,
    width = .65
  ) +
  
  ## Away bars
  geom_col(
    aes(Away, team),
    fill = away_col,
    width = .65
  ) +
  
  ## Centre line
  # geom_vline(
  #   xintercept = 0,
  #   linewidth = .8,
  #   colour = "#94A3B8"
  # ) +
  
  ## Home labels
  geom_text(
    aes(Home - 3, team,
        label = abs(Home)),
    colour = text_col,
    fontface = "bold",
    size = 4.5
  ) +
  
  ## Away labels
  geom_text(
    aes(Away + 3, team,
        label = Away),
    colour = text_col,
    fontface = "bold",
    size = 4.5
  ) +
  
  ## HOME title
  annotate(
    "text",
    x = -55,
    y = 6.7,
    label = "HOME",
    colour = home_col,
    fontface = "bold",
    size = 5.5
  ) +
  
  ## AWAY title
  annotate(
    "text",
    x = 55,
    y = 6.7,
    label = "AWAY",
    colour = away_col,
    fontface = "bold",
    size = 5.5
  ) +
  
  ## Difference labels
  # geom_text(
  #   aes(
  #     x = 0,
  #     y = team,
  #     label = paste0(
  #       ifelse(Difference > 0, "+", ""),
  #       Difference
  #     )
  #   ),
  #   fontface = "bold",
  #   colour = "#64748B",
  #   size = 3.8
  # ) +
  
  scale_x_continuous(
    limits = c(-70, 70),
    breaks = seq(-60, 60, 20),
    labels = abs,
    expand = expansion(mult = c(.02, .02))
  ) +
  
  coord_cartesian(clip = "off") +
  
  labs(
    title = "Home vs Away Performance",
    subtitle = "Points won in the 2025/26 EFL Championship season",
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(base_size = 15) +
  
  theme(
    text = element_text(family = "Oswald"),
    plot.background = element_rect(
      fill = bg_col,
      colour = NA
    ),
    
    panel.background = element_rect(
      fill = bg_col,
      colour = NA
    ),
    
    panel.grid.major.y = element_blank(),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_line(
      colour = grid_col,
      linewidth = .5
    ),
    
    axis.text.y = element_text(
      face = "bold",
      colour = text_col,
      size = 12
    ),
    
    axis.text.x = element_text(
      colour = "#64748B",
      size = 11
    ),
    
    axis.title = element_blank(),
    
    plot.title = element_text(
      size = 20,
      face = "bold",
      colour = text_col
    ),
    
    plot.subtitle = element_text(
      size = 12,
      colour = "#64748B"
    ),
    
    plot.margin = margin(
      20,
      60,
      20,
      60
    )
  )
header<-summary_plot + chart




