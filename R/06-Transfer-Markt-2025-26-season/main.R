
# Loading libraries -------------------------------------------------------
library(grid)
library(png)
library(tidyverse)
library(ggforce)
library(ggplot2)
library(ggtext)
library(sysfonts)
library(showtext)
library(sysfonts)

showtext_auto()
font_add_google("oswald","oswald")
# Adding variables --------------------------------------------------------


scraped_data <-
    scrape %>%
    arrange(exp) %>%
    mutate(
      id   = row_number(),
      frac = exp / 200 ,
      # scale to 0–1
      frac_plot = ifelse(frac == 0, 0.005, frac),
      zero_flag = frac == 0,
      label_check = ifelse(exp != 0, paste0(club, "-","",exp, "M"), club)
    )


# Loading PL Logo ---------------------------------------------------------


img <- png::readPNG("pl_logo.png")
g <- rasterGrob(img, interpolate = TRUE)


# Loading hex colours for clubs -------------------------------------------

club_cols <- c(
  "Brighton & Hove Albion" = "#4FA3E3",  # Fresh sky blue
  "Nottingham Forest"     = "#e07c7d",  # Soft coral red
  "Wolves"                = "#F7D046",  # Pale gold
  "Brentford FC"          = "#F44336",  # Light red
  "Sunderland AFC"        = "#9E1B32",  # Salmon red
  "Fulham FC"             = "#4A4A4A",  # Charcoal (not harsh)
  "Aston Villa"           = "#BFD9EA",  # Pastel villa blue
  "AFC Bournemouth"       = "red",  # Soft tomato
  "Tottenham Hotspur"     = "#3C4B6E",  # Muted navy
  "West Ham United"       = "#A44A5E",  # Light claret
  "Crystal Palace"        = "#5B84C4",  # Powder blue
  "Manchester City"       = "#9AD0F5"   # Very light sky blue
)

  

# ggplot-main plot-Radial Bar Chart ----------------------------------------

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
      alpha = 0.08
    ) +
    geom_text(
      aes(x = -4.1, y = id, label = label_check),
      size = 4,
      color = "black",family="oswald",
      fontface = "bold.italic"
    ) +
    geom_richtext(
      aes(x = 9.5, y = -1.5),
      label = "
  <span style='font-size:30px; font-weight:bold;color:#000000;'>
  Premier League  <span style='color:#D00000;'>€453M</span>
  </span><br>",
      fill = NA,
      label.color = NA,alpha=0.01,
      hjust = 0.8
    )+
    annotate(
      "text",
      x = -12.5,
      y = 3.5,
      label = "Teams with no spends",
      size = 4,
      color = "red",
      fontface = "bold",
    ) +
    coord_fixed(clip = "off") +
    scale_fill_manual(values = club_cols) +
    theme_void() +
    theme(
      plot.margin = margin(10, -35, 10, 10),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      legend.position = "none",
      plot.title = element_text(
        colour = "black",
        family = "oswald",
        size = 23,
        face = "bold",
        hjust = 1.5
      ),
      plot.caption = element_text(
        colour = "black",family = "oswald",
        size = 15,vjust = 1.7,
        hjust =0,face = "bold.italic",
        margin = margin(t = -20,l=15)
      )
    ) +
    annotation_custom(
      grob = g,
      xmin = -5.0,
      xmax = -6.9,
      ymin = 26,
      ymax = 30
    )+
    labs(
      title = "Premier League January Spending:2025/26",
      subtitle = glue::glue(
"Manchester City focused on profile optimisation, investing in Semenyo & Guehi \n",
"Tottenham reshaped for system balance — Johnson exits, while Gallagher and Souza arrive \n",
"Crystal Palace FC quietly added depth through Larsen \n",
"Six Premier League clubs, including Arsenal and Liverpool, remained inactive \n",
"West Ham balanced arrivals and exits — Traoré in, Paquetá out",
      ),
      caption = "Viz by Hari Krishna-Data:TransferMarkt"
    )
    
  
  
  
  
  
