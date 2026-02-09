
# Loading libraries -------------------------------------------------------
library(grid)
library(png)
library(tidyverse)
library(ggforce)
library(ggplot2)
library(ggtext)


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
    "Brighton & Hove Albion"  = "#0057B8",
    # Blue
    "Nottingham Forest"       = "#DD0000",
    # Red
    "Wolves" = "#FDB913",
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
  <span style='font-size:40px; font-weight:bold;color:#000000;'>
  Premier League  <span style='color:#D00000;'>£453M</span>
  </span><br>",
      fill = NA,
      label.color = NA,alpha=0.01,
      hjust = 0.8
    )+
    # geom_richtext(
    #   aes(x = -30.5, y = -10.5),
    #   label = glue::glue(
    #     "<span style='font-family:Helvetica; font-size:15px; line-height:1.35;'>",
    #     "<b style='color:#6CABDD;'>City</b> invest <b>£62M</b> on <b>Semenyo</b> to strengthen the right-wing profile<br>",
    #     "<b>Wolves</b> add depth with the arrival of <b>Larsen</b><br>",
    #     "<b style='color:#132257;'>Tottenham</b>reshape the squad — <b>Johnson exits</b>, <b>Gallagher</b> and <b>Souza</b> arrive for system fit<br>",
    #     "<b>Oscar Bobb</b> secures minutes with a move to <b>Fulham</b><br>",
    #     "<b>Six PL clubs</b>, including <b>Arsenal</b> and <b>Liverpool</b>, remain inactive in January<br>",
    #     "<b>West Ham</b> see movement both ways — <b>Traoré in</b>, <b>Paquetá out</b>",
    #     "</span>"
    #   ),
    #   fill = NA,
    #   label.color = NA,
    #   hjust = 0
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
        hjust = 2.0
      ),
      # plot.subtitle  = element_text(
      #   colour = "black",
      #   family = "oswald",
      #   size = 8,
      #   hjust = 2.5,vjust = 0.8
      # ),
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
    
  
  
  
  
  
