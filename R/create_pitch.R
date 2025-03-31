  library(ggplot2)
  
  # Function to create a soccer field
  create_pitch <- function() {
    # Generate points for the center circle
    theta <- seq(0, 2 * pi, length.out = 100)
    circle <- data.frame(
      x = 50 + 10 * cos(theta),  # Center at (50,50) with radius 10
      y = 50 + 10 * sin(theta)
    )
    
    ggplot() +
      # Draw field outline
      geom_rect(aes(xmin = 0, xmax = 100, ymin = 0, ymax = 100), fill = "lightgreen", color = "black") +
      # Draw center circle using geom_path()
      geom_path(data = circle, aes(x, y), color = "black") +
      # Goals
      geom_rect(aes(xmin = 45, xmax = 55, ymin = 0, ymax = 5), fill = NA, color = "black") +
      geom_rect(aes(xmin = 45, xmax = 55, ymin = 95, ymax = 100), fill = NA, color = "black") +
      theme_void()
  }
  
  # Define a 4-2-3-1 formation
  formation_4231 <- data.frame(
    x = c(50, 30, 70, 20, 80, 35, 65, 20, 50, 80, 50),  # X-coordinates
    y = c(5, 25, 25, 40, 40, 55, 55, 70, 75, 70, 90),    # Y-coordinates
    label = c("Alisson", "Van Dijk", "Konate", "Robertson", "Arnold", "Mac Allister","Gravenberch", "Diaz", "Szoboszlai", "Salah", "Jota")  # Player positions
  )
  
  # Plot formation
  build_pitch <- create_pitch() +
    geom_point(data = formation_4231, aes(x, y), color = "red", size = 5) +
    geom_text(data = formation_4231, aes(x, y, label = label), vjust = -1, color = "red") +
    labs(
      title = "Liverpool FC 4-2-3-1 Formation",
      caption = "Diogo Jota leads the attack, while Mohamed Salah and Luis Díaz operate on the flanks.\n
                Szoboszlai plays as the No. 10,supported by a double pivot of Mac Allister & Gravenberch in midfield.\n
                In defense,Arnold & Robertson occupy the full-back positions, with Konaté and Van Dijk forming the center-back pairing.\n
                Alisson Becker remains the first-choice goalkeeper."
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Center title and make it bold
      plot.caption = element_text(hjust = 0.3, size = 8, face = "bold", color = "black", lineheight = 1.2)  # Center caption with proper spacing
    )
  
  
  