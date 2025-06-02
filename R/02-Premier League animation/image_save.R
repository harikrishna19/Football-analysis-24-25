library(magick)
library(av)

# Step 1: Load the GIF
gif_path <- "combined.gif"
gif <- image_read(gif_path)

# Step 2: Extract frames and save them as temporary images
temp_dir <- tempdir()
frame_files <- vector("character", length = length(gif))

for (i in seq_along(gif)) {
  frame_file <- file.path(temp_dir, sprintf("frame_%03d.png", i))
  image_write(gif[i], path = frame_file, format = "png")
  frame_files[i] <- frame_file
}

# Step 3: Convert frames to video using av
output_video <- "output_video1.mp4"
av_encode_video(
  input = frame_files,
  output = output_video,
  framerate = 10, # Adjust as needed
)

cat("Video saved to:", output_video)