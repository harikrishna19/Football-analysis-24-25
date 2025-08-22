
# Loading required packages for analysis ----------------------------------

library(worldfootballR)
library(tidyverse)


# Fetch Match URLS for pl 2024/25 -------------------------------------------
epl_urls <- worldfootballR::fb_match_urls(country = "ENG", gender = "M", season_end_year = 2025, tier="1st")


