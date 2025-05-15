# 1) suppress all package‐startup messages
suppressPackageStartupMessages({
  library(tidyverse)
  library(fuzzyjoin)
})

# 2) turn off readr’s “── Column specification ──” banner
options(readr.show_col_types = FALSE)

# 3) disable dplyr progress bars & summarise messages
options(
  dplyr.show_progress    = FALSE,
  dplyr.summarise.inform = FALSE
)

# 4) silence all vctrs/tibble “New names:” repairs
options(rlib_name_repair_verbosity = "quiet")