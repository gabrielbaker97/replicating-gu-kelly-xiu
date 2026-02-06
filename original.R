library(tidyverse)
library(archive)
library(arrow)
library(RSQLite)
library(tidymodels)
library(butcher)
library(future)
library(tidyfinance)

# Data download
options(timeout = 10800)

url <- "https://dachxiu.chicagobooth.edu/download/datashare.zip"
dest <- "data/datashare.zip"

# Only download if not already present
if (!file.exists(dest)) {
  download.file(url, dest, mode = "wb")
}

# Read from local file
characteristics <- read_csv(
  archive_read(dest, file = "datashare.csv")
)

# Your cleaning pipeline
characteristics <- characteristics |>
  rename(month = DATE) |>
  mutate(
    month = ymd(month),
    month = floor_date(month, "month")
  ) |>
  rename_with(~ paste0("characteristic_", .), -c(permno, month, sic2)) |>
  drop_na(sic2) |>
  mutate(sic2 = as_factor(sic2))

# Rank transform
rank_transform <- function(x) {
  rank_x <- rank(x)
  rank_x[is.na(x)] <- NA
  min_rank <- 1
  max_rank <- length(na.omit(x))

  if (max_rank == 0) {
    # only NAs
    return(rep(NA, length(x)))
  } else {
    return(2 * ((rank_x - min_rank) / (max_rank - min_rank) - 0.5))
  }
}

characteristics <- characteristics |>
  group_by(month) |>
  mutate(across(contains("characteristic"), rank_transform)) |>
  ungroup()

# Replace NA with 0
characteristics <- characteristics |>
  group_by(month) |>
  mutate(across(contains("characteristic"), \(x) {
    replace_na(x, median(x, na.rm = TRUE))
  })) |>
  ungroup()

characteristics <- characteristics |>
  mutate(across(contains("characteristic"), \(x) replace_na(x, 0)))

# CRSP
