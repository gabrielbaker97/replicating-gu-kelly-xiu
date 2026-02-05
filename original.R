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
dest <- "datashare.zip"

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
