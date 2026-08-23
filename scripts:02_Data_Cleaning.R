# ============================================================
# CMCE30005 Business Analytics Challenge
# Script: 00_packages.R
# Purpose: Install and load all packages required for this project
# Author: [CMCE30005-Group-5]
# Date: [13/08/2026]
# ============================================================

# Install packages (run once - comment out after installation)
# install.packages(c(
#   "tidyverse",    # Data manipulation and visualisation
#   "lubridate",    # Date handling
#   "janitor",      # Data cleaning utilities
#   "skimr",        # Quick data summary
#   "naniar",       # Missing data visualisation
#   "corrplot",     # Correlation matrices
#   "ggcorrplot",   # ggplot2-style correlation matrices
#   "scales",       # Number formatting in plots
#   "patchwork",    # Combining ggplot2 plots
#   "knitr",        # Report generation
#   "rmarkdown"     # R Markdown rendering
# ))

# Load packages
library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(naniar)
library(ggcorrplot)
library(scales)
library(patchwork)

# Session info (for reproducibility) s  
sessionInfo()

df_small <- read.csv("business-establishments-and-jobs-data-by-business-size-and-anzsic.csv") # Aggregated by census year, CLUE area, ANSZIC Industry, Biz size
df_large <- read.csv("business-establishments-with-address-and-industry-classification.csv") # Aggregated by address-level


#### 1. EXPLORE df_large
dim(df_large)
glimpse(df_large)

# Count missing values in each col
colSums(is.na(df_large))

# Count distinct CLUE areas and ANZSIC industries
sapply(df_large[c("clue_small_area","industry_anzsic4_description")], n_distinct)

# Check range of census data
range(df_large$census_year)

# Check for duplicate rows
sum(duplicated(df_large))


#### 1. Compare number of observations (aggregated by Census year, CLUE area) in df_small vs. df_large.

df_large_summary <- df_large |>
  count(census_year, clue_small_area, name = "n_from_df_large")

df_small_summary <- df_small |>
  group_by(Census.year, CLUE.small.area) |>
  summarise(n_from_df_small = sum(Total.establishments), .groups = "drop")

compare <- df_large_summary |>
  left_join(df_small_summary, by = c("census_year" = "Census.year", "clue_small_area" = "CLUE.small.area"))
