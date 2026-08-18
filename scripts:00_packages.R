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

# Preliminary data exploration
BE_jobs_size <- read.csv("business-establishments-and-jobs-data-by-business-size-and-anzsic.csv")
BE_add_indu <- read.csv("business-establishments-with-address-and-industry-classification.csv")

glimpse(BE_jobs_size) # Small one (Aggregated bby business size)
glimpse(BE_add_indu) # Big one (All businesses)

locations <- BE_add_indu |> 
  group_by(clue_small_area) |>
  summarise(n = n())

View(locations)
