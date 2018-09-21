#!/usr/bin/env Rscript
# !diagnostics off
# Preprocess and clean data

source("setup_sqf.R")
source("pkgs.R")

# Read data ---------------------------------------------------------------
load(SOURCE_RDA, verbose = TRUE)

cat(sprintf("Starting with %.0f rows\n", nrow(stops)))

# Private functions -------------------------------------------------------
factor_without_rare_values <- function(x, min_count = 300) {
  t = table(x)
  factor(plyr::mapvalues(
    x, names(t)[t < min_count],
    rep(ifelse(sum(t[t < min_count]) < min_count, NA, 'Other'),
        sum(t < min_count))))
}

# Process and clean -------------------------------------------------------
stops <- stops %>%
  filter(year >= 2008, year <= 2011) %>%
  mutate(
    hour = substring(time, 1, 2),
    month = substring(date, 6, 7),
    year = as.character(year),
    suspect.age.bin = cut(suspect.age, c(0, 18, 25, 32, 40, Inf)),
    suspect.race = factor(
      plyr::mapvalues(
        suspect.race,
        c('black', 'black hispanic', 'white', 'white hispanic'),
        c('Black', 'Hispanic', 'White', 'Hispanic')),
      c('White','Black', 'Hispanic')),
    is_white = suspect.race == 'White'
  ) %>%
  filter(!is.na(suspect.race),
         !is.na(suspect.sex)) %>%
  mutate_if(function(x) is.character(x) | is.logical(x) | is.factor(x),
            factor_without_rare_values) %>%
  mutate_at(vars(found.weapon, frisked, is_white), funs(as.logical))

# Final touch -------------------------------------------------------------
# Split data into 50:50 TRAIN/TEST data
set.seed(90210)
stops$fold_id <- sample(rep_len(1:2, nrow(stops)))

# Save data ---------------------------------------------------------------
# Check if directory exits, and create if it doesn't
check_path(TARGET_RDS)

# Save data
message("\nSaving clean data file to:", TARGET_RDS)
write_rds(stops, TARGET_RDS, compress = "gz")
