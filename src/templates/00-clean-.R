#!/usr/bin/env Rscript
# !diagnostics off
# Preprocess and clean data

source("setup_%DATASET%.R")
source("pkgs.R")

# Read data ---------------------------------------------------------------
full_df <- read_rds(SOURCE_RDS)

cat(sprintf("Starting with %.0f rows\n", nrow(full_df)))

# Final touch -------------------------------------------------------------
# Split data into 50:50 TRAIN/TEST data
set.seed(90210)
full_df$fold_id <- sample(rep_len(1:2, nrow(full_df)))

# Save data ---------------------------------------------------------------
# Check if directory exits, and create if it doesn't
check_path(TARGET_RDS)

# Save data
message("\nSaving clean data file to:", TARGET_RDS)
write_rds(full_df, TARGET_RDS)
