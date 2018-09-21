#!/usr/bin/env Rscript
# Generate synthetic policy

source("pkgs.R")
source("helpers.R")
source("optparse.R")

# Set and unpack command line arguments -----------------------------------
OPT_PARSER <- OptionParser(option_list = OPT_LIST)
opt <- parse_args(OPT_PARSER)

scaling_type <- get_scaling_type(opt)

if (scaling_type == "raw") {
  calibrate <- FALSE
} else if (scaling_type == "platt") {
  calibrate <- TRUE
} else {
  stop("Policy calibration for scaling type ", scaling_type, " not implemented")
}

message(sprintf("Generating synthetic policy for %s data", opt$target))

# Set file paths ----------------------------------------------------------
df_rds <- get_policy_path(opt)
synth_df_rds <- get_policy_path(opt, "synth")

# Load vanilla policy object and synthesize policy ------------------------
# Generate synthetic policy
synth_pol <- read_rds(df_rds) %>%
  synthesize()

# Write policy object -----------------------------------------------------
message(sprintf("Writing %s ...", synth_df_rds))
check_path(synth_df_rds)
write_rds(synth_pol, synth_df_rds)
