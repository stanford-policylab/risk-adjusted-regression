#!/usr/bin/env Rscript
# Fit synthetic policy with censored features

source("pkgs.R")
source("helpers.R")
source("modeling.R")
source("optparse.R")
source("setup.R")

# Set and unpack command line arguments -----------------------------------
OPT_PARSER <- OptionParser(option_list = OPT_LIST)
opt <- parse_args(OPT_PARSER)

message(sprintf("Fit synthetic policy with censored features for %s data",
                opt$target))
source(sprintf("params_%s.R", opt$target))

scaling_type <- get_scaling_type(opt)

if (scaling_type == "raw") {
  calibrate <- FALSE
} else if (scaling_type == "platt") {
  calibrate <- TRUE
} else {
  stop("Policy calibration for scaling type ", scaling_type, " not implemented")
}

# Set file paths ----------------------------------------------------------
synth_df_rds <- get_policy_path(opt, "synth")
cens_df_rds <- get_policy_path(opt, "cens")

# Load vanilla policy object ----------------------------------------------
# Fit censored policy
message(sprintf("Censoring features to: %s", paste(cens_feats, collapse = ", ")))
cens_pol <- read_rds(synth_df_rds) %>%
  estimate_policy(features = cens_feats)

# Write policy object -----------------------------------------------------
message(sprintf("Writing %s ...", cens_df_rds))
check_path(cens_df_rds)
write_rds(cens_pol, cens_df_rds)
