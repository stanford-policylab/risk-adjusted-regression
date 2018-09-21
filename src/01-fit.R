#!/usr/bin/env Rscript
# Fit undi::policy model for specified target

source("pkgs.R")
source("helpers.R")
source("modeling.R")
source("optparse.R")
source("setup.R")

# Set and unpack command line arguments -----------------------------------
OPT_PARSER <- OptionParser(option_list = OPT_LIST)
opt <- parse_args(OPT_PARSER)

message(sprintf("Fitting policy for %s data", opt$target))

source(sprintf("setup_%s.R", opt$target))
source(sprintf("params_%s.R", opt$target))
message(sprintf("Loading clean %s data as full_df from: %s",
                opt$target, TARGET_RDS))
full_df <- read_rds(TARGET_RDS)

df_rds <- get_policy_path(opt)
check_path(df_rds)

scaling_type <- get_scaling_type(opt)

if (scaling_type == "raw") {
  calibrate <- FALSE
} else if (scaling_type == "platt") {
  calibrate <- TRUE
} else {
  stop("Policy calibration for scaling type ", scaling_type, " not implemented")
}

# Create undi::policy objects ---------------------------------------------
full_df <- full_df %>%
  mutate(fold = ifelse(fold_id == TRAIN_FOLD, "train", "test"))

formula <- make_formula(treatment, base_feats)

# Fit initial policy object
pol <- do.call(policy,
               c(formula = formula,
                 data = list(full_df),
                 seed = opt$seed,
                 outcome = outcome,
                 train = "fold",
                 risk = risk_col,
                 calibrate = calibrate,
                 fit1 = models_setup[[opt$riskmodel]]$f_fit,
                 pred1 = models_setup[[opt$riskmodel]]$f_pred,
                 policy_args,
                 model_args[[opt$riskmodel]]))

# Write policy object -----------------------------------------------------
message(sprintf("Writing %s ...", df_rds))
write_rds(pol, df_rds)
