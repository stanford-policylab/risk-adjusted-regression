#!/usr/bin/env Rscript
# Fit bootstrap undi::policy models for specified target

source("pkgs.R")
source("helpers.R")
source("modeling.R")
source("optparse.R")
source("setup.R")

# Set and unpack command line arguments -----------------------------------
OPT_PARSER <- OptionParser(option_list = OPT_LIST)
OPT_PARSER <- add_option(OPT_PARSER, c("-b", "--n_bootstrap"),
                         default = 100, type = "integer",
                         help = "Number of bootstrap samples")
opt <- parse_args(OPT_PARSER)

message(sprintf("Fitting policy for %s data", opt$target))

source(sprintf("setup_%s.R", opt$target))
source(sprintf("params_%s.R", opt$target))
message(sprintf("Loading clean %s data as full_df from: %s",
                opt$target, TARGET_RDS))
full_df <- read_rds(TARGET_RDS)

df_rds <- get_policy_path(opt, type = "bootstrap")
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
# Settings that are equal across bootstrap samples
formula <- make_formula(treatment, base_feats)

full_df <- full_df %>%
  filter(complete.cases(.[base_feats]))

message(sprintf("Fitting %s bootstrap samples", opt$n_bootstrap))

bs_policies <- future_map(1:opt$n_bootstrap, function(b) {
  set.seed(b)

  # Sample with replacement and create 50:50 train/test split
  sample_df <- full_df %>%
    sample_frac(replace = TRUE) %>%
    mutate(fold = sample(c("train", "test"), n(), TRUE))

  # Fit initial policy object
  do.call(policy,
          c(formula = formula,
            data = list(sample_df),
            outcome = outcome,
            train = "fold",
            risk = risk_col,
            calibrate = calibrate,
            fit1 = models_setup[[opt$riskmodel]]$f_fit,
            pred1 = models_setup[[opt$riskmodel]]$f_pred,
            policy_args,
            model_args[[opt$riskmodel]],
            verbose = FALSE))
})

# Write policy object -----------------------------------------------------
message(sprintf("Writing %s ...", df_rds))
write_rds(bs_policies, df_rds)
