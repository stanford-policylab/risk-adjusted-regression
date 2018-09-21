# Base features used for model training, where the first feature is protected
# group (e.g., Race)
base_feats <- c("%GROUP%",
                "OTHER", "PRE_TREATMENT",
                "FEATURES")
message("Loading model covariates base_feats:\n",
        paste0(base_feats, collapse = ", "))

# policy arguments
treatment <- "%TREATMENT%"
outcome <- "%OUTCOME%"
# Risk column, either resp_ctl (bail) or resp_trt (stop-and-frisk)
risk_col <- "%RISK_COL%"

# Extra arguments to pass when fitting initial policy
policy_args <- list()

# Model specific arguments (if any) to pass to policy()
model_args <- list(
  l1 = list(),
  l2 = list(),
  rf = list(),
  gbm = list()
)

# Features that are visible under censored simulation
cens_feats <- c()

# Group membership definitions
base_group <- "white"
minority_groups <- c("black", "hispanic")

# Factors that are considered leigitimate policy concerns, in addition to risk
valid_controls <- c()

# Sensitivity params ------------------------------------------------------
# Range of p(u | x) to search
qs <- c(0, .25, .35, .4)

# Whether or not to search dp, d0, d1
search_dp <- TRUE
search_d0 <- TRUE
search_d1 <- TRUE
