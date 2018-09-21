# Base features used for model training, where the first feature is protected
# group (e.g., Race)
base_feats <- c(
  "suspect.race",
  "suspected.crime",
  "year",
  "month",
  "hour",
  "precinct",
  "location.housing",
  "suspect.sex",
  "suspect.age",
  "suspect.height",
  "suspect.weight",
  "suspect.hair",
  "suspect.eye",
  "suspect.build",
  "additional.report",
  "additional.investigation",
  "additional.proximity",
  "additional.evasive",
  "additional.associating",
  "additional.direction",
  "additional.highcrime",
  "additional.time",
  "additional.sights",
  "additional.other",
  "stopped.bc.object",
  "stopped.bc.desc",
  "stopped.bc.casing",
  "stopped.bc.lookout",
  "stopped.bc.clothing",
  "stopped.bc.drugs",
  "stopped.bc.furtive",
  "stopped.bc.violent",
  "stopped.bc.bulge",
  "stopped.bc.other"
  )

message("Loading model covariates base_feats:\n",
        paste0(base_feats, collapse = ", "))

# policy arguments
treatment <- "frisked"
outcome <- "found.weapon"
# Risk column, either resp_ctl (bail) or resp_trt (stop-and-frisk)
risk_col <- "resp_trt"

# Extra arguments to pass when fitting initial policy
policy_args <- list(resp_ctl = 0)

# Model specific arguments (if any) to pass to policy()
model_args <- list(
  l1 = list(),
  l2 = list(),
  rf = list(),
  gbm = list(verbose = TRUE,
             n.trees = 1000)
)

# Features that are visible under censored simulation
cens_feats <- c(
  "suspect.race",
  "location.housing",
  "suspected.crime",
  "precinct",
  "suspect.sex",
  "suspect.age"
  )

# Group membership definitions
base_group <- "White"
minority_groups <- c("Black", "Hispanic")

# Factors that are considered leigitimate policy concerns, in addition to risk
valid_controls <- list(
  location = c("location.housing", "precinct")
  )

# Sensitivity params ------------------------------------------------------
# Range of p(u | x) to search
qs <- c(0, .25, .35, .4)

# Whether or not to search dp, d0, d1
search_dp <- TRUE
search_d0 <- FALSE
search_d1 <- TRUE
