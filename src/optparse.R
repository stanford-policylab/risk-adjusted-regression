# Parse modeling commandline options

# Options must be parsed from calling script, e.g.,
# >> opt <- parse_args(OptionParser(option_list = OPT_LIST))

# Additional script-specific flags can be set in each script, prior to calling
# to parse_args, by saving the OptionParser and using add_option(),
# e.g.,
# >> OPT_PARSER <- OptionParser(option_list = OPT_LIST)
# >> OPT_PARSER <- add_option(OPT_LIST, ...)
# >> opt <- parse_args(OPT_PARSER)

library(optparse)

OPT_LIST <- list(
  make_option(c("-r", "--riskmodel"), default = "gbm",
              help = "Baseline risk model to use (e.g., l1 or gbm)"),
  make_option(c("-c", "--calibrate"), default = "raw",
              help = "Scaling scheme to use (raw, platt, isotonic)"),
  make_option(c("-t", "--target"), default = "sqf",
              help = "Target dataset to use (sqf, cpd, bail)"),
  make_option(c("-s", "--seed"), type = "integer", default = 1745,
              help = "Base seed used to generate random seeds")
)
