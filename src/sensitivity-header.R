#!/usr/bin/env Rscript
# Header for defining common sensitivity functions/variables

source("pkgs.R")
source("helpers.R")
source("modeling.R")
source("optparse.R")
source("setup.R")
source("plot.R")

# Set and unpack command line arguments -----------------------------------
OPT_PARSER <- OptionParser(option_list = OPT_LIST)
OPT_PARSER <- add_option(OPT_PARSER, c("--edl"),
                         default = 1, type = "double",
                         help = "Lower bound of exp(delta) in sensitivity")
OPT_PARSER <- add_option(OPT_PARSER, c("--edu"),
                         default = 3, type = "double",
                         help = "Upper bound of exp(delta) in sensitivity")
opt <- parse_args(OPT_PARSER)

message(sprintf("Setting up sensitivity for %s", opt$target))
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
pol_rds <- get_policy_path(opt)
cens_rds <- get_policy_path(opt, "cens")
synth_rds <- get_policy_path(opt, "synth")

cens_output_rds <- get_policy_path(opt, "cens_output")
full_output_rds <- get_policy_path(opt, "full_output")

# Private functions -------------------------------------------------------
.write_params <- function(sens, type) {
  # Wrapper to extract parameters for each extreme value of a sens object

  # Args:
  #   sens: the sens object to extract min/max params from
  prefix <- file.path("optim_params",
                        paste0(type, "_",
                               opt$riskmodel, "_",
                               get_scaling_type(opt), "_",
                               "edl", opt$edl, "_edu", opt$edu))
  csv_path <- get_csv_path(opt$target, opt, prefix)
  check_path(csv_path)

  message(sprintf("Writing optim params to: %s", csv_path))
  sens$results %>%
    select(tag, controls, pars) %>%
    unnest(pars) %>%
    transmute(tag = tag,
              controls = controls,
              q_base = qb,
              q_minority = qm,
              alpha = exp(ab),
              delta_0 = exp(d0b),
              delta_1 = exp(d1b)) %>%
    write_csv(path = csv_path)
}
