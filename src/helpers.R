if (!exists("HELPERS_R__")) {
  source("consts.R")

  # Helper functions in an attempt to make life easier and code more concise
  make_formula <- function(y, vars) {
    vars <- vars[vars != ""]
    as.formula(paste(y, "~", paste(vars, collapse = "+")))
  }

  logit <- binomial()$linkfun
  inv_logit <- binomial()$linkinv

# Helpful getters ---------------------------------------------------------
  get_scaling_type <- function(opt) {
    # Return the full character of scaling type, based on (partial)
    # matching with opt$scaling

    # Args:
    #   opts: list of options parsed via parse_args()

    # Returns:
    #   Full character string of appropriate scaling type
    #   (raw/isotonic/platt)
    valid_types <- c("raw", "platt", "isotonic")

    for (type in valid_types) {
      if (grepl(opt$calibrate, type)) {
        return(type)
      }
    }

    stop("Unknown scaling type: ", opt$calibrate,
         "\nExpecting one of: ", paste(valid_types, collapse = ", "))
  }

  get_rds_path <- function(subdir, opt, prefix = "fit") {
    # Create file path for an rds file given a subdirectory and a list of
    # hyperparameter settings

    # Args:
    #   subdir: subdirectory for storing rds file
    #   opt: list of options parsed via parse_args()
    #   prefix: prefix to prepend to filename (default: "fit")

    # Returns:
    #   path to an rds file in the form of
    #   DATA_DIR/{subdir}/{riskmodel}_{prefix}_{hyperparams}.rds
    pattern <- file.path(DATA_DIR, subdir,
                         paste0(opt$riskmodel, "_", prefix,
                                "_c", opt$calibrate,
                                "_s", opt$seed, ".rds"))

    return(pattern)
  }

  get_df_path <- function(subdir, opt, prefix = "dfs") {
    # Create file path using just number of data points and random seed; primarily
    # for getting the path to a intermediate data frame used to feed data into
    # stan

    # Args:
    #   subdir: subdirectory for saving data frame
    #   opt: list of options parsed via parse_args()
    #   prefix: prefix to prepend to filename (default: "dfs")

    # Returns:
    #   constructed string path to rds file based on hyperparameters in the form
    #   DATA_DIR/{subdir}/dfs_{prefix}_{ndata_seed}.rds
    pattern = file.path(DATA_DIR, subdir, paste0(prefix, "_s%s.rds"))

    sprintf(pattern, opt$seed)
  }

  get_policy_path <- function(opt, type = "policy") {
    get_df_path(opt$target, opt, paste(type, opt$riskmodel, sep = "_"))
  }

  get_csv_path <- function(subdir, opt, prefix) {
    # Args:
    #   subdir: subdirectory for saving data frame
    #   opt: list of options parsed via parse_args()
    #   prefix: prefix to prepend to filename (default: "dfs")

    # Returns:
    #   constructed string path to rds file based on hyperparameters in the form
    #   DATA_DIR/{subdir}/{prefix}_s{seed}.csv
    pattern = file.path(DATA_DIR, subdir, paste0(prefix, "_s%s.csv"))

    sprintf(pattern, opt$seed)
  }

  get_opt_pattern <- function(opt) {
    # Generate a string of selected hyperparameters used in filenames

    # Args:
    #   opts: list of options parsed via parse_args()

    # Returns:
    #   String concatinated for use in a filename or figure caption
    pattern <- "s%s"

    sprintf(pattern, opt$seed)
  }

  get_undi <- function() {
    # Convenient wrapper to reinstall/update `undi` from source
    devtools::install_github("jongbinjung/undi")
  }

# Printing/debuging functions ---------------------------------------------
  print_sep <- function(msg = "") {
    if (nchar(msg) < 1) {
      message(rep("-", 80))
    } else {
      spaces <- max(4, 80 - 1 - nchar(msg))
      message(msg, " ", rep("-", spaces))
    }
  }

  HELPERS_R__ <- TRUE
} else {
  message("helpers.R already loaded")
}
