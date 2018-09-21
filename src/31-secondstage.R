#!/usr/bin/env Rscript
# Compute bootstrap CIs for second-stage of all models

source("pkgs.R")
source("helpers.R")
source("optparse.R")
source("setup.R")

# Private function --------------------------------------------------------
.compute_estimates <- function(p, base_group, minority_groups,
                               controls, links) {
  # Given some policy object `p`, compute all combinations of estimates for
  # minority_groups X controls X links
  # (for figure 2)
  # TODO(jongbin): undi should be updated to encompass these different models

  # Args:
  #   p: policy object
  #   base_group: (string) base group to compare against (e.g. "White")
  #   minority_groups: other groups to compare against the base_group
  #   controls: list of different controls; each item should have named elements
  #       name: name of the controls specification
  #       controls: character vector of variable names to control for
  #   links: list of link functions to iterate over; each item should include
  #       name: human-readable name of the link function
  #       spec: string specification of link function (used in formula)
  #       fun: actual function to be used for model fitting
  specs <- cross_df(list(groups = minority_groups,
                         controls = controls,
                         links = links))
  risk_cuts <- with(p$data, c(0, quantile(resp_trt__[fold == "train"],
                                          seq(0.1, 0.9, 0.1)), 1))

  map_dfr(1:nrow(specs), function(i) {
      group <- specs[i, ]$groups
      control <- specs[i, ]$controls[[1]]
      link <- specs[i, ]$links[[1]]

      test_df <- p$data %>%
        filter(fold == "test", suspect.race %in% c(base_group, group))

      if (link$name == "Decile bins") {
        test_df <- test_df %>%
          mutate(bin__ = cut(resp_trt__, risk_cuts))
      }

      m <- link$fun(make_formula("ptrt__", c(p$grouping,
                                             control$controls,
                                             link$spec)),
                    "binomial", test_df)

      broom::tidy(m, parametric=T)[2,] %>%
        mutate(controls = control$name,
               spec = link$name,
               term = gsub(p$grouping, "", term))
  })
}

# Set modeling params -----------------------------------------------------
controls <- list(
    list(controls = "", name = "Risk"),
    list(controls = c("precinct", "location.housing"),
         name = "Risk and location")
)

links <- list(
    ll = list(spec = "logit(resp_trt__)", fun = glm, name = "Logit-linear"),
    db = list(spec = "bin__", fun = glm, name = "Decile bins"),
    tps = list(spec = "s(logit(resp_trt__))", fun = mgcv::gam,
               name = "Thin plate spline")
)

link_options <-
  map_chr(names(links), ~ sprintf("%s: %s", .x, links[[.x]]$name)) %>%
  paste(collapse = ", ")

# Set and unpack command line arguments -----------------------------------
OPT_PARSER <- OptionParser(option_list = OPT_LIST)
OPT_PARSER <- add_option(OPT_PARSER, c("-l", "--link"),
                         default = "ll",
                         help = sprintf("Second stage model type. One of %s.",
                                        link_options))
opt <- parse_args(OPT_PARSER)

message(sprintf("Loading policies for %s data", opt$target))

source(sprintf("setup_%s.R", opt$target))
source(sprintf("params_%s.R", opt$target))

label_suffix <- paste0("result_", opt$link)

# Compute rad from all policies -------------------------------------------
policies_rds <- get_policy_path(opt, type = "bootstrap")
policies <- read_rds(policies_rds)
bs_rds <- get_policy_path(opt, type = paste0("bs_", label_suffix))

B <- length(policies)
message(sprintf("\t> loaded %d bootstrap samples", B))

rads_df <- future_map_dfr(policies,
                          .progress = TRUE,
                          ~ .compute_estimates(.x,
                                               base_group,
                                               minority_groups,
                                               controls,
                                               links[opt$link]))

message(sprintf("Writing %s ...", bs_rds))
write_rds(rads_df, bs_rds)

# Compute naive estimate --------------------------------------------------
pol_rds <- get_policy_path(opt)
pol <- read_rds(pol_rds)
est_rds <- get_policy_path(opt, type = label_suffix)

est_df <- .compute_estimates(pol, base_group, minority_groups, controls,
                             links[opt$link])

message(sprintf("Writing %s ...", est_rds))
write_rds(est_df, est_rds)
