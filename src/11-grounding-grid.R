#!/usr/bin/env Rscript
# Ground sensitivity via grid search on censored data sets
source("sensitivity-header.R")

# Load relevant policy objects --------------------------------------------
synth_pol <- read_rds(synth_rds)
cens_pol <- read_rds(cens_rds)

# Sensitivity with censored policy ----------------------------------------
synth_rad <- compute_rad(synth_pol,
                         base_group = base_group,
                         minority_groups = minority_groups,
                         fit_fn = "logit_coef")

gs <- gridsens(cens_pol,
               qs = qs,
               dps = unique(search_dp * log(seq(opt$edl, opt$edu, 1))),
               d0s = unique(search_d0 * log(seq(opt$edl, opt$edu, 1))),
               d1s = unique(search_d1 * log(seq(opt$edl, opt$edu, 1))),
               fit_fn = "logit_coef",
               base_group = base_group,
               minority_groups = minority_groups)

# Find Theta, the approx smallest upper bound on (ab, am, d1b, d1m) such
# that the sensitivity bounds contain the true disparate impact
# value for all races
Theta <- gs$grid %>%
  left_join(
    synth_rad %>%
      dplyr::select(term, estimate),
    by = "term") %>%
  mutate(
    Theta = sapply(pars, function(x) pmax(x$ab, x$am, x$d1b, x$d1m))) %>%
  group_by(Theta, minor) %>%
  summarize(
    cover = any(estimate.y > estimate.x) & any(estimate.y < estimate.x)) %>%
  group_by(Theta) %>%
  filter(all(cover)) %>%
  with(round(min(exp(Theta))))

write(Theta, paste0("Theta_", opt$target, ".value"))

.write_params(gs, "cens_grid")

# Write gridsearch object for future reference ----------------------------
message(sprintf("Writing censored grid search result as %s ...",
                cens_output_rds))
check_path(cens_output_rds)
write_rds(gs, cens_output_rds)
