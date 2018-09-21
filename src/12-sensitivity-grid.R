#!/usr/bin/env Rscript
# Run full sensitivity via grid search on censored data sets
source("sensitivity-header.R")

Theta <- scan(paste0("Theta_", opt$target, ".value"))

# Sensitivity with vanilla policy -----------------------------------------
pol <- read_rds(pol_rds)

gs <- gridsens(pol,
               qs = qs,
               dps = unique(search_dp * log(1:Theta)),
               d0s = unique(search_d0 * log(1:Theta)),
               d1s = unique(search_d1 * log(1:Theta)),
               include_benchmark = TRUE,
               base_group = base_group,
               minority_groups = minority_groups,
               fit_fn = "logit_coef")

.write_params(gs, "full_grid")

# Grid search with controls -----------------------------------------------
gs_list <- map(names(valid_controls), function(controls_name) {
  controls <- valid_controls[[controls_name]]
  gs <- gridsens(pol,
                 qs = qs,
                 dps = unique(search_dp * log(1:Theta)),
                 d0s = unique(search_d0 * log(1:Theta)),
                 d1s = unique(search_d1 * log(1:Theta)),
                 include_benchmark = FALSE,
                 base_group = base_group,
                 minority_groups = minority_groups,
                 controls = controls,
                 fit_fn = "logit_coef")
  list(name = controls_name, controls = controls, gs = gs)
})

# Append vanilla grid search to list of results
gs_list[["no_controls"]] <- list(name = "None", controls = "", gs = gs)

# Write gridsearch object for future reference ----------------------------
message(sprintf("Writing list of full grid search results as %s ...",
                full_output_rds))
check_path(full_output_rds)
write_rds(gs_list, full_output_rds)
