#!/usr/bin/env Rscript
# Generate additional model-check plots

source("pkgs.R")
source("helpers.R")
source("optparse.R")
source("plot.R")
source("setup.R")

# Set and unpack command line arguments -----------------------------------
OPT_PARSER <- OptionParser(option_list = OPT_LIST)
OPT_PARSER <- add_option(OPT_PARSER, c("--edl"),
                         default = 1, type = "double",
                         help = "Lower bound of exp(delta) in RnR")
OPT_PARSER <- add_option(OPT_PARSER, c("--edu"),
                         default = 3, type = "double",
                         help = "Upper bound of exp(delta) in RnR")
OPT_PARSER <- add_option(OPT_PARSER, c("--output"), default = "all",
                         help = "Ouput plot filename to build")
opt <- parse_args(OPT_PARSER)

source(sprintf("plot_params_%s.R", opt$target))

# Helpful functions -------------------------------------------------------
compute_hitrates <- function(d, groups, v_true, v_est) {
  map_dfr(groups, function(g) {
    v_group <- rlang::sym(g)

    d %>%
      group_by(!!v_group) %>%
      summarize(true_hitrate = mean(!!v_true),
                est_hitrate = mean(!!v_est)) %>%
      ungroup() %>%
      mutate(covar = g) %>%
      rename(group = !!v_group)
    })
}

compute_twoway_hitrates <- function(d, group1, group2, v_true, v_est) {
  g1 <- rlang::sym(group1)
  g2 <- rlang::sym(group2)

  d %>%
    group_by(!!g1, !!g2) %>%
    summarize(true_hitrate = mean(!!v_true),
              est_hitrate = mean(!!v_est),
              count = n()) %>%
    ungroup()
}

write_plot_as <- function(p, filename, ...) {
  # Wrapper to avoid repeating subdirectories
  filepath <- file.path(opt$target,
                        paste0(opt$riskmodel, "_",
                               get_scaling_type(opt), "_",
                               filename, "_", get_opt_pattern(opt), ".png"))

  write_plot(p, filepath, ...)
}

# Read policy object ------------------------------------------------------
pol <- opt %>%
  get_policy_path() %>%
  read_rds()

v_treatment <- rlang::sym(pol$treatment)
v_group <- rlang::sym(pol$grouping)
v_outcome <- rlang::sym(pol$outcome)
s_riskcol <- paste0(pol$risk_col, "__")
v_riskcol <- rlang::sym(s_riskcol)
v_treat <- rlang::sym("ptrt__")

# PLOT: Covariate hitrate (outcome) ---------------------------------------
hitrate_df <- pol$data %>%
  filter(!!v_treatment) %>%
  f_covar_pre() %>%
  compute_hitrates(covar_groups, v_outcome, v_riskcol) %>%
  gather(type, value, true_hitrate, est_hitrate)

p_hitrate <- ggplot(hitrate_df, aes(x = value, y = group)) +
  geom_point(aes(shape = type), size = 3, alpha = .6) +
  scale_shape_manual(element_blank(),
                     values = c(19, 1),
                     limits = c("est_hitrate", "true_hitrate"),
                     breaks = c("est_hitrate", "true_hitrate"),
                     labels = c("Model", "Empirical")) +
  scale_x_continuous(name = covar_hitrate_x_axis$name,
                     limits = covar_hitrate_x_axis$limits,
                     labels = covar_hitrate_x_axis$labels,
                     expand = c(0, 0)) +
  scale_y_discrete(name = covar_y_axis$name,
                   breaks = covar_y_axis$breaks,
                   labels = covar_y_axis$labels) +
  facet_grid(covar ~ ., scale = "free", space = "free",
             labeller = covar_labeller) +
  theme(legend.background = element_blank(),
        legend.position = c(1, 1),
        legend.justification = c(1, 1))
write_plot(p_hitrate, "FigA1a.pdf")
# write_plot_as(p_hitrate,
              # "hitrate_by_covar",
              # width = 5,
              # height = 5,
              # save_rds = TRUE)

# PLOT: Covariate treatrate (treatment) -----------------------------------
treatrate_df <- pol$data %>%
  f_covar_pre() %>%
  compute_hitrates(covar_groups, v_treatment, v_treat) %>%
  gather(type, value, true_hitrate, est_hitrate)

p_treatrate <- ggplot(treatrate_df, aes(x = value, y = group)) +
  geom_point(aes(shape = type), size = 3, alpha = .6) +
  scale_shape_manual(element_blank(),
                     values = c(19, 1),
                     limits = c("est_hitrate", "true_hitrate"),
                     breaks = c("est_hitrate", "true_hitrate"),
                     labels = c("Model", "Empirical")) +
  scale_x_continuous(name = covar_treatrate_x_axis$name,
                     limits = covar_treatrate_x_axis$limits,
                     labels = covar_treatrate_x_axis$labels,
                     expand = c(0, 0)) +
  scale_y_discrete(name = covar_y_axis$name,
                   breaks = covar_y_axis$breaks,
                   labels = covar_y_axis$labels) +
  facet_grid(covar ~ ., scale = "free", space = "free",
             labeller = covar_labeller) +
  theme(legend.background = element_blank(),
        legend.position = c(1, 1),
        legend.justification = c(1, 1))
write_plot(p_treatrate, "FigA1b.pdf")
# write_plot_as(p_treatrate,
              # "treatrate_by_covar",
              # width = 5,
              # height = 5,
              # save_rds = TRUE)

# PLOT: Two way hitrate (outcome) -----------------------------------------
hitrate2_df <- pol$data %>%
  filter(!!v_treatment) %>%
  compute_twoway_hitrates(twoway_g1, twoway_g2, v_outcome, v_riskcol) %>%
  filter(count > twoway_count_lb)

p_hitrate2 <- ggplot(hitrate2_df, aes(x = est_hitrate, y = true_hitrate)) +
  geom_point(aes_string(size = "count", color = twoway_g2), alpha = .6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_size_area(guide = FALSE) +
  scale_color_g2 +
  scale_x_hitrate2(
    name = paste("\nModel", twoway_hitrate_axis$name),
    labels = twoway_hitrate_axis$labels,
    limits = twoway_hitrate_axis$limits,
    breaks = twoway_hitrate_axis$breaks,
    expand = c(0, 0)) +
  scale_y_hitrate2(
    name = paste("\nEmpirical", twoway_hitrate_axis$name),
    labels = twoway_hitrate_axis$labels,
    limits = twoway_hitrate_axis$limits,
    breaks = twoway_hitrate_axis$breaks,
    expand = c(0, 0)) +
  theme(legend.background = element_blank(),
        legend.position = c(.95, 0),
        legend.justification = c(.95, 0))
write_plot(p_hitrate2, "FigA1c.pdf")
# write_plot_as(p_hitrate2,
              # "twoway_hitrate",
              # width = 5,
              # height = 5,
              # caption = sprintf("binned by %s and %s",
                                # twoway_g1, twoway_g2),
              # save_rds = TRUE)

# PLOT: Two way treatrate (treatment) -------------------------------------
treatrate2_df <- pol$data %>%
  compute_twoway_hitrates(twoway_g1, twoway_g2, v_treatment, v_treat) %>%
  filter(count > twoway_count_lb)

p_treatrate2 <- ggplot(treatrate2_df, aes(x = est_hitrate, y = true_hitrate)) +
  geom_point(aes_string(size = "count", color = twoway_g2), alpha = .6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_size_area(guide = FALSE) +
  scale_color_g2 +
  scale_x_treatrate2(
    name = paste("\nModel", twoway_treatrate_axis$name),
    labels = twoway_treatrate_axis$labels,
    limits = twoway_treatrate_axis$limits,
    breaks = twoway_treatrate_axis$breaks,
    expand = c(0, 0)) +
  scale_y_treatrate2(
    name = paste("\nEmpirical", twoway_treatrate_axis$name),
    labels = twoway_treatrate_axis$labels,
    limits = twoway_treatrate_axis$limits,
    breaks = twoway_treatrate_axis$breaks,
    expand = c(0, 0)) +
  theme(legend.background = element_blank(),
        legend.position = c(.95, 0),
        legend.justification = c(.95, 0))
write_plot(p_treatrate2, "FigA1d.pdf")
# write_plot_as(p_treatrate2,
              # "twoway_treatrate",
              # width = 5,
              # height = 5,
              # caption = sprintf("binned by %s and %s",
                                # twoway_g1, twoway_g2),
              # save_rds = TRUE)

