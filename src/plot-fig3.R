#!/usr/bin/env Rscript
source("pkgs.R")
source("plot.R")
source("consts.R")
source("helpers.R")
source("optparse.R")
source("setup.R")

# Set and unpack command line arguments -----------------------------------
OPT_PARSER <- OptionParser(option_list = OPT_LIST)
OPT_PARSER <- add_option(OPT_PARSER, c("-l", "--link"),
                         default = "ll",
                         help = "Second stage model type.")
opt <- parse_args(OPT_PARSER)

source(sprintf("params_%s.R", opt$target))
source(sprintf("plot_params_%s.R", opt$target))

# Set file paths ----------------------------------------------------------
pol <- opt %>%
  get_policy_path() %>%
  read_rds()

bs_std_err <- get_policy_path(opt, type = sprintf("bs_results_%s", opt$link)) %>%
  read_rds() %>%
  filter(controls == "Risk") %>%
  group_by(term, spec) %>%
  summarize(bs_se = sd(estimate)) %>%
  select(term, bs_se)

cens_gs <- opt %>%
  get_policy_path("cens_output") %>%
  read_rds()

synth_pol <- opt %>%
  get_policy_path("synth") %>%
  read_rds()

# PLOT: Grounding sensitivity ---------------------------------------------
synth_rad <- compute_rad(synth_pol,
                         base_group = base_group,
                         minority_groups = minority_groups,
                         fit_fn = "logit_coef")

rad_ctls <- unique(cens_gs$results$controls)
Theta <- scan(paste0("Theta_", opt$target, ".value"))

grounding_df <- cens_gs$grid %>%
  mutate(term = gsub(pol$grouping, "", term)) %>%
  left_join(bs_std_err, by = c("term")) %>%
  filter(sapply(pars, function(x) all(x[1, 3:8] < log(Theta+0.1)))) %>%
  group_by(term) %>%
  mutate(odds_ratio = exp(estimate),
         ciub = exp(estimate + 2 * bs_se),
         cilb = exp(estimate - 2 * bs_se)) %>%
  summarize(ub = max(odds_ratio),
            lb = min(odds_ratio),
            ciub = max(ciub),
            cilb = min(cilb)) %>%
  left_join(cens_gs$base_case %>%
    filter(controls %in% rad_ctls) %>%
    mutate(odds_ratio = exp(estimate)) %>%
    mutate(term = gsub(pol$grouping, "", term)),
    by = "term") %>%
  left_join(synth_rad %>%
            transmute(term = gsub(pol$grouping, "", term),
                      true = exp(estimate)),
            by = "term")

grounding_plot <- ggplot(grounding_df, aes(x = term, y = odds_ratio)) +
  geom_hline(yintercept = 1, size = 2,
             color = "grey92") +
  geom_errorbar(aes(ymin = cilb, ymax = ciub), size = .5, width = .2,
                alpha = .4) +
  geom_linerange(aes(ymin = lb, ymax = ub), size = 4) +
  geom_point(color = "black", fill = "white", size = 3, shape = 21) +
  geom_point(aes(y = true),
             color = "black", fill = "red", size = 3, shape = 21) +
  scale_y_continuous(y_title,
                     limits = c(1, ceiling(10*max(grounding_df$ciub))/10)) +
  scale_x_discrete(element_blank()) +
  theme_bw(13)

write_plot(grounding_plot, "Fig3.pdf", width = 3, height = 3)
