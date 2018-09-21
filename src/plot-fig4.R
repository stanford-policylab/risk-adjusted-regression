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

source(sprintf("plot_params_%s.R", opt$target))

# Set file paths ----------------------------------------------------------
pol <- opt %>%
  get_policy_path() %>%
  read_rds()

bs_std_err <- get_policy_path(opt, type = sprintf("bs_results_%s", opt$link)) %>%
  read_rds() %>%
  group_by(term, controls, spec) %>%
  summarize(bs_se = sd(estimate)) %>%
  select(term, controls, bs_se)

gs_list <- opt %>%
  get_policy_path("full_output") %>%
  read_rds()

gs <- gs_list$no_control$gs
gs_loc <- gs_list[map_chr(gs_list, "name") == "location"][[1]]$gs

# PLOT: Main plot ---------------------------------------------------------
Theta <- scan(paste0("Theta_", opt$target, ".value"))

gs_loc_df <- gs_loc$grid %>%
  left_join(gs_loc$base_case %>%
            transmute(term, controls, value = exp(estimate)),
            by = c("term", "controls")) %>%
  mutate(controls = "Risk and location",
         term = gsub(pol$grouping, "", term)) %>%
  left_join(bs_std_err, by = c("term", "controls"))

gs_df <- gs$grid %>%
  left_join(gs$base_case %>%
            transmute(term, controls, value = exp(estimate)),
            by = c("term", "controls")) %>%
  mutate(controls = "Risk",
         term = gsub(pol$grouping, "", term)) %>%
  left_join(bs_std_err, by = c("term", "controls"))

main_df <- bind_rows(gs_df, gs_loc_df) %>%
  filter(sapply(pars, function(x) all(x[1, 3:8] < log(Theta+0.1)))) %>%
  mutate(odds_ratio = exp(estimate),
         ciub = exp(estimate + 2 * bs_se),
         cilb = exp(estimate - 2 * bs_se)) %>%
  group_by(term, controls) %>%
  summarize(pt = value[1],
            ub = max(odds_ratio),
            lb = min(odds_ratio),
            ciub = max(ciub),
            cilb = min(cilb))

main_plot <- ggplot(main_df %>%
                    mutate(controls = factor(controls,
                                             unique(controls)[c(1, 2)])),
                    aes(x = term, y = pt)) +
  geom_hline(yintercept = 1, size = 2, color = "grey92") +
  geom_errorbar(aes(ymin = cilb, ymax = ciub),
                size = .5, width = .2, alpha = .4) +
  geom_linerange(aes(ymin = lb, ymax = ub), size = 4) +
  geom_point(color = "black", fill = "white", size = 3, shape = 21) +
  scale_y_continuous(y_title,
                     limits = c(1, ceiling(10*max(main_df$ciub))/10)) +
  scale_x_discrete(element_blank()) +
  facet_grid(~ controls) +
  theme_bw(13)

write_plot(main_plot, "Fig4.pdf", width = 5, height = 3)
