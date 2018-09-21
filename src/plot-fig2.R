#!/usr/bin/env Rscript
source("pkgs.R")
source("plot.R")
source("consts.R")
source("helpers.R")
source("optparse.R")
source("setup.R")

# Set and unpack command line arguments -----------------------------------
OPT_PARSER <- OptionParser(option_list = OPT_LIST)
opt <- parse_args(OPT_PARSER)

source(sprintf("plot_params_%s.R", opt$target))

# Set file paths ----------------------------------------------------------
pol <- opt %>%
  get_policy_path() %>%
  read_rds()

second_stage <- map_dfr(c("ll", "db", "tps"), function(type) {
  est_path <- get_policy_path(opt, type = sprintf("results_%s", type))
  est_df <- read_rds(est_path)

  bs_path <- get_policy_path(opt, type = sprintf("bs_results_%s", type))
  bs_df <- read_rds(bs_path) %>%
    group_by(term, controls, spec) %>%
    summarize(bs_se = sd(estimate))

  est_df %>%
    left_join(bs_df, by = c("term", "controls", "spec")) %>%
    rename(std.error = bs_se)
})

base_case <- bind_rows(compute_bm(pol), compute_bm(pol, kitchen = TRUE))

# PLOT: Second stage plot -------------------------------------------------
second_stage_pd <- second_stage %>%
  bind_rows(base_case) %>%
  # bind_rows(filter(gs$base_case, !grepl("risk", controls))) %>%
  mutate(controls = car::recode(controls, stage2_recode_string),
         controls = factor(controls, unique(controls)[c(3,4,1,2)]),
         spec = factor(spec, unique(spec)),
         term = factor(gsub("suspect.race", "", term)))

second_stage_plot <- ggplot(data = second_stage_pd) +
  geom_hline(yintercept = 1, size = 2, color = "grey92") +
  # RAR methods
  geom_point(data = function(x) filter(x, !is.na(spec)),
             mapping = aes(x = term, y = exp(estimate), color = spec),
             position = position_dodge(width=0.5)) +
  geom_errorbar(data = function(x) filter(x, !is.na(spec)),
                mapping = aes(x = term,
                              ymax = exp(estimate + 2*std.error),
                              ymin = exp(estimate - 2*std.error),
                              color = spec),
                 position = position_dodge(width=0.5), width = 0.4) +
  # Benchmark methods
  geom_point(data = function(x) filter(x, is.na(spec)),
             mapping = aes(x = term, y = exp(estimate))) +
  geom_errorbar(data = function(x) filter(x, is.na(spec)),
                mapping = aes(x = term,
                              ymax = exp(estimate + 2*std.error),
                              ymin = exp(estimate - 2*std.error),
                              width = 0.4/3)) +
  scale_y_continuous(
    "Odds of frisk vs whites\n",
    breaks = function(lim) seq(1, lim[2], ifelse(lim[2] > 2.5, 0.5, 0.2))
  ) +
  facet_grid(~controls) +
  scale_x_discrete("") +
  scale_color_discrete(expression(paste("Specification of ", italic("g"))),
                       breaks = unique(second_stage$spec)) +
  theme_bw(13) +
  theme(legend.position = "bottom", axis.title.x = element_blank())

write_plot(second_stage_plot, "Fig2.pdf", width = 7, height = 4)
