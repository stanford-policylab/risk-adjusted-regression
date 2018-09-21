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

# Read policy object ------------------------------------------------------
pol <- opt %>%
  get_policy_path() %>%
  read_rds()

v_group <- rlang::sym(pol$grouping)
s_riskcol <- paste0(pol$risk_col, "__")
v_riskcol <- rlang::sym(s_riskcol)

# Pre-process dataset from policy object ----------------------------------
risk_pd <- pol$data %>%
  filter(fold__ == "test") %>%
  rename(group = !!v_group, risk = !!v_riskcol)

plot_limits <- c(0.002, quantile(risk_pd$risk, probs = c(0.995)))

# PLOT: Model evaluation -------------------------------------------------
risk_plot <- ggplot(risk_pd) +
  geom_density(aes(x = risk, color = group)) +
  scale_x_log10("Estimated risk",
                labels = scales::percent_format(),
                limits = plot_limits,
                breaks = c(0.003, 0.01,0.03,0.1,0.3)) +
  geom_vline(data = function(df) df %>%
               group_by(group) %>%
               summarize(mn = mean(risk)),
             mapping = aes(xintercept = mn, color = group), linetype = 2) +
  scale_color_discrete(element_blank()) +
  scale_y_continuous("Density\n") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

write_plot(risk_plot, "Fig1.pdf", width = 5, height = 3)
