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

# Set file paths ----------------------------------------------------------
pol <- opt %>%
  get_policy_path() %>%
  read_rds()

# Pre-process data --------------------------------------------------------
s_riskcol <- paste0(pol$risk_col, "__")

d <- pol$data

f <- make_formula("ptrt__",
                  paste0(pol$grouping, "*", "logit(", s_riskcol, ")"))
model <- glm(f, "binomial", d %>% sample_frac(0.1))

pd <- d %>%
  sample_frac(0.05) %>%
  mutate(pred = predict(model, ., type = "response")) %>%
  rename(risk = !!s_riskcol,
         group = pol$grouping)

limits <- quantile(pd$risk, c(0.01,0.99))

# PLOT: Discussion figure -------------------------------------------------
discussion_plot <- ggplot(pd) +
  geom_line(aes(x = risk, y = pred, color = group)) +
  scale_x_log10("Estimated risk",
                labels = scales::percent,
                breaks = c(0.003, 0.01, 0.03, 0.1, 0.3),
                limits = limits) +
  scale_y_continuous("Probability of frisk\n",
                     labels = scales::percent) +
  scale_color_discrete(element_blank()) +
  theme_bw(13)

write_plot(discussion_plot, "Fig5.pdf", width = 5, height = 3)
