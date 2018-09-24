#!/usr/bin/env Rscript
source("pkgs.R")
source("consts.R")
source("helpers.R")
source("modeling.R")
source("optparse.R")
source("setup.R")

# Private functions -------------------------------------------------------
.log <- function(fmt, ...) {
  cat(sprintf(paste0(fmt, "\n"), ...))
}

.sep <- function(s, ...) {
  cat("\n")
  s <- str_pad(paste0(s, ""), options()$width, side = "right", pad = "-")
  cat(s)
  cat("\n")
}

.comma <- function(d, ...) {
  formatC(d, big.mark = ",")
}

# Set and unpack command line arguments -----------------------------------
OPT_PARSER <- OptionParser(option_list = OPT_LIST)
opt <- parse_args(OPT_PARSER)

source(sprintf("setup_%s.R", opt$target))

# Set file paths ----------------------------------------------------------
clean_df <- read_rds(TARGET_RDS)

pol <- opt %>%
  get_policy_path() %>%
  read_rds()

v_group <- rlang::sym(pol$grouping)
s_riskcol <- paste0(pol$risk_col, "__")
v_riskcol <- rlang::sym(s_riskcol)

base_case <- bind_rows(compute_bm(pol), compute_bm(pol, kitchen = TRUE))

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


# Log base rate summaries -------------------------------------------------
.log("Rows in clean %s dataset: %s", opt$target, .comma(nrow(clean_df)))

.sep("Base case benchmark")
pfrisk_df <- clean_df %>%
    group_by(suspect.race) %>%
    summarize(p_frisk = mean(frisked)) %>%
    spread(suspect.race, p_frisk)

walk(colnames(pfrisk_df), function(name) {
  p <- pfrisk_df[[name]]
  .log("Frisk rate for %s: %.2f%% with %.2f odds", name, p*100, p/(1-p))
})

# Log kitchen sink summaries ----------------------------------------------
.sep("Kitchen sink benchmark")
ks_odds <- base_case %>%
  filter(controls == "kitchen sink") %>%
  transmute(group = gsub("suspect.race", "", term),
            odds = exp(estimate)) %>%
  spread(group, odds)

walk(colnames(ks_odds), function(name) {
  .log("Odds of frisk v. whites for %s (kitchen sink): %.2f",
       name, ks_odds[[name]])
})

# Log first-step risk estimate summaries ----------------------------------
.sep("First-stage model results")

risk_df <- pol$data %>%
  filter(fold__ == "test") %>%
  rename(group = !!v_group, risk = !!v_riskcol)

mean_risk <- risk_df %>%
  group_by(group) %>%
  summarize(mean = mean(risk))

.log("Average risk for each race group:")
cat(paste(format(mean_risk$group),
          format(mean_risk$mean),
          sep = ": ",
          collapse = "\n"))
cat("\n")

test_auc <- risk_df %>%
  filter(frisked)
.log("First-stage model AUC on risk: %.0f%%",
     compute_auc(test_auc$risk, test_auc$found.weapon)*100)
.log("First-stage model AUC on treatment (frisk): %.0f%%",
     compute_auc(risk_df$ptrt__, risk_df$frisked)*100)
