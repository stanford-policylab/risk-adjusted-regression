# Plot related parameters

# X-axis labels for when x is protected group
x_group_title <- "\nRace"
x_group_limits <- c("suspect.raceBlack", "suspect.raceHispanic")
x_group_labels <- c("Black", "Hispanic")

y_title <- "Odds of frisk vs. whites\n"
y_odds_limits <- c(NA, 2.15)
y_full_odds_limits <- c(NA, 2.5)

# Facet labels for when facetting by control types
control_labels <- c(
    suspect.race = "Race",
    `suspect.race, risk__` = "Risk",
    `suspect.race, s(risk__)` = "Risk",
    `suspect.race, riskbin__` = "Risk",
    `suspect.race, risk__, location.housing` = "Risk and location",
    `suspect.race, s(risk__), location.housing` = "Risk and location",
    `suspect.race, riskbin__, location.housing` = "Risk and location",
    `kitchen sink` = "All pre-treatment covariates"
)
controls_labeller <- labeller(controls = control_labels)

stage2_recode_string <-
  '"suspect.race"="Raw disparities"; "kitchen sink"="Kitchen sink"'

# Model check plot parameters ---------------------------------------------
# Single covariate checks
covar_groups <- c("suspect.age.bin", "suspect.sex", "suspect.race")

f_covar_pre <- function(d) {
  d %>%
    mutate(suspect.age.bin = cut(suspect.age, c(0, 17, 25, 32, 40, Inf)),
           suspect.age.bin = fct_rev(as.factor(suspect.age.bin)))
}

covar_y_axis <- list(
  name = ggplot2::element_blank(),
  breaks = c("(0,17]", "(17,25]", "(25,32]", "(32,40]", "(40,Inf]",
             "female","male",
             "Black", "Hispanic", "White"),
  labels = c("Under 18", "18 to 25", "26 to 32", "33 to 40", "Over 40",
             "Female", "Male",
             "Black", "Hispanic", "White")
  )

covar_labeller <- labeller(covar = c(
    suspect.race = "Race",
    suspect.sex = "Gender",
    suspect.age.bin = "Age"
))

covar_hitrate_x_axis <- list(
  name = "\nHit rate",
  limits = c(0.01, 0.04),
  labels = scales::percent
  )

covar_treatrate_x_axis <- list(
  name = "\nFrisk rate",
  limits = c(0.25, 0.65),
  labels = scales::percent
  )

# Two-way checks
twoway_g1 <- "precinct"
twoway_g2 <- "location.housing"

# Minimum number of ovservations required for a point in twoway plot
twoway_count_lb <- 100

twoway_hitrate_axis <- list(
  name = "hit rate",
  breaks = c(0.003, .01, .03, .1),
  limits = c(0.002, .2),
  labels = scales::percent
  )

twoway_treatrate_axis <- list(
  name = "frisk rate",
  breaks = seq(0, 1, .25),
  limits = c(0, 1),
  labels = scales::percent
  )

scale_color_g2 <- scale_color_hue(
  name = "Location",
  breaks = c("transit", "housing", "neither"),
  limits = c("transit", "housing", "neither"),
  labels = c("transit", "housing", "other")
  )

# Calibration plot settings
scale_x_calib <- scale_x_log10
scale_y_calib <- scale_y_log10

calib_y_label <- "weapon recovery rate"
calib_axis_limits <- c(.002, .5)

scale_x_risk <- scale_x_log10
scale_y_risk <- scale_y_continuous

scale_x_hitrate2 <- scale_x_log10
scale_y_hitrate2 <- scale_y_log10

scale_x_treatrate2 <- scale_x_continuous
scale_y_treatrate2 <- scale_y_continuous
