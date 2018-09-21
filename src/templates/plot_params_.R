# Plot related parameters

# X-axis labels for when x is protected group
x_group_title <- "\nRace"
x_group_limits <- c("Raceblack", "Racehispanic")
x_group_labels <- c("black", "hispanic")

# Facet labels for when facetting by control types
controls_labeller <- labeller(controls = c(
    Race = "Race",
    `Race, risk__` = "Race + Risk",
    `kitchen sink` = "Kitchen sink (no risk)"
))
