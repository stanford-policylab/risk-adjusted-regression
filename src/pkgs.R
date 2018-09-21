# Load packages -----------------------------------------------------------
PKGS <- c(
  "undi",
  "assertthat",
  "forcats",
  "lubridate",
  "lazyeval",
  "tidyverse",
  "furrr"
)

suppressMessages(
  PKGS <- sapply(PKGS, library, character.only = TRUE, logical.return = TRUE)
)

if (any(PKGS == FALSE)) {
  print(PKGS)
  stop("Failed loading packages.")
}

message("Loaded packages:", paste(names(PKGS), collapse = ", "))

