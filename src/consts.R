# Central definition of global constants (e.g., file paths, fold numbers)
if (!exists("CONSTS_R__")) {

  # Base directory paths
  DATA_DIR <- normalizePath("../data/")
  CLEAN_DATA_DIR <- file.path(DATA_DIR, "clean")

  TRAIN_FOLD <- 1
  TEST_FOLD <- 2

  check_path <- function (path) {
    dirpath <- dirname(path)
    # Checks whether the directory of path exists, and if not, creates it
    if (!dir.exists(dirpath)) {
      message(sprintf("Creating path\n\t%s", dirpath))
      dir.create(dirpath, recursive = TRUE)
    }
  }

  CONSTS_R__ <- TRUE
} else {
  message("consts.R already loaded")
}
