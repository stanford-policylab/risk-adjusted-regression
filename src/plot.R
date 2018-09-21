# Helper functions for plotting with ggplot
library("ggbuildr")

theme_set(theme_bw())

# Dependency/sanity checks ------------------------------------------------

# Global constants for plotting -------------------------------------------
PLOTS_DIR <- normalizePath("../fig/")

# Helper functions --------------------------------------------------------
capitalize <- function(string) {
  # Capitalize string. Mainly for capitalizing facet labels
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string
}

validate_path <- function(path) {
  # Check if the directory for path exists, and if not create

  # Args:
  #   path: full path (including filename)

  # Returns:
  #   file path to directory, sans filename (returned from dirname)
  assert_that(tools::file_ext(path) != "",
              msg = "validate_path expects full path (including file name)")
  dir <- dirname(path)

  if (!dir.exists(dir)) {
    message("Creating:", dir)
    dir.create(dir, recursive = TRUE)
  }

  return(dir)
}

write_plot <- function(p, file_name, save_rds = FALSE,
                       width = 5, height = 5, caption = NULL, ...) {
  # Save plot p to the PLOTS_DIR with file_name and write the p object as an
  # rds file in an rds/ subdirectory

  # Args:
  #   p: ggplot object to write
  #   file_name: target filename, including sub-directories and extension
  #   save_rds; if TRUE, save the plot object as an rds file
  #   width/height: default 5; passed to ggsave
  #   caption: Optional caption to add to plot (but not to rds file)
  #   ...: extra arguments passed to ggsave
  name <- basename(tools::file_path_sans_ext(file_name))

  plot_path <- file.path(PLOTS_DIR, file_name)
  plot_dir <- validate_path(plot_path)

  if (save_rds) {
    rds_path <- file.path(plot_dir, "rds", paste0(name, ".rds"))
    rds_dir <- validate_path(rds_path)
    write_rds(p, rds_path)
  }

  if (!is.null(caption)) {
    p <- p + labs(caption = caption)
  }

  message("Saving:", plot_path)
  ggsave(plot_path,
         width = width,
         height = height,
         plot = p,
         ...)
}

histogram <- function(df, ...) {
  qplot(df, geom="histogram", ...)
}

sample4plots <- function(df, p, seed = sample(200:400, 1)) {
  # Down sample rows of df for plotting. Preserves first and last observation.

  # Args:
  #   df: data to down sample
  #   p: proportion to sample
  #   seed: (Optional) random seed

  # Returns: Down-sampled data frame with first/last observation preserved
  set.seed(seed)
  bind_rows(
    df %>% slice(1),
    df %>% slice(2:(n() - 1)) %>% sample_frac(p),
    df %>% slice(n()),
    )
}
