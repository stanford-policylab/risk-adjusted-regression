# Load modeling-specific packages -----------------------------------------
MODELING_PKGS <- c(
  "randomForest",
  "glmnet",
  "gbm",
  "rflann",  # Neighbors
  "ROCR"
)

suppressMessages(
  MODELING_PKGS <- sapply(MODELING_PKGS, library, character.only = TRUE,
                          logical.return = TRUE)
)

if (any(MODELING_PKGS == FALSE)) {
  print(MODELING_PKGS)
  stop("Failed to load packages.")
}

message("Loaded modeling packages:",
        paste(names(MODELING_PKGS), collapse = ", "))

# Hyper-params and data prep. ---------------------------------------------
# nlambda for glmnet models
nlambda <- 1000

# number of trees for random forest
ntrees <- 10000

# Define fit and prediction functions for policy
models_setup <- list(
  l1 = list(model_type = "l1_",
            f_fit = function(f, d, ...)
              fit_glmnet(d, f, nlambda = nlambda, ...),
            f_pred = function(m, d, f) pred_glmnet(d, f, m)),
  l2 = list(model_type = "l2_",
            f_fit = function(f, d, ...)
              fit_glmnet(d, f, alpha = 0, ...),
            f_pred = function(m, d, f) pred_glmnet(d, f, m)),
  rf = list(model_type = "rf_",
            f_fit = function(f, d, ...)
              fit_rf(d, f, trees = ntrees, cores = 10),
            f_pred = function(m, d, f) pred_rf(d, f, m)),
  glm = list(model_type = "glm_",
             f_fit = function(f, d, ...) fit_glm(d, f),
             f_pred = function(m, d, f) pred_glm(d, m)),
  gbm = list(model_type = "gbm_",
             f_fit = function(f, d, ...) {
                 args = overwrite_list(
                   list(
                     n.trees = ntrees,
                     cv.folds = 0,
                     n.cores = NULL,
                     distribution = "bernoulli",
                     interaction.depth = 2,
                     train.fraction = 0.8,
                     keep.data = FALSE,
                     shrinkage = 0.05
                   ), ...)
                 do.call(fit_gbm, c(list(d = d, f = f), args))
               },
             f_pred = function(m, d, f) pred_gbm(d, m, method = "test"))
  )

# Matching functions -----------------------------------------------------
match_flann <- function(df, treat_col, dist_col, factors = list(),
                        max_dist = 0.05, cores = 4, ...) {
  # Find matching rows for treatment v. control (via Sam)

  # Args:
  #   df: data frame to find matches from
  #   treat_col: name of treatment indicator column
  #   dist_col: name of column to use as distance measure
  #   factors: list of columns to group data into cells
  #   max_dist: maximum distance to consider as neighbors
  #   cores: number of cores to use in rflann::Neighbour
  #   ...: additional arguments passed to rflann::Neighbour
  out = list()

  cells = df %>% group_by_(.dots = factors) %>%
    summarize(x__ = NA) %>%
    as.data.frame() %>%
    select(-x__) %>%
    filter(complete.cases(.))

  for (i in seq_len(nrow(cells))) {
    # print(cells[i,])
    dots = lapply(
      seq_len(ncol(cells)),
      function(j) interp(
        ~y==x,
        .values = list(
          y = as.name(names(cells)[j]),
          x = cells[i,j])))

    df_cell = df %>% filter_(.dots = dots)

    match = Neighbour(
      df_cell[df_cell[[treat_col]], dist_col],
      df_cell[!df_cell[[treat_col]], dist_col],
      k = 1, cores = cores, ...)

    match_found = sqrt(match$distances) < max_dist

    out[[length(out)+1]] =
      bind_rows(
        df_cell[df_cell[[treat_col]],][match_found,],
        df_cell[!df_cell[[treat_col]],][match$indices[match_found],]
      )
  }
  bind_rows(out)
}
# Modeling functions ------------------------------------------------------
fit_glm <- function(d, f, family = binomial, ...) {
  # Fits a vanilla glm; kept as a function for consistency

  # Args:
  #   d: data frame used for training
  #   f: model formula
  #   family: description of error distribution and link function, passed to
  #       glm(); (default: binomial)
  #   ...: arguments passed to glm()

  # Reutrns:
  #   fitted gbm object
  glm(f, data = d, family = family, ...)
}

fit_glmnet <- function(d, f, family = "binomial", alpha = 1, ...) {
  # Fits a glmnet model, taking care of model matrix and other junk

  # Args:
  #   d: data frame used for training
  #   f: model formula
  #   family: model type, passed as the family argument to cv.glmnet
  #   alpha: 1 for Lasso, 0 for Ridge; passed to cv.glmnet
  #   ...: arguments passed to cv.glmnet

  # Reutrns:
  #   cv.glmnet object

  # Extract model matrix given data and formula
  mm <- .get_mm(d, f, discrete_y = ifelse(family == "binomial", TRUE, FALSE))
  X <- mm$X
  y <- mm$y

  cv.glmnet(X, y, family = family, alpha = alpha, parallel = TRUE, ...)
}

fit_rf <- function(d, f, trees, cores = 4, discrete_y = TRUE, ...) {
  # Fits a randomForest model in parallel, taking care of model matrix and
  # other junk

  # Args:
  #   d: data frame used for training
  #   f: model formula
  #   trees: number of trees to use
  #   cores: number of cores to use for parallel (default: 4)
  #   discrete_y: boolean indicating whether target y is discrete or continuous
  #   ...: arguments passed to randomForest (within foreach loop)

  # Reutrns:
  #   fitted randomForest object

  # Extract model matrix given data and formula
  mm <- .get_mm(d, f, discrete_y = discrete_y)
  X <- mm$X
  y <- mm$y

  # Set randomForest hyperparams
  ntree <- trees / cores
  if (!(as.integer(ntree) == ntree)) {
    ntree <- ceiling(ntree)
    warning(sprintf(paste("Cannot divide %d trees equally among %d cores;",
                           "training %d trees instead"),
                    trees, cores, ntree * cores))
  }

  rf <- foreach(i = 666:(666 + cores - 1),
                .combine = randomForest::combine,
                .multicombine = TRUE,
                .packages = "randomForest") %dopar%
    {
      set.seed(i)
      randomForest(X, y, ntree = ntree)
    }
}

fit_gbm <- function(d, f, n.trees, cv.folds = 10, ...) {
  # Fits a gbm; kept as a function for consistency

  # Args:
  #   d: data frame used for training
  #   f: model formula
  #   n.trees: max number of trees to use
  #   cv.folds: number of cv folds to use (default: 10); also sets the number of
  #   discrete_y: boolean indicating whether target y is discrete or continuous
  #       parallel cores to use
  #   ...: arguments passed to gbm

  # Reutrns:
  #   fitted gbm object
  gbm(f, data = d, n.trees = n.trees, cv.folds = cv.folds, ...)
}

fit_iso <- function(d, outcome_col, x_col, factors) {
  list(
    models = d %>%
      group_by_(.dots = factors) %>%
      do(m__ =  isoreg(.[[x_col]], .[[outcome_col]])),
    x_col = x_col
  )
}

pred_iso <- function(d, m) {
  factors <- head(names(m$models),-1)
  x_col <- as.name(m$x_col)
  d %>% left_join(m$models, by = factors) %>%
    group_by_(.dots = factors) %>%
    mutate(pred = as.stepfun(m__[[1]])(!!x_col)) %>%
    with(pred)
}

pred_glm <- function(d, m, as = NULL, type = "response") {
  # Append data with predictions from glm

  # Args:
  #   d: data frame to predict for
  #   m: prediction model of class "glm"
  #   as: character name of prediction column to add
  #   type: prediction return type (default: response)

  # Reutrns:
  #   data frame with signle column of name `as` containing predictions
  if (!("glm" %in% class(m))) {
    stop("Expected glm model, got ", class(m))
  }

  if (is.null(as)) {
    predict(m, newdata = d, type = type)
  } else {
    d %>%
      transmute(!!as := predict(m, newdata = d, type = type))
  }
}

pred_glmnet <- function(d, f, m, as = NULL) {
  # Append data with predictions from a cv.glmnet model

  # Args:
  #   d: data frame to predict for
  #   f: model formula
  #   m: prediction model of class "cv.glmnet"
  #   as: character name of prediction column to add

  # Reutrns:
  #   data frame of single column of name `as` containing predictions
  if (!("cv.glmnet" %in% class(m))) {
    stop("Expected cv.glmnet model, got ", class(m))
  }

  .pred_with_mm(d, f, m, use_column = 1, as = as, type = "response",
                s = "lambda.min")
}

pred_rf <- function(d, f, m, as = NULL) {
  # Append data with predictions from a randomForest model

  # Args:
  #   d: data frame to predict for
  #   f: model formula
  #   m: prediction model of class "randomForest"
  #   as: character name of prediction column to add

  # Reutrns:
  #   data frame of single column of name `as` containing predictions
  if (!("randomForest" %in% class(m))) {
    stop("Expected randomForest model, got ", class(m))
  }

  .pred_with_mm(d, f, m, use_column = 2, as = as, type = "prob")
}

pred_gbm <- function(d, m, as = NULL, method = "cv") {
  # Append data with predictions from gbm

  # Args:
  #   d: data frame to predict for
  #   m: prediction model of class "gbm"
  #   as: character name of prediction column to add
  #   method: method for selecting "best" gbm (default: "cv"), see ?gbm

  # Reutrns:
  #   data frame with signle column of name `as` containing predictions
  if (!("gbm" %in% class(m))) {
    stop("Expected gbm model, got ", class(m))
  }

  best_iter <- gbm.perf(m, method = method, plot.it = FALSE)

  if (is.null(as)) {
    predict(m, d, best_iter, type = "response")
  } else {
    d %>%
      transmute(!!as := predict(m, d, best_iter, type = "response"))
  }
}

compute_auc <- function(pred, label) {
  p <- prediction(pred, label)
  auc <- performance(p, "auc")
  unlist(slot(auc, "y.values"))
}

# Private helper functions ------------------------------------------------
.get_mm <- function (d, f, discrete_y = TRUE) {
  # Given a data.frame and formula, return the model matrix X and y as a list

  # Args:
  #   d: data frame used for training
  #   f: model formula
  #   discrete_y: boolean indicating whether y is discrete or continuous

  # Reutrns:
  #   List with elements X (the model matrix) and y (labels, extracted from the
  #   LHS of formula f)
  X <- model.matrix(f, d)[, -1]

  # Extract target column from LHS of f
  y <- d[[as.character(f_lhs(f))]]

  # Heuristic to determine whether y should be discrete or continuous
  if (discrete_y) {
    y <- as.factor(y)
  }

  list(X = X, y = y)
}

.pred_with_mm <- function(d, f, m, use_column, as = NULL, ...) {
  # Generate predictions and appends to the data frame as a new column, for a
  # model by first converting the data to a model matrix based on the formula
  # provided

  # Args:
  #   d: data frame to predict for
  #   f: model formula
  #   m: prediction model
  #   use_column: the column from prediction results to keep in the data frame
  #       e.g., for 1 for glmnet (single column), and 2 for randomForest
  #       (assuming the positive class prediction is of interest)
  #   as: character name of prediction column to add
  #   ...: other arguments passed to the general predict function

  # Reutrns:
  #   data frame with single column of name `as` containing predictions

  mm <- .get_mm(d, f)  # Don't worry about y --- not used

  if (is.null(as)) {
    predict(m, mm$X, ...)[, use_column]
  } else {
    d %>%
      transmute(!!as := predict(m, mm$X, ...)[, use_column])
  }
}

