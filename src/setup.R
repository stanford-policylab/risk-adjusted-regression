# !diagnostics off
# System options and setup

# Changing these options should *only* affect performance/runtime and not change
# any results/hyperparameters etc.

# System/environment settings ---------------------------------------------
# Setup multicores: use half of what's available, or at most 10
N_CORES <- min(30, round(parallel::detectCores() / 2))

message("Registering up to ", N_CORES, " core(s)")
options(future.globals.maxSize = Inf)
options(mc.cores = N_CORES)
options(mc.preschedule = TRUE)
future::plan(future::multiprocess, workers = N_CORES)
