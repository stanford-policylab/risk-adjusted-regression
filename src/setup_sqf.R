# Generate parameters related to the dataset (e.g., column names, labels)
source("consts.R")

# Source (raw) data and target (clead) data RDS paths
# (DATA_DIR will point to /share/data)
SOURCE_RDA <- file.path(DATA_DIR, "sqf/sqf.RData")
TARGET_RDS <- file.path(CLEAN_DATA_DIR, "sqf.rds")
