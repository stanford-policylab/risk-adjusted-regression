# Generate parameters related to the dataset (e.g., column names, labels)
source("consts.R")

# Source (raw) data and target (clead) data RDS paths
# (DATA_DIR will point to /share/data)
SOURCE_RDS <- file.path("%RAW_DATA_PATH%")
TARGET_RDS <- file.path(CLEAN_DATA_DIR, "%DATASET%.rds")
