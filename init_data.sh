#!/usr/bin/env bash

# Created: 2018-05-16
# Author: jongbin
# Description: Initialze a dataset

SETUP_SOURCE=src/templates/setup_.R
PARAMS_SOURCE=src/templates/params_.R
PLOT_PARAMS_SOURCE=src/templates/plot_params_.R
CLEAN_SOURCE=src/templates/00-clean-.R

read -p "Set dataset name:" DI_DATASET

# Generate setup_*.R ------------------------------------------------------
SETUP_FILE=src/setup_"$DI_DATASET".R

function generate_setup {
  while true; do
    read -e -p "Absolute path to (raw) data rds file:" DI_RAW_SRC
    if [[ "$DI_RAW_SRC" == *.rds ]]; then
      break
    else
      echo "Source data must be *.rds" 1>&2
    fi
  done
  cp "$SETUP_SOURCE" $SETUP_FILE
  sed -i "s|%RAW_DATA_PATH%|$DI_RAW_SRC|" "$SETUP_FILE"
  sed -i "s|%DATASET%|$DI_DATASET|" "$SETUP_FILE"
}

if [[ -f "$SETUP_FILE" ]]; then
  echo "$SETUP_FILE exists. Overwrite?"
  select yn in "Yes" "No"
  do
    case $yn in
      Yes)
      echo "Overwriting $SETUP_FILE"
      generate_setup
        break ;;
      No)
      echo "Use existing $SETUP_FILE"
        break ;;
    esac
  done
else
  echo "Creating $SETUP_FILE"
  generate_setup
fi

# Generate params_*.R -----------------------------------------------------
PARAMS_FILE=src/params_"$DI_DATASET".R

function generate_params {
  read -p "Name of column indicating group membership (e.g., Race):" DI_GROUP
  read -p "Name of treatment indicator column:" DI_TREATMENT
  read -p "Name of outcome indicator column:" DI_OUTCOME
  echo "Which of the response (outcome) estimates indicate risk?"
  select rc in "resp_ctl" "resp_trt"
  do
    case $rc in
      resp_ctl)
        DI_RISK_COL=resp_ctl
        break ;;
      resp_trt)
        DI_RISK_COL=resp_trt
        break ;;
    esac
  done
  cp "$PARAMS_SOURCE" "$PARAMS_FILE"
  sed -i "s|%GROUP%|$DI_GROUP|" "$PARAMS_FILE"
  sed -i "s|%TREATMENT%|$DI_TREATMENT|" "$PARAMS_FILE"
  sed -i "s|%OUTCOME%|$DI_OUTCOME|" "$PARAMS_FILE"
  sed -i "s|%RISK_COL%|$DI_RISK_COL|" "$PARAMS_FILE"
}

if [[ -f "$PARAMS_FILE" ]]; then
  echo "$PARAMS_FILE exists. Overwrite?"
  select yn in "Yes" "No"
  do
    case $yn in
      Yes)
      echo "Overwriting $PARAMS_FILE"
      generate_params
        break ;;
      No)
      echo "Use existing $PARAMS_FILE"
        break ;;
    esac
  done
else
  echo "Creating $PARAMS_FILE"
  generate_params
fi

# Generate 00-clean-*.R ---------------------------------------------------
CLEAN_FILE=src/00-clean-"$DI_DATASET".R

function generate_clean {
  cp "$CLEAN_SOURCE" $CLEAN_FILE
  sed -i "s|%DATASET%|$DI_DATASET|" "$CLEAN_FILE"
}

if [[ -f "$CLEAN_FILE" ]]; then
  echo "$CLEAN_FILE exists. Overwrite?"
  select yn in "Yes" "No"
  do
    case $yn in
      Yes)
      echo "Overwriting $CLEAN_FILE"
      generate_clean
        break ;;
      No)
      echo "Use existing $CLEAN_FILE"
        break ;;
    esac
  done
else
  echo "Creating $CLEAN_FILE"
  generate_clean
fi

# Generate plot_params_*.R ------------------------------------------------
PLOT_PARAMS_FILE=src/plot_params_"$DI_DATASET".R

function generate_plot_params {
  cp "$PLOT_PARAMS_SOURCE" $PLOT_PARAMS_FILE
}

if [[ -f "$PLOT_PARAMS_FILE" ]]; then
  echo "$PLOT_PARAMS_FILE exists. Overwrite?"
  select yn in "Yes" "No"
  do
    case $yn in
      Yes)
      echo "Overwriting $PLOT_PARAMS_FILE"
      generate_plot_params
        break ;;
      No)
      echo "Use existing $PLOT_PARAMS_FILE"
        break ;;
    esac
  done
else
  echo "Creating $PLOT_PARAMS_FILE"
  generate_plot_params
fi

