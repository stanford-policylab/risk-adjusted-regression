# Risk adjusted regression

Analysis of disparate impact using risk adjusted regressions (See [Jung et al., 2018](https://arxiv.org/abs/1809.05651))

## Quick-start guide

Use `make` to download the data and reproduce _all_ results reported in the [paper](https://arxiv.org/abs/1809.05651).

```sh
$ make
# Will produce files
#  |-- fig
#  |   |-- Fig1.pdf
#  |   |-- Fig2.pdf
#  |   |-- Fig3.pdf
#  |   |-- Fig4.pdf
#  |   |-- Fig5.pdf
#  |   |-- FigA1a.pdf
#  |   |-- FigA1b.pdf
#  |   |-- FigA1c.pdf
#  |   `-- FigA1d.pdf
#  `-- summary.log
```

The filename for `summary.log` or each figure can be provided to selectively generate results.
For example,

```sh
make summary.log
```

to generate the plain-text log file, or

```sh
make Fig1.pdf
```

to reproduce Figure 1,

To remove all generated output and rebuild from scratch, run `make clean` first.
See [`Makefile`](./Makefile) for all available build targets.

## Configure

See [`Makefile`](./Makefile) for all available configurations.
Most common options would be:
```sh
RISKMODEL  # The model used to estimate risk/treatment (e.g., l1, gbm, rf)

# Limits---lowerbound (`LB`) and upperbounds (`UB`)---of exp(delta) for sensitivity
# e.g., exp(delta) = 3 means u can triple the odds
EXP_DELTA_LB
EXP_DELTA_UB

B  # Number of bootstrap samples to use for computing CIs
```


## Adding new datasets

Running `make new` will start a prompt for adding a new datasets.
This will create four new files under `src/`, which will likely need some
manual review to fit the context/dataset.

```sh
# Files created under src/ for data set named *
00-clean-*.R     # Script to clean raw data
params_*.R       # Parameters describing what features to include, etc.
plot_params_*.R  # Plot (axis/facet) labels, custom limits, etc.
setup_*.R        # Script to load the clean data (no review necessary)
```

Once the cleaning script and parameters are setup, run
```sh
make TARGET=your_new_dataset_name
```
to execute standard analysis.

