# Baseline risk model (e.g., l1 or gbm)
RISKMODEL ?= gbm

# Target data set
TARGET ?= sqf

# Calibration scaling scheme to use (raw, platt, isotonic) --- allows partial
# matching
SCALING ?= raw

# Limits of exp(delta) for sensitivity
EXP_DELTA_LB ?= 1
EXP_DELTA_UB ?= 3

# Number of bootstrap samples
B ?= 100

# Link function for second stage; one of ll, db, tps
LINK ?= ll

# Base random seed to use
RSEED ?= 1745

# Method for sweeping sensitivity values
SEARCH_METHOD ?= grid

# Directories and paths
FIGS_DIR ?= fig
DATA_DIR ?= data


# File path/flag aliases -----------------------------------------------------
CLEAN_RDS=$(DATA_DIR)/clean/$(TARGET).rds

# Hyper-parameter filename patterns
OPTS_PATTERN=s$(RSEED)_$(SCALING)
SENS_PATTERN=edl$(EXP_DELTA_LB)_edu$(EXP_DELTA_UB)

RDS_SUFFIX=$(RISKMODEL)_s$(RSEED).rds

POLICY_RDS=$(DATA_DIR)/$(TARGET)/policy_$(RDS_SUFFIX)
SYNTH_RDS=$(DATA_DIR)/$(TARGET)/synth_$(RDS_SUFFIX)
CENSOR_RDS=$(DATA_DIR)/$(TARGET)/cens_$(RDS_SUFFIX)
BS_RDS=$(DATA_DIR)/$(TARGET)/bootstrap_$(RDS_SUFFIX)
CENS_GRID_RDS=$(DATA_DIR)/$(TARGET)/cens_output_$(RDS_SUFFIX)
FULL_GRID_RDS=$(DATA_DIR)/$(TARGET)/full_output_$(RDS_SUFFIX)

RFLAGS=-r $(RISKMODEL) -t $(TARGET) -c $(SCALING) -s $(RSEED)
BFLAGS=-b $(B)
SENSFLAGS=--edl $(EXP_DELTA_LB) --edu $(EXP_DELTA_UB)

# Setup prerequisite paths
vpath %.pdf $(FIGS_DIR)
vpath %.R src/

# Target files: figures (intermediate figures)
stage2results=$(DATA_DIR)/$(TARGET)/bs_results_$(LINK)_$(RDS_SUFFIX) \
	$(DATA_DIR)/$(TARGET)/results_$(LINK)_$(RDS_SUFFIX)

figures=$(addsuffix .pdf, \
	Fig1 \
	Fig2 \
	Fig3 \
	Fig4 \
	Fig5 \
	FigA1a \
	FigA1b \
	FigA1c \
	FigA1d \
	)

# High-level build targets ---------------------------------------------------
all: $(figures)
.PHONY: all

clean:
	rm -rfv \
		src/Theta_$(TARGET).value \
		$(FIGS_DIR)/ \
		$(DATA_DIR)/
# .PHONY: clean

# Add new dataset
new:
	bash ./init_data.sh
.PHONY: new

data: $(CLEAN_RDS)
.PHONY: data

synth: $(SYNTH_RDS)
.PHONY: synth

undi: $(CENSOR_RDS)
.PHONY: undi

bootstrap: $(BS_RDS)
.PHONY: bootstrap

sensitivity: $(CENS_GRID_RDS) $(FULL_GRID_RDS)
.PHONY: sensitivity

secondstage: $(stage2results)
.PHONY: secondstage

# Helpful shortcuts
print-%: ; @echo $* = $($*)

# Build target files ---------------------------------------------------------
# Base data files
$(DATA_DIR)/$(TARGET)/$(TARGET).RData: get_data.sh
	./get_data.sh

$(CLEAN_RDS): \
	$(DATA_DIR)/$(TARGET)/$(TARGET).RData \
	00-clean-$(TARGET).R \
	setup_$(TARGET).R \
	consts.R
	cd src && ./00-clean-$(TARGET).R

$(POLICY_RDS): \
	01-fit.R \
	params_$(TARGET).R \
	$(CLEAN_RDS)
	cd src && ./01-fit.R $(RFLAGS)

$(SYNTH_RDS): \
	02-synth.R \
	$(POLICY_RDS)
	cd src && ./02-synth.R $(RFLAGS)

$(CENSOR_RDS): \
	03-censor.R \
	$(SYNTH_RDS)
	cd src && ./03-censor.R $(RFLAGS)

src/Theta_$(TARGET)%value \
$(DATA_DIR)/$(TARGET)/cens_output_$(RISKMODEL)_s$(RSEED)%rds: \
	11-grounding-$(SEARCH_METHOD).R \
	sensitivity-header.R \
	$(CENSOR_RDS)
	cd src && ./11-grounding-$(SEARCH_METHOD).R $(RFLAGS) $(SENSFLAGS)

$(DATA_DIR)/$(TARGET)/full_output_$(RISKMODEL)_s$(RSEED)%rds: \
	12-sensitivity-$(SEARCH_METHOD).R \
	sensitivity-header.R \
	src/Theta_$(TARGET).value \
	$(POLICY_RDS)
	cd src && ./12-sensitivity-$(SEARCH_METHOD).R $(RFLAGS) $(SENSFLAGS)

$(BS_RDS): \
	21-bs-fit.R \
	params_$(TARGET).R \
	$(CLEAN_RDS)
	cd src && ./21-bs-fit.R $(RFLAGS) $(BFLAGS)

$(DATA_DIR)/$(TARGET)/bs_results_%_$(RISKMODEL)_s$(RSEED).rds \
$(DATA_DIR)/$(TARGET)/results_%_$(RISKMODEL)_s$(RSEED).rds: \
	31-secondstage.R \
	$(BS_RDS) \
	$(POLICY_RDS)
	cd src && ./31-secondstage.R $(RFLAGS) -l $*

Fig1.pdf: \
	plot.R \
	plot-fig1.R \
	$(POLICY_RDS)
	cd src && ./plot-fig1.R $(RFLAGS)

Fig2.pdf: \
	plot.R \
	plot_params_$(TARGET).R \
	plot-fig2.R \
	$(DATA_DIR)/$(TARGET)/bs_results_db_$(RDS_SUFFIX) \
	$(DATA_DIR)/$(TARGET)/bs_results_ll_$(RDS_SUFFIX) \
	$(DATA_DIR)/$(TARGET)/bs_results_tps_$(RDS_SUFFIX) \
	$(DATA_DIR)/$(TARGET)/results_db_$(RDS_SUFFIX) \
	$(DATA_DIR)/$(TARGET)/results_ll_$(RDS_SUFFIX) \
	$(DATA_DIR)/$(TARGET)/results_tps_$(RDS_SUFFIX) \
	$(POLICY_RDS)
	cd src && ./plot-fig2.R $(RFLAGS)

Fig3.pdf: \
	plot.R \
	plot_params_$(TARGET).R \
	plot-fig3.R \
	$(DATA_DIR)/$(TARGET)/bs_results_$(LINK)_$(RDS_SUFFIX) \
	$(CENS_GRID_RDS) \
	$(POLICY_RDS)
	cd src && ./plot-fig3.R $(RFLAGS) -l $(LINK)

Fig4.pdf: \
	plot.R \
	plot_params_$(TARGET).R \
	plot-fig4.R \
	$(DATA_DIR)/$(TARGET)/bs_results_$(LINK)_$(RDS_SUFFIX) \
	$(CENS_GRID_RDS) \
	$(POLICY_RDS)
	cd src && ./plot-fig4.R $(RFLAGS) -l $(LINK)

Fig5.pdf: \
	plot.R \
	plot-fig5.R \
	$(POLICY_RDS)
	cd src && ./plot-fig5.R $(RFLAGS)

FigA1a%pdf \
FigA1b%pdf \
FigA1c%pdf \
FigA1d%pdf: \
	plot.R \
	plot_params_$(TARGET).R \
	plot-figA1.R \
	$(POLICY_RDS)
	cd src && ./plot-figA1.R $(RFLAGS)

#  vim: set ts=8 sw=8 tw=80 noet :
