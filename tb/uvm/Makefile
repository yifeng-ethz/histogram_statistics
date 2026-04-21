include ../../../scripts/questa_one.mk

IP_SRC     = $(realpath ../..)
UVM_HOME   = $(QUESTA_UVM_HOME)
UVM_DPI_SO = $(QUESTA_UVM_DPI_SO)
UVM_DPI_LIB = $(basename $(UVM_DPI_SO))
DPI_CPP    ?= /usr/bin/gcc
WORK       = work_hist_uvm
TB_TOP     = tb_top
BUILD_STAMP = $(WORK)/_vmake
COV_BUILD_STAMP = $(WORK)/_vmake_cov
MODELSIM_INI := $(abspath $(WORK))/modelsim.ini

TEST ?= hist_smoke_test
SEED ?= random
UCDB ?= $(TEST)_$(SEED).ucdb
MERGED_UCDB ?= merged.ucdb
COV_HTML_DIR ?= cov_html
COVERAGE ?= 0
PLUSARGS ?=
VCD_FILE ?= $(TEST).vcd
WAVE_SCOPE ?= /tb_top/*
CASE_ID ?= P141_real_triad
POINTS ?= 256,512,1024,2048
PLUSARGS_TEMPLATE ?= +HIST_REAL_PROFILE=triad +HIST_REAL_HITS={hits}
OUTDIR ?= cov_runs/manual
BASELINE_RUN ?= cov_runs/signoff_run_v7_fullquesta
JOBS ?= 2

VCOM_COV_FLAGS = $(if $(filter 1,$(COVERAGE)),+cover=sbceft,)
VLOG_COV_FLAGS = $(if $(filter 1,$(COVERAGE)),+cover=sbceft,)

# All SV source files (order matters for package dependencies)
SV_SRCS = \
	hist_if.sv \
	hist_env_pkg.sv \
	sva/hist_avmm_sva.sv \
	sva/hist_avst_sva.sv \
	sva/hist_fifo_sva.sv \
	sva/hist_pipeline_sva.sv \
	tb_top.sv

TB_INCLUDE_DEPS = $(sort $(wildcard *.sv) $(wildcard tests/*.sv) $(wildcard sva/*.sv))

.PHONY: check_license license_diag full_questa_probe compile compile_cov run run_cov run_vcd merge_cov wave_index cov_trend cov_farm clean

check_license:
	@echo "=== Checking Mentor floating license features ==="
	@$(LMUTIL) lmstat -f msimhdlmix -c $(QUESTA_LICENSE_SERVER)
	@$(LMUTIL) lmstat -f msimcoverage -c $(QUESTA_LICENSE_SERVER)
	@$(LMUTIL) lmstat -f mtiverification -c $(QUESTA_LICENSE_SERVER)

license_diag:
	@echo "QUESTA_HOME=$(QUESTA_HOME)"
	@echo "LM_LICENSE_FILE=$(LM_LICENSE_FILE)"
	@echo "MGLS_LICENSE_FILE=$(MGLS_LICENSE_FILE)"
	@echo "SALT_LICENSE_SERVER=$(SALT_LICENSE_SERVER)"
	@echo "--- vsim banner ---"
	@$(VSIM) -version
	@echo "--- mtiverification availability ---"
	@$(LMUTIL) lmdiag -c $(QUESTA_LICENSE_SERVER) -n mtiverification || true

full_questa_probe:
	@echo "=== Probing full Questa features (rand/constraint/covergroup) ==="
	@tmpdir=$$(mktemp -d); \
	printf '%s\n' \
'module full_questa_probe;' \
'  class item;' \
'    rand bit [7:0] a;' \
'    rand bit [7:0] b;' \
'    constraint c { a < b; b inside {[1:255]}; }' \
'  endclass' \
'' \
'  item it;' \
'  covergroup cg with function sample(bit [7:0] a, bit [7:0] b);' \
'    coverpoint a;' \
'    coverpoint b;' \
'    cross a, b;' \
'  endgroup' \
'  cg cov;' \
'' \
'  initial begin' \
'    it = new();' \
'    cov = new();' \
'    repeat (5) begin' \
'      if (!it.randomize()) $$fatal(1, "randomize failed");' \
'      cov.sample(it.a, it.b);' \
'    end' \
'    $$display("FULL_QUESTA_PROBE_OK a=%0d b=%0d", it.a, it.b);' \
'    $$finish;' \
'  end' \
'endmodule' > "$$tmpdir/full_questa_probe.sv"; \
	$(VLIB) "$$tmpdir/work" >/dev/null && \
	$(VLOG) -sv -work "$$tmpdir/work" "$$tmpdir/full_questa_probe.sv" && \
	$(VSIM) -c -work "$$tmpdir/work" full_questa_probe -do "run -all; quit -f"; \
	status=$$?; rm -rf "$$tmpdir"; exit $$status

fe_runtime_probe:
	@echo "legacy FE runtime probing is deprecated; the only supported simulator is $(QUESTA_HOME)." >&2
	@exit 2

compile: $(BUILD_STAMP)

compile_cov: COVERAGE=1
compile_cov: $(COV_BUILD_STAMP)

COMPILE_DEPS = $(IP_SRC)/rtl/histogram_statistics_v2_pkg.vhd \
               $(IP_SRC)/rtl/histogram_statistics_v2.vhd \
               $(IP_SRC)/rtl/bin_divider.vhd \
               $(IP_SRC)/rtl/coalescing_queue.vhd \
               $(IP_SRC)/rtl/hit_fifo.vhd \
               $(IP_SRC)/rtl/rr_arbiter.vhd \
               $(IP_SRC)/rtl/pingpong_sram.vhd \
               $(IP_SRC)/rtl/true_dual_port_ram_single_clock.vhd \
               $(IP_SRC)/rtl/alt_dpram/alt_dpram_true.vhd \
               $(TB_INCLUDE_DEPS)

define DO_COMPILE
	@echo "=== Compiling histogram_statistics_v2 UVM testbench ==="
	rm -rf $(WORK)
	mkdir -p $(WORK)
	cp $(QUESTA_MODELSIM_INI) $(MODELSIM_INI)
	chmod u+w $(MODELSIM_INI)
	$(VLIB) $(WORK)
	$(VLIB) lpm
	$(VLIB) altera_mf
	$(VMAP) -modelsimini $(MODELSIM_INI) $(WORK) $(WORK)
	$(VMAP) -modelsimini $(MODELSIM_INI) lpm lpm
	$(VMAP) -modelsimini $(MODELSIM_INI) altera_mf altera_mf
	$(VCOM) -modelsimini $(MODELSIM_INI) -work lpm -2008 $(QUARTUS_SIM_LIB)/220pack.vhd
	$(VCOM) -modelsimini $(MODELSIM_INI) -work lpm -2008 $(QUARTUS_SIM_LIB)/220model.vhd
	$(VCOM) -modelsimini $(MODELSIM_INI) -work altera_mf -2008 $(QUARTUS_SIM_LIB)/altera_mf_components.vhd
	$(VCOM) -modelsimini $(MODELSIM_INI) -work altera_mf -2008 $(QUARTUS_SIM_LIB)/altera_mf.vhd
	@echo "--- VHDL DUT sources ---"
	$(VCOM) -modelsimini $(MODELSIM_INI) $(VCOM_COV_FLAGS) -work $(WORK) -2008 $(IP_SRC)/rtl/histogram_statistics_v2_pkg.vhd
	$(VCOM) -modelsimini $(MODELSIM_INI) $(VCOM_COV_FLAGS) -work $(WORK) -2008 $(IP_SRC)/rtl/true_dual_port_ram_single_clock.vhd
	$(VCOM) -modelsimini $(MODELSIM_INI) $(VCOM_COV_FLAGS) -work $(WORK) -2008 $(IP_SRC)/rtl/alt_dpram/alt_dpram_true.vhd
	$(VCOM) -modelsimini $(MODELSIM_INI) $(VCOM_COV_FLAGS) -work $(WORK) -2008 $(IP_SRC)/rtl/hit_fifo.vhd
	$(VCOM) -modelsimini $(MODELSIM_INI) $(VCOM_COV_FLAGS) -work $(WORK) -2008 $(IP_SRC)/rtl/rr_arbiter.vhd
	$(VCOM) -modelsimini $(MODELSIM_INI) $(VCOM_COV_FLAGS) -work $(WORK) -2008 $(IP_SRC)/rtl/bin_divider.vhd
	$(VCOM) -modelsimini $(MODELSIM_INI) $(VCOM_COV_FLAGS) -work $(WORK) -2008 $(IP_SRC)/rtl/coalescing_queue.vhd
	$(VCOM) -modelsimini $(MODELSIM_INI) $(VCOM_COV_FLAGS) -work $(WORK) -2008 $(IP_SRC)/rtl/pingpong_sram.vhd
	$(VCOM) -modelsimini $(MODELSIM_INI) $(VCOM_COV_FLAGS) -work $(WORK) -2008 $(IP_SRC)/rtl/histogram_statistics_v2.vhd
	@echo "--- UVM library ---"
	$(VLOG) -modelsimini $(MODELSIM_INI) -work $(WORK) -sv $(UVM_HOME)/src/uvm_pkg.sv +incdir+$(UVM_HOME)/src
	@echo "--- Testbench sources ---"
	$(VLOG) -modelsimini $(MODELSIM_INI) $(VLOG_COV_FLAGS) -work $(WORK) -sv +incdir+. +incdir+$(UVM_HOME)/src $(SV_SRCS)
endef

$(BUILD_STAMP): $(COMPILE_DEPS)
	$(DO_COMPILE)
	@rm -f $(COV_BUILD_STAMP)
	@touch $@

$(COV_BUILD_STAMP): $(COMPILE_DEPS)
	$(DO_COMPILE)
	@rm -f $(BUILD_STAMP)
	@touch $@

run: compile
	@echo "=== Running TEST=$(TEST) SEED=$(SEED) ==="
	$(VSIM) -modelsimini $(MODELSIM_INI) -c -work $(WORK) -t ps -dpicpppath $(DPI_CPP) -sv_lib $(UVM_DPI_LIB) -voptargs=+acc \
	  -suppress 19 -suppress 3009 -suppress 3473 \
	  +UVM_TESTNAME=$(TEST) -sv_seed $(SEED) $(PLUSARGS) \
	  -do "quietly set NumericStdNoWarnings 1; quietly set StdArithNoWarnings 1; run -all; quit -f" \
	  $(TB_TOP)

run_cov: compile_cov
	@echo "=== Running TEST=$(TEST) SEED=$(SEED) with coverage ==="
	$(VSIM) -modelsimini $(MODELSIM_INI) -c -work $(WORK) -t ps -dpicpppath $(DPI_CPP) -sv_lib $(UVM_DPI_LIB) -voptargs=+acc \
	  -suppress 19 -suppress 3009 -suppress 3473 \
	  -coverage \
	  +UVM_TESTNAME=$(TEST) -sv_seed $(SEED) $(PLUSARGS) \
	  -do "quietly set NumericStdNoWarnings 1; quietly set StdArithNoWarnings 1; coverage save -onexit $(UCDB); run -all; quit -f" \
	  $(TB_TOP)

run_vcd: compile
	@echo "=== Running TEST=$(TEST) SEED=$(SEED) with VCD ==="
	$(VSIM) -modelsimini $(MODELSIM_INI) -c -work $(WORK) -t ps -dpicpppath $(DPI_CPP) -sv_lib $(UVM_DPI_LIB) -voptargs=+acc \
	  -suppress 19 -suppress 3009 -suppress 3473 \
	  +UVM_TESTNAME=$(TEST) -sv_seed $(SEED) $(PLUSARGS) \
	  -do "quietly set NumericStdNoWarnings 1; quietly set StdArithNoWarnings 1; vcd file $(VCD_FILE); vcd add -r $(WAVE_SCOPE); run -all; vcd flush; quit -f" \
	  $(TB_TOP)

merge_cov:
	$(VCOVER) merge $(MERGED_UCDB) *.ucdb
	$(VCOVER) report -html -output $(COV_HTML_DIR) $(MERGED_UCDB)
	@echo "=== Coverage report at $(COV_HTML_DIR)/index.html ==="

wave_index:
	python3 scripts/build_wave_manifest.py

cov_trend: compile_cov
	python3 scripts/run_hist_cov_trend.py $(CASE_ID) --uvm-dir . --test $(TEST) --seed $(SEED) --points "$(POINTS)" --outdir $(OUTDIR) --plusargs-template "$(PLUSARGS_TEMPLATE)"

cov_farm: compile_cov
	python3 scripts/run_hist_cov_farm.py --uvm-dir . --baseline-run $(BASELINE_RUN) --outdir $(OUTDIR) --jobs $(JOBS)

clean:
	rm -rf $(WORK) transcript vsim.wlf *.ucdb *.vcd cov_html
