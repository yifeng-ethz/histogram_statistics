QUESTA_PREFER_FE ?= 0
ifeq ($(QUESTA_PREFER_FE),1)
ifneq ($(wildcard /data1/intelFPGA_pro/23.1/questa_fe),)
QUESTA_HOME ?= /data1/intelFPGA_pro/23.1/questa_fe
else
QUESTA_HOME ?= /data1/intelFPGA_pro/23.1/questa_fse
endif
else
ifneq ($(wildcard /data1/intelFPGA_pro/23.1/questa_fse),)
QUESTA_HOME ?= /data1/intelFPGA_pro/23.1/questa_fse
else
QUESTA_HOME ?= /data1/intelFPGA_pro/23.1/questa_fe
endif
endif
QUESTA_FLAVOR   := $(notdir $(QUESTA_HOME))
QUESTA_LICENSE  ?= $(firstword $(wildcard $(QUESTA_HOME)/LR-*_License.dat /data1/intelFPGA/LR-*_License.dat))
ETH_LIC_SERVER  := 8161@lic-mentor.ethz.ch
TB_LICENSE_CHAIN := $(ETH_LIC_SERVER)
ifneq ($(wildcard $(QUESTA_LICENSE)),)
TB_LICENSE_CHAIN := $(ETH_LIC_SERVER):$(QUESTA_LICENSE)
endif
LMUTIL          := $(firstword $(wildcard $(QUESTA_HOME)/linux_x86_64/lmutil $(QUESTA_HOME)/bin/lmutil))

# ETH floating license first so coverage/UVM features do not fall back to the local base license.
export LM_LICENSE_FILE := $(TB_LICENSE_CHAIN)
ifeq ($(QUESTA_FLAVOR),questa_fe)
export MGLS_LICENSE_FILE := $(TB_LICENSE_CHAIN)
export SALT_LICENSE_SERVER := $(ETH_LIC_SERVER)
endif

VLIB = $(firstword $(wildcard $(QUESTA_HOME)/bin/vlib $(QUESTA_HOME)/linux_x86_64/vlib))
VMAP = $(firstword $(wildcard $(QUESTA_HOME)/bin/vmap $(QUESTA_HOME)/linux_x86_64/vmap))
VCOM = $(firstword $(wildcard $(QUESTA_HOME)/bin/vcom $(QUESTA_HOME)/linux_x86_64/vcom))
VLOG = $(firstword $(wildcard $(QUESTA_HOME)/bin/vlog $(QUESTA_HOME)/linux_x86_64/vlog))
VSIM = $(firstword $(wildcard $(QUESTA_HOME)/bin/vsim $(QUESTA_HOME)/linux_x86_64/vsim))
VCOVER = $(firstword $(wildcard $(QUESTA_HOME)/bin/vcover $(QUESTA_HOME)/linux_x86_64/vcover))

IP_SRC     = $(realpath ../..)
INTEL_LIBS = $(firstword $(wildcard $(QUESTA_HOME)/intel/vhdl $(QUESTA_HOME)/../questa_fse/intel/vhdl))
UVM_HOME   = $(firstword $(wildcard $(QUESTA_HOME)/verilog_src/uvm-1.2 $(QUESTA_HOME)/uvm-1.2))
UVM_DPI_SO = $(firstword $(wildcard $(QUESTA_HOME)/uvm-1.2/linux_x86_64/uvm_dpi.so $(QUESTA_HOME)/verilog_src/uvm-1.2/linux_x86_64/uvm_dpi.so))
UVM_DPI_LIB = $(basename $(UVM_DPI_SO))
DPI_CPP    ?= /usr/bin/gcc
WORK       = work_hist_uvm
TB_TOP     = tb_top
BUILD_STAMP = $(WORK)/_vmake
COV_BUILD_STAMP = $(WORK)/_vmake_cov

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

.PHONY: check_license license_diag full_questa_probe fe_runtime_probe compile compile_cov run run_cov run_vcd merge_cov wave_index cov_trend cov_farm clean

check_license:
	@echo "=== Checking Mentor floating license features ==="
	@$(LMUTIL) lmstat -f msimhdlmix -c $(ETH_LIC_SERVER)
	@$(LMUTIL) lmstat -f msimcoverage -c $(ETH_LIC_SERVER)
	@$(LMUTIL) lmstat -f mtiverification -c $(ETH_LIC_SERVER)

license_diag:
	@echo "QUESTA_HOME=$(QUESTA_HOME)"
	@echo "QUESTA_FLAVOR=$(QUESTA_FLAVOR)"
	@echo "LM_LICENSE_FILE=$(LM_LICENSE_FILE)"
	@echo "MGLS_LICENSE_FILE=$(MGLS_LICENSE_FILE)"
	@echo "SALT_LICENSE_SERVER=$(SALT_LICENSE_SERVER)"
	@echo "--- vsim banner ---"
	@$(VSIM) -version
	@echo "--- mtiverification availability ---"
	@$(LMUTIL) lmdiag -c $(ETH_LIC_SERVER) -n mtiverification || true

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
	@if [ "$(QUESTA_FLAVOR)" != "questa_fe" ]; then \
	  echo "fe_runtime_probe requires QUESTA_PREFER_FE=1"; \
	  exit 2; \
	fi
	@echo "=== Probing questa_fe runtime entitlement ==="
	@tmpdir=$$(mktemp -d); \
	mkdir -p "$$tmpdir/logs"; \
	printf '%s\n' \
'module fe_runtime_probe;' \
'  initial begin' \
'    $$display("FE_RUNTIME_PROBE_OK");' \
'    $$finish;' \
'  end' \
'endmodule' > "$$tmpdir/fe_runtime_probe.sv"; \
	echo "--- mgls_ok msimhdlsim ---"; \
	"$(QUESTA_HOME)/linux_x86_64/mgls_ok" msimhdlsim || true; \
	echo "--- mgls_ok mtiverification ---"; \
	"$(QUESTA_HOME)/linux_x86_64/mgls_ok" mtiverification || true; \
	echo "--- mgls_ok intelqsim ---"; \
	int_status=0; \
	"$(QUESTA_HOME)/linux_x86_64/mgls_ok" intelqsim || int_status=$$?; \
	"$(VLIB)" "$$tmpdir/work" >/dev/null; \
	"$(VLOG)" -sv -work "$$tmpdir/work" "$$tmpdir/fe_runtime_probe.sv" >/dev/null; \
	vsim_status=0; \
	SALT_LOGGING_DIR="$$tmpdir/logs" "$(VSIM)" -c -work "$$tmpdir/work" fe_runtime_probe -do "run -all; quit -f" >"$$tmpdir/vsim.log" 2>&1 || vsim_status=$$?; \
	echo "--- vsim output ---"; \
	sed -n '1,120p' "$$tmpdir/vsim.log"; \
	echo "--- SALT logs ---"; \
	find "$$tmpdir/logs" -maxdepth 2 -type f | sort | sed -n '1,40p'; \
	rm -rf "$$tmpdir"; \
	test $$int_status -eq 0 -a $$vsim_status -eq 0

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
	$(VLIB) $(WORK)
	$(VMAP) $(WORK) $(WORK)
	$(VMAP) lpm $(INTEL_LIBS)/220model
	@echo "--- VHDL DUT sources ---"
	$(VCOM) $(VCOM_COV_FLAGS) -work $(WORK) -2008 $(IP_SRC)/rtl/histogram_statistics_v2_pkg.vhd
	$(VCOM) $(VCOM_COV_FLAGS) -work $(WORK) -2008 $(IP_SRC)/rtl/true_dual_port_ram_single_clock.vhd
	$(VCOM) $(VCOM_COV_FLAGS) -work $(WORK) -2008 $(IP_SRC)/rtl/alt_dpram/alt_dpram_true.vhd
	$(VCOM) $(VCOM_COV_FLAGS) -work $(WORK) -2008 $(IP_SRC)/rtl/hit_fifo.vhd
	$(VCOM) $(VCOM_COV_FLAGS) -work $(WORK) -2008 $(IP_SRC)/rtl/rr_arbiter.vhd
	$(VCOM) $(VCOM_COV_FLAGS) -work $(WORK) -2008 $(IP_SRC)/rtl/bin_divider.vhd
	$(VCOM) $(VCOM_COV_FLAGS) -work $(WORK) -2008 $(IP_SRC)/rtl/coalescing_queue.vhd
	$(VCOM) $(VCOM_COV_FLAGS) -work $(WORK) -2008 $(IP_SRC)/rtl/pingpong_sram.vhd
	$(VCOM) $(VCOM_COV_FLAGS) -work $(WORK) -2008 $(IP_SRC)/rtl/histogram_statistics_v2.vhd
	@echo "--- UVM library ---"
	$(VLOG) -work $(WORK) -sv $(UVM_HOME)/src/uvm_pkg.sv +incdir+$(UVM_HOME)/src
	@echo "--- Testbench sources ---"
	$(VLOG) $(VLOG_COV_FLAGS) -work $(WORK) -sv +incdir+. +incdir+$(UVM_HOME)/src $(SV_SRCS)
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
	$(VSIM) -c -work $(WORK) -t ps -dpicpppath $(DPI_CPP) -sv_lib $(UVM_DPI_LIB) -voptargs=+acc \
	  -suppress 19 -suppress 3009 -suppress 3473 \
	  +UVM_TESTNAME=$(TEST) -sv_seed $(SEED) $(PLUSARGS) \
	  -do "quietly set NumericStdNoWarnings 1; quietly set StdArithNoWarnings 1; run -all; quit -f" \
	  $(TB_TOP)

run_cov: compile_cov
	@echo "=== Running TEST=$(TEST) SEED=$(SEED) with coverage ==="
	$(VSIM) -c -work $(WORK) -t ps -dpicpppath $(DPI_CPP) -sv_lib $(UVM_DPI_LIB) -voptargs=+acc \
	  -suppress 19 -suppress 3009 -suppress 3473 \
	  -coverage \
	  +UVM_TESTNAME=$(TEST) -sv_seed $(SEED) $(PLUSARGS) \
	  -do "quietly set NumericStdNoWarnings 1; quietly set StdArithNoWarnings 1; coverage save -onexit $(UCDB); run -all; quit -f" \
	  $(TB_TOP)

run_vcd: compile
	@echo "=== Running TEST=$(TEST) SEED=$(SEED) with VCD ==="
	$(VSIM) -c -work $(WORK) -t ps -dpicpppath $(DPI_CPP) -sv_lib $(UVM_DPI_LIB) -voptargs=+acc \
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
