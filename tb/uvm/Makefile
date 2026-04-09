QUESTA_HOME     ?= /data1/intelFPGA_pro/23.1/questa_fse
QUESTA_LICENSE  := $(QUESTA_HOME)/LR-287689_License.dat
ETH_LIC_SERVER  := 8161@lic-mentor.ethz.ch

# Local license first, ETH floating server as fallback
ifeq ($(wildcard $(QUESTA_LICENSE)),)
  export LM_LICENSE_FILE := $(ETH_LIC_SERVER)
else
  export LM_LICENSE_FILE := $(QUESTA_LICENSE):$(ETH_LIC_SERVER)
endif

VLIB = $(QUESTA_HOME)/bin/vlib
VMAP = $(QUESTA_HOME)/bin/vmap
VCOM = $(QUESTA_HOME)/bin/vcom
VLOG = $(QUESTA_HOME)/bin/vlog
VSIM = $(QUESTA_HOME)/bin/vsim

IP_SRC     = $(realpath ../..)
INTEL_LIBS = $(QUESTA_HOME)/intel/vhdl
UVM_HOME   = $(QUESTA_HOME)/verilog_src/uvm-1.2
WORK       = work_hist_uvm
TB_TOP     = tb_top

TEST ?= hist_smoke_test
SEED ?= random

# All SV source files (order matters for package dependencies)
SV_SRCS = \
	hist_if.sv \
	hist_env_pkg.sv \
	sva/hist_avmm_sva.sv \
	sva/hist_avst_sva.sv \
	sva/hist_fifo_sva.sv \
	sva/hist_pipeline_sva.sv \
	tb_top.sv

.PHONY: compile run run_cov run_vcd merge_cov clean

compile: $(WORK)/_vmake

$(WORK)/_vmake: $(IP_SRC)/histogram_statistics_v2_pkg.vhd \
                $(IP_SRC)/histogram_statistics_v2.vhd \
                $(IP_SRC)/bin_divider.vhd \
                $(IP_SRC)/coalescing_queue.vhd \
                $(IP_SRC)/hit_fifo.vhd \
                $(IP_SRC)/rr_arbiter.vhd \
                $(IP_SRC)/pingpong_sram.vhd \
                $(IP_SRC)/true_dual_port_ram_single_clock.vhd \
                $(IP_SRC)/alt_dpram/alt_dpram_true.vhd \
                $(SV_SRCS)
	@echo "=== Compiling histogram_statistics_v2 UVM testbench ==="
	rm -rf $(WORK)
	$(VLIB) $(WORK)
	$(VMAP) $(WORK) $(WORK)
	$(VMAP) lpm $(INTEL_LIBS)/220model
	@echo "--- VHDL DUT sources ---"
	$(VCOM) -work $(WORK) -2008 $(IP_SRC)/histogram_statistics_v2_pkg.vhd
	$(VCOM) -work $(WORK) -2008 $(IP_SRC)/true_dual_port_ram_single_clock.vhd
	$(VCOM) -work $(WORK) -2008 $(IP_SRC)/alt_dpram/alt_dpram_true.vhd
	$(VCOM) -work $(WORK) -2008 $(IP_SRC)/hit_fifo.vhd
	$(VCOM) -work $(WORK) -2008 $(IP_SRC)/rr_arbiter.vhd
	$(VCOM) -work $(WORK) -2008 $(IP_SRC)/bin_divider.vhd
	$(VCOM) -work $(WORK) -2008 $(IP_SRC)/coalescing_queue.vhd
	$(VCOM) -work $(WORK) -2008 $(IP_SRC)/pingpong_sram.vhd
	$(VCOM) -work $(WORK) -2008 $(IP_SRC)/histogram_statistics_v2.vhd
	@echo "--- UVM library ---"
	$(VLOG) -work $(WORK) -sv $(UVM_HOME)/src/uvm_pkg.sv +define+UVM_NO_DPI +incdir+$(UVM_HOME)/src
	@echo "--- Testbench sources ---"
	$(VLOG) -work $(WORK) -sv +incdir+. +incdir+$(UVM_HOME)/src $(SV_SRCS)
	@touch $@

run: compile
	@echo "=== Running TEST=$(TEST) SEED=$(SEED) ==="
	$(VSIM) -c -work $(WORK) -t ps -nodpiexports -voptargs=+acc \
	  -suppress 19 -suppress 3009 -suppress 3473 \
	  +UVM_TESTNAME=$(TEST) -sv_seed $(SEED) \
	  -do "quietly set NumericStdNoWarnings 1; quietly set StdArithNoWarnings 1; run -all; quit -f" \
	  $(TB_TOP)

run_cov: compile
	@echo "=== Running TEST=$(TEST) SEED=$(SEED) with coverage ==="
	$(VSIM) -c -work $(WORK) -t ps -nodpiexports -voptargs=+acc \
	  -suppress 19 -suppress 3009 -suppress 3473 \
	  -coverage \
	  +UVM_TESTNAME=$(TEST) -sv_seed $(SEED) \
	  -do "quietly set NumericStdNoWarnings 1; quietly set StdArithNoWarnings 1; coverage save -onexit $(TEST)_$(SEED).ucdb; run -all; quit -f" \
	  $(TB_TOP)

run_vcd: compile
	@echo "=== Running TEST=$(TEST) SEED=$(SEED) with VCD ==="
	$(VSIM) -c -work $(WORK) -t ps -nodpiexports -voptargs=+acc \
	  -suppress 19 -suppress 3009 -suppress 3473 \
	  +UVM_TESTNAME=$(TEST) -sv_seed $(SEED) \
	  -do "quietly set NumericStdNoWarnings 1; quietly set StdArithNoWarnings 1; vcd file $(TEST).vcd; vcd add -r /tb_top/dut/*; run -all; vcd flush; quit -f" \
	  $(TB_TOP)

merge_cov:
	$(QUESTA_HOME)/bin/vcover merge merged.ucdb *.ucdb
	$(QUESTA_HOME)/bin/vcover report -html -output cov_html merged.ucdb
	@echo "=== Coverage report at cov_html/index.html ==="

clean:
	rm -rf $(WORK) transcript vsim.wlf *.ucdb *.vcd cov_html
