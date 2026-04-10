QUESTA_HOME ?= /data1/intelFPGA_pro/23.1/questa_fse
VLIB        = $(QUESTA_HOME)/bin/vlib
VMAP        = $(QUESTA_HOME)/bin/vmap
VCOM        = $(QUESTA_HOME)/bin/vcom
VLOG        = $(QUESTA_HOME)/bin/vlog
VSIM        = $(QUESTA_HOME)/bin/vsim

QUESTA_LICENSE := $(QUESTA_HOME)/LR-287689_License.dat
ETH_LIC_SERVER := 8161@lic-mentor.ethz.ch
ifeq ($(wildcard $(QUESTA_LICENSE)),)
  export LM_LICENSE_FILE := $(ETH_LIC_SERVER)
else
  export LM_LICENSE_FILE := $(QUESTA_LICENSE):$(ETH_LIC_SERVER)
endif

IP_SRC     = $(realpath ..)
INTEL_LIBS = $(QUESTA_HOME)/intel/vhdl
WORK       = work_hist_stat_v2
TB_TOP     = tb_histogram_statistics_v2

TEST ?= B01_smoke
SEED ?= 42

.PHONY: compile run run_all clean

compile: $(WORK)/_vmake

$(WORK)/_vmake: $(IP_SRC)/rtl/histogram_statistics_v2_pkg.vhd \
                $(IP_SRC)/rtl/histogram_statistics_v2.vhd \
                $(IP_SRC)/rtl/bin_divider.vhd \
                $(IP_SRC)/rtl/coalescing_queue.vhd \
                $(IP_SRC)/rtl/hit_fifo.vhd \
                $(IP_SRC)/rtl/rr_arbiter.vhd \
                $(IP_SRC)/rtl/pingpong_sram.vhd \
                $(IP_SRC)/rtl/true_dual_port_ram_single_clock.vhd \
                $(IP_SRC)/rtl/alt_dpram/alt_dpram_true.vhd \
                tb_histogram_statistics_v2.sv
	@echo "═══ Compiling histogram_statistics_v2 standalone testbench ═══"
	rm -rf $(WORK)
	$(VLIB) $(WORK)
	$(VMAP) $(WORK) $(WORK)
	$(VMAP) lpm $(INTEL_LIBS)/220model
	$(VCOM) -work $(WORK) -2008 $(IP_SRC)/rtl/histogram_statistics_v2_pkg.vhd
	$(VCOM) -work $(WORK) -2008 $(IP_SRC)/rtl/true_dual_port_ram_single_clock.vhd
	$(VCOM) -work $(WORK) -2008 $(IP_SRC)/rtl/alt_dpram/alt_dpram_true.vhd
	$(VCOM) -work $(WORK) -2008 $(IP_SRC)/rtl/hit_fifo.vhd
	$(VCOM) -work $(WORK) -2008 $(IP_SRC)/rtl/rr_arbiter.vhd
	$(VCOM) -work $(WORK) -2008 $(IP_SRC)/rtl/bin_divider.vhd
	$(VCOM) -work $(WORK) -2008 $(IP_SRC)/rtl/coalescing_queue.vhd
	$(VCOM) -work $(WORK) -2008 $(IP_SRC)/rtl/pingpong_sram.vhd
	$(VCOM) -work $(WORK) -2008 $(IP_SRC)/rtl/histogram_statistics_v2.vhd
	$(VLOG) -work $(WORK) -sv tb_histogram_statistics_v2.sv
	@touch $@

run: compile
	@echo "═══ Running TEST=$(TEST) SEED=$(SEED) ═══"
	$(VSIM) -c -do "quietly set NumericStdNoWarnings 1; quietly set StdArithNoWarnings 1; run -all; quit -f" \
	  -work $(WORK) -t ps -voptargs=+acc \
	  -suppress 19 -suppress 3009 -suppress 3473 -nodpiexports \
	  +TEST=$(TEST) +SEED=$(SEED) \
	  $(TB_TOP)

run_all: compile
	@echo "═══ Running ALL tests ═══"
	$(VSIM) -c -do "quietly set NumericStdNoWarnings 1; quietly set StdArithNoWarnings 1; run -all; quit -f" \
	  -work $(WORK) -t ps -voptargs=+acc \
	  -suppress 19 -suppress 3009 -suppress 3473 -nodpiexports \
	  +TEST=ALL +SEED=$(SEED) \
	  $(TB_TOP)

clean:
	rm -rf $(WORK) transcript vsim.wlf
