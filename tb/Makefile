include ../../scripts/questa_one.mk

IP_SRC     = $(realpath ..)
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
	$(VLIB) lpm
	$(VLIB) altera_mf
	cp $(QSIM_INI) modelsim.ini
	chmod u+w modelsim.ini
	$(VMAP) -modelsimini modelsim.ini $(WORK) $(WORK)
	$(VMAP) -modelsimini modelsim.ini lpm lpm
	$(VMAP) -modelsimini modelsim.ini altera_mf altera_mf
	$(VCOM) -modelsimini modelsim.ini -work lpm -2008 $(QUARTUS_SIM_LIB)/220pack.vhd
	$(VCOM) -modelsimini modelsim.ini -work lpm -2008 $(QUARTUS_SIM_LIB)/220model.vhd
	$(VCOM) -modelsimini modelsim.ini -work altera_mf -2008 $(QUARTUS_SIM_LIB)/altera_mf_components.vhd
	$(VCOM) -modelsimini modelsim.ini -work altera_mf -2008 $(QUARTUS_SIM_LIB)/altera_mf.vhd
	$(VCOM) -modelsimini modelsim.ini -work $(WORK) -2008 $(IP_SRC)/rtl/histogram_statistics_v2_pkg.vhd
	$(VCOM) -modelsimini modelsim.ini -work $(WORK) -2008 $(IP_SRC)/rtl/true_dual_port_ram_single_clock.vhd
	$(VCOM) -modelsimini modelsim.ini -work $(WORK) -2008 $(IP_SRC)/rtl/alt_dpram/alt_dpram_true.vhd
	$(VCOM) -modelsimini modelsim.ini -work $(WORK) -2008 $(IP_SRC)/rtl/hit_fifo.vhd
	$(VCOM) -modelsimini modelsim.ini -work $(WORK) -2008 $(IP_SRC)/rtl/rr_arbiter.vhd
	$(VCOM) -modelsimini modelsim.ini -work $(WORK) -2008 $(IP_SRC)/rtl/bin_divider.vhd
	$(VCOM) -modelsimini modelsim.ini -work $(WORK) -2008 $(IP_SRC)/rtl/coalescing_queue.vhd
	$(VCOM) -modelsimini modelsim.ini -work $(WORK) -2008 $(IP_SRC)/rtl/pingpong_sram.vhd
	$(VCOM) -modelsimini modelsim.ini -work $(WORK) -2008 $(IP_SRC)/rtl/histogram_statistics_v2.vhd
	$(VLOG) -modelsimini modelsim.ini -work $(WORK) -sv tb_histogram_statistics_v2.sv
	@touch $@

run: compile
	@echo "═══ Running TEST=$(TEST) SEED=$(SEED) ═══"
	$(VSIM) -modelsimini modelsim.ini -c -do "quietly set NumericStdNoWarnings 1; quietly set StdArithNoWarnings 1; run -all; quit -f" \
	  -work $(WORK) -t ps -voptargs=+acc \
	  -suppress 19 -suppress 3009 -suppress 3473 \
	  +TEST=$(TEST) +SEED=$(SEED) \
	  $(TB_TOP)

run_all: compile
	@echo "═══ Running ALL tests ═══"
	$(VSIM) -modelsimini modelsim.ini -c -do "quietly set NumericStdNoWarnings 1; quietly set StdArithNoWarnings 1; run -all; quit -f" \
	  -work $(WORK) -t ps -voptargs=+acc \
	  -suppress 19 -suppress 3009 -suppress 3473 \
	  +TEST=ALL +SEED=$(SEED) \
	  $(TB_TOP)

clean:
	rm -rf $(WORK) transcript vsim.wlf
