include ../../../scripts/questa_one.mk

IP_SRC       := $(realpath ../..)
TB_DIR       := $(CURDIR)
WORK         ?= work_histogram_ingress_bridge
TB_SWITCH_TOP = tb_histogram_ingress_bridge_switch

STAMP       := $(shell date +%Y%m%d_%H%M%S)
REPORT_ROOT ?= $(TB_DIR)/../REPORT/histogram_ingress_bridge
SWITCH_MODE ?= observed
EXPECT_BUG  ?= 1
REPORT_DIR  ?= $(REPORT_ROOT)/switch_$(SWITCH_MODE)_$(STAMP)
REPORT_PREFIX = $(REPORT_DIR)/histogram_ingress_bridge_switch

.PHONY: compile_switch run_switch run_switch_observed run_switch_contract clean

compile_switch:
	@echo "=== Compiling histogram_ingress_bridge switch testbench ==="
	rm -rf $(WORK)
	$(VLIB) $(WORK)
	cp $(QSIM_INI) modelsim.ini
	chmod u+w modelsim.ini
	$(VMAP) -modelsimini modelsim.ini $(WORK) $(WORK)
	$(VCOM) -modelsimini modelsim.ini -work $(WORK) -2008 \
		$(IP_SRC)/rtl/histogram_ingress_bridge.vhd
	$(VCOM) -modelsimini modelsim.ini -work $(WORK) -2008 \
		$(TB_DIR)/tb_histogram_ingress_bridge_switch.vhd

run_switch: compile_switch
	@mkdir -p $(REPORT_DIR)
	$(VSIM) -modelsimini modelsim.ini -c -do "run -all; quit -f" \
		-work $(WORK) -t ps -voptargs=+acc \
		-gREPORT_PREFIX=$(REPORT_PREFIX) \
		-gEXPECT_BUG=$(EXPECT_BUG) \
		$(TB_SWITCH_TOP) | tee $(REPORT_PREFIX)_transcript.log
	@if ! grep -q "tb_histogram_ingress_bridge_switch PASS" $(REPORT_PREFIX)_transcript.log; then \
		echo "FAIL: PASS marker missing in $(REPORT_PREFIX)_transcript.log"; \
		exit 1; \
	fi
	@echo "report=$(REPORT_PREFIX)_summary.md"
	@echo "events=$(REPORT_PREFIX)_events.csv"
	@echo "transcript=$(REPORT_PREFIX)_transcript.log"

run_switch_observed:
	$(MAKE) run_switch SWITCH_MODE=observed EXPECT_BUG=1

run_switch_contract:
	$(MAKE) run_switch SWITCH_MODE=contract EXPECT_BUG=0

clean:
	rm -rf $(WORK) transcript tr_db.log vsim.wlf modelsim.ini
