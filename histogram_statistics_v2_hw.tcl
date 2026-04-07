# TCL File Generated for restored Histogram Statistics v2 source IP
# 26.0.0321 - restore histogram_statistics_v2 from feb_system_v2 generated RTL

package require -exact qsys 16.1

set_module_property DESCRIPTION "Multi-port coalescing histogram with pipelined bin index and ping-pong rate readout. Drop-in replacement for per-ASIC channel rate counter arrays."
set_module_property NAME histogram_statistics_v2
set_module_property VERSION 26.0.321
set_module_property INTERNAL false
set_module_property OPAQUE_ADDRESS_MAP true
set_module_property GROUP "Mu3e Data Plane/Debug"
set_module_property AUTHOR "Yifeng Wang"
set_module_property DISPLAY_NAME "Histogram Statistics v2 Mu3e IP"
set_module_property INSTANTIATE_IN_SYSTEM_MODULE true
set_module_property EDITABLE true
set_module_property REPORT_TO_TALKBACK false
set_module_property ALLOW_GREYBOX_GENERATION false
set_module_property REPORT_HIERARCHY false
set_module_property ELABORATION_CALLBACK elaborate
set_module_property VALIDATION_CALLBACK validate

add_fileset QUARTUS_SYNTH QUARTUS_SYNTH "" ""
set_fileset_property QUARTUS_SYNTH TOP_LEVEL histogram_statistics_v2
set_fileset_property QUARTUS_SYNTH ENABLE_RELATIVE_INCLUDE_PATHS false
set_fileset_property QUARTUS_SYNTH ENABLE_FILE_OVERWRITE_MODE false
add_fileset_file histogram_statistics_v2.vhd VHDL PATH histogram_statistics_v2.vhd TOP_LEVEL_FILE
add_fileset_file histogram_statistics_v2_pkg.vhd VHDL PATH histogram_statistics_v2_pkg.vhd
add_fileset_file hit_fifo.vhd VHDL PATH hit_fifo.vhd
add_fileset_file coalescing_queue.vhd VHDL PATH coalescing_queue.vhd
add_fileset_file rr_arbiter.vhd VHDL PATH rr_arbiter.vhd
add_fileset_file bin_divider.vhd VHDL PATH bin_divider.vhd
add_fileset_file true_dual_port_ram_single_clock.vhd VHDL PATH true_dual_port_ram_single_clock.vhd
add_fileset_file pingpong_sram.vhd VHDL PATH pingpong_sram.vhd

add_parameter N_BINS NATURAL 256
set_parameter_property N_BINS DISPLAY_NAME "Number of Bins"
set_parameter_property N_BINS HDL_PARAMETER true
add_parameter MAX_COUNT_BITS NATURAL 32
set_parameter_property MAX_COUNT_BITS DISPLAY_NAME "Max Count Bits"
set_parameter_property MAX_COUNT_BITS HDL_PARAMETER true
add_parameter DEF_LEFT_BOUND INTEGER -1000
set_parameter_property DEF_LEFT_BOUND DISPLAY_NAME "Default Left Bound"
set_parameter_property DEF_LEFT_BOUND HDL_PARAMETER true
add_parameter DEF_BIN_WIDTH NATURAL 16
set_parameter_property DEF_BIN_WIDTH DISPLAY_NAME "Default Bin Width"
set_parameter_property DEF_BIN_WIDTH HDL_PARAMETER true
add_parameter UPDATE_KEY_REPRESENTATION STRING UNSIGNED
set_parameter_property UPDATE_KEY_REPRESENTATION DISPLAY_NAME "Key Representation"
set_parameter_property UPDATE_KEY_REPRESENTATION HDL_PARAMETER true
add_parameter SAR_TICK_WIDTH NATURAL 32
set_parameter_property SAR_TICK_WIDTH DISPLAY_NAME "Boundary Resolution"
set_parameter_property SAR_TICK_WIDTH HDL_PARAMETER true
add_parameter SAR_KEY_WIDTH NATURAL 16
set_parameter_property SAR_KEY_WIDTH DISPLAY_NAME "Max Key Width"
set_parameter_property SAR_KEY_WIDTH HDL_PARAMETER true
add_parameter N_PORTS NATURAL 8
set_parameter_property N_PORTS DISPLAY_NAME "Number of Ingress Ports"
set_parameter_property N_PORTS HDL_PARAMETER true
add_parameter CHANNELS_PER_PORT NATURAL 32
set_parameter_property CHANNELS_PER_PORT DISPLAY_NAME "Channels per Port"
set_parameter_property CHANNELS_PER_PORT HDL_PARAMETER true
add_parameter COAL_QUEUE_DEPTH NATURAL 256
set_parameter_property COAL_QUEUE_DEPTH DISPLAY_NAME "Coalescing Queue Depth"
set_parameter_property COAL_QUEUE_DEPTH HDL_PARAMETER true
add_parameter ENABLE_PINGPONG BOOLEAN true
set_parameter_property ENABLE_PINGPONG DISPLAY_NAME "Enable Ping-Pong Rate Mode"
set_parameter_property ENABLE_PINGPONG HDL_PARAMETER true
add_parameter DEF_INTERVAL_CLOCKS NATURAL 125000000
set_parameter_property DEF_INTERVAL_CLOCKS DISPLAY_NAME "Default Interval (clocks)"
set_parameter_property DEF_INTERVAL_CLOCKS HDL_PARAMETER true
add_parameter UPDATE_KEY_BIT_HI NATURAL 29
set_parameter_property UPDATE_KEY_BIT_HI DISPLAY_NAME "Update Key MSB"
set_parameter_property UPDATE_KEY_BIT_HI HDL_PARAMETER true
add_parameter UPDATE_KEY_BIT_LO NATURAL 17
set_parameter_property UPDATE_KEY_BIT_LO DISPLAY_NAME "Update Key LSB"
set_parameter_property UPDATE_KEY_BIT_LO HDL_PARAMETER true
add_parameter FILTER_KEY_BIT_HI NATURAL 38
set_parameter_property FILTER_KEY_BIT_HI DISPLAY_NAME "Filter Key MSB"
set_parameter_property FILTER_KEY_BIT_HI HDL_PARAMETER true
add_parameter FILTER_KEY_BIT_LO NATURAL 35
set_parameter_property FILTER_KEY_BIT_LO DISPLAY_NAME "Filter Key LSB"
set_parameter_property FILTER_KEY_BIT_LO HDL_PARAMETER true
add_parameter AVST_DATA_WIDTH NATURAL 39
set_parameter_property AVST_DATA_WIDTH DISPLAY_NAME "AVST Data Width"
set_parameter_property AVST_DATA_WIDTH HDL_PARAMETER true
add_parameter AVST_CHANNEL_WIDTH NATURAL 4
set_parameter_property AVST_CHANNEL_WIDTH DISPLAY_NAME "AVST Channel Width"
set_parameter_property AVST_CHANNEL_WIDTH HDL_PARAMETER true
add_parameter AVS_ADDR_WIDTH NATURAL 8
set_parameter_property AVS_ADDR_WIDTH DISPLAY_NAME "Histogram Address Width"
set_parameter_property AVS_ADDR_WIDTH HDL_PARAMETER true
add_parameter N_DEBUG_INTERFACE NATURAL 6
set_parameter_property N_DEBUG_INTERFACE DISPLAY_NAME "Debug Interfaces"
set_parameter_property N_DEBUG_INTERFACE HDL_PARAMETER true
add_parameter DEBUG NATURAL 0
set_parameter_property DEBUG DISPLAY_NAME "Debug Level"
set_parameter_property DEBUG HDL_PARAMETER true
add_parameter VERSION_MAJOR NATURAL 26
set_parameter_property VERSION_MAJOR DISPLAY_NAME "Version Major"
set_parameter_property VERSION_MAJOR HDL_PARAMETER true
add_parameter VERSION_MINOR NATURAL 0
set_parameter_property VERSION_MINOR DISPLAY_NAME "Version Minor"
set_parameter_property VERSION_MINOR HDL_PARAMETER true
add_parameter VERSION_PATCH NATURAL 0
set_parameter_property VERSION_PATCH DISPLAY_NAME "Version Patch"
set_parameter_property VERSION_PATCH HDL_PARAMETER true
add_parameter BUILD NATURAL 0
set_parameter_property BUILD DISPLAY_NAME "Build Stamp"
set_parameter_property BUILD HDL_PARAMETER true
add_parameter SNOOP_EN BOOLEAN true
set_parameter_property SNOOP_EN DISPLAY_NAME "Enable Snooping"
set_parameter_property SNOOP_EN HDL_PARAMETER true
add_parameter ENABLE_PACKET BOOLEAN true
set_parameter_property ENABLE_PACKET DISPLAY_NAME "Packet Support"
set_parameter_property ENABLE_PACKET HDL_PARAMETER true
add_parameter M10K_BINS_DERIVED NATURAL 2
set_parameter_property M10K_BINS_DERIVED HDL_PARAMETER false
set_parameter_property M10K_BINS_DERIVED DERIVED true
set_parameter_property M10K_BINS_DERIVED VISIBLE false
add_parameter M10K_COAL_DERIVED NATURAL 1
set_parameter_property M10K_COAL_DERIVED HDL_PARAMETER false
set_parameter_property M10K_COAL_DERIVED DERIVED true
set_parameter_property M10K_COAL_DERIVED VISIBLE false
add_parameter M10K_TOTAL_DERIVED NATURAL 3
set_parameter_property M10K_TOTAL_DERIVED HDL_PARAMETER false
set_parameter_property M10K_TOTAL_DERIVED DERIVED true
set_parameter_property M10K_TOTAL_DERIVED VISIBLE false
add_parameter DSP_COUNT_DERIVED NATURAL 0
set_parameter_property DSP_COUNT_DERIVED HDL_PARAMETER false
set_parameter_property DSP_COUNT_DERIVED DERIVED true
set_parameter_property DSP_COUNT_DERIVED VISIBLE false
add_parameter EST_ALM_DERIVED NATURAL 710
set_parameter_property EST_ALM_DERIVED HDL_PARAMETER false
set_parameter_property EST_ALM_DERIVED DERIVED true
set_parameter_property EST_ALM_DERIVED VISIBLE false

add_interface clock clock sink
add_interface_port clock i_clk clk Input 1

add_interface reset reset sink
set_interface_property reset associatedClock clock
add_interface_port reset i_rst reset Input 1

add_interface interval_reset reset sink
set_interface_property interval_reset associatedClock clock
add_interface_port interval_reset i_interval_reset reset Input 1

add_interface hist_bin avalon end
set_interface_property hist_bin addressUnits WORDS
set_interface_property hist_bin associatedClock clock
set_interface_property hist_bin associatedReset reset
set_interface_property hist_bin bitsPerSymbol 8
set_interface_property hist_bin burstOnBurstBoundariesOnly false
set_interface_property hist_bin burstcountUnits WORDS
set_interface_property hist_bin explicitAddressSpan 0
set_interface_property hist_bin linewrapBursts false
set_interface_property hist_bin maximumPendingReadTransactions 1
set_interface_property hist_bin maximumPendingWriteTransactions 1
set_interface_property hist_bin readWaitTime 0
add_interface_port hist_bin avs_hist_bin_address address Input AVS_ADDR_WIDTH
add_interface_port hist_bin avs_hist_bin_read read Input 1
add_interface_port hist_bin avs_hist_bin_write write Input 1
add_interface_port hist_bin avs_hist_bin_writedata writedata Input 32
add_interface_port hist_bin avs_hist_bin_readdata readdata Output 32
add_interface_port hist_bin avs_hist_bin_readdatavalid readdatavalid Output 1
add_interface_port hist_bin avs_hist_bin_waitrequest waitrequest Output 1
add_interface_port hist_bin avs_hist_bin_burstcount burstcount Input AVS_ADDR_WIDTH
add_interface_port hist_bin avs_hist_bin_response response Output 2
add_interface_port hist_bin avs_hist_bin_writeresponsevalid writeresponsevalid Output 1

add_interface csr avalon end
set_interface_property csr addressUnits WORDS
set_interface_property csr associatedClock clock
set_interface_property csr associatedReset reset
set_interface_property csr bitsPerSymbol 8
set_interface_property csr burstOnBurstBoundariesOnly false
set_interface_property csr burstcountUnits WORDS
set_interface_property csr explicitAddressSpan 0
set_interface_property csr linewrapBursts false
set_interface_property csr maximumPendingReadTransactions 0
set_interface_property csr maximumPendingWriteTransactions 0
set_interface_property csr readLatency 1
set_interface_property csr readWaitTime 0
add_interface_port csr avs_csr_address address Input 4
add_interface_port csr avs_csr_read read Input 1
add_interface_port csr avs_csr_write write Input 1
add_interface_port csr avs_csr_writedata writedata Input 32
add_interface_port csr avs_csr_readdata readdata Output 32
add_interface_port csr avs_csr_waitrequest waitrequest Output 1

add_interface ctrl avalon_streaming end
set_interface_property ctrl associatedClock clock
set_interface_property ctrl associatedReset reset
set_interface_property ctrl dataBitsPerSymbol 9
set_interface_property ctrl readyLatency 0
add_interface_port ctrl asi_ctrl_data data Input 9
add_interface_port ctrl asi_ctrl_valid valid Input 1
add_interface_port ctrl asi_ctrl_ready ready Output 1

add_interface hist_fill_in avalon_streaming end
set_interface_property hist_fill_in associatedClock clock
set_interface_property hist_fill_in associatedReset reset
set_interface_property hist_fill_in dataBitsPerSymbol AVST_DATA_WIDTH
set_interface_property hist_fill_in maxChannel 15
set_interface_property hist_fill_in readyLatency 0
add_interface_port hist_fill_in asi_hist_fill_in_valid valid Input 1
add_interface_port hist_fill_in asi_hist_fill_in_ready ready Output 1
add_interface_port hist_fill_in asi_hist_fill_in_data data Input AVST_DATA_WIDTH
add_interface_port hist_fill_in asi_hist_fill_in_startofpacket startofpacket Input 1
add_interface_port hist_fill_in asi_hist_fill_in_endofpacket endofpacket Input 1
add_interface_port hist_fill_in asi_hist_fill_in_channel channel Input AVST_CHANNEL_WIDTH

add_interface fill_in_1 avalon_streaming end
set_interface_property fill_in_1 associatedClock clock
set_interface_property fill_in_1 associatedReset reset
set_interface_property fill_in_1 dataBitsPerSymbol AVST_DATA_WIDTH
set_interface_property fill_in_1 maxChannel 15
set_interface_property fill_in_1 readyLatency 0
add_interface_port fill_in_1 asi_fill_in_1_valid valid Input 1
add_interface_port fill_in_1 asi_fill_in_1_ready ready Output 1
add_interface_port fill_in_1 asi_fill_in_1_data data Input AVST_DATA_WIDTH
add_interface_port fill_in_1 asi_fill_in_1_startofpacket startofpacket Input 1
add_interface_port fill_in_1 asi_fill_in_1_endofpacket endofpacket Input 1
add_interface_port fill_in_1 asi_fill_in_1_channel channel Input AVST_CHANNEL_WIDTH

add_interface fill_in_2 avalon_streaming end
set_interface_property fill_in_2 associatedClock clock
set_interface_property fill_in_2 associatedReset reset
set_interface_property fill_in_2 dataBitsPerSymbol AVST_DATA_WIDTH
set_interface_property fill_in_2 maxChannel 15
set_interface_property fill_in_2 readyLatency 0
add_interface_port fill_in_2 asi_fill_in_2_valid valid Input 1
add_interface_port fill_in_2 asi_fill_in_2_ready ready Output 1
add_interface_port fill_in_2 asi_fill_in_2_data data Input AVST_DATA_WIDTH
add_interface_port fill_in_2 asi_fill_in_2_startofpacket startofpacket Input 1
add_interface_port fill_in_2 asi_fill_in_2_endofpacket endofpacket Input 1
add_interface_port fill_in_2 asi_fill_in_2_channel channel Input AVST_CHANNEL_WIDTH

add_interface fill_in_3 avalon_streaming end
set_interface_property fill_in_3 associatedClock clock
set_interface_property fill_in_3 associatedReset reset
set_interface_property fill_in_3 dataBitsPerSymbol AVST_DATA_WIDTH
set_interface_property fill_in_3 maxChannel 15
set_interface_property fill_in_3 readyLatency 0
add_interface_port fill_in_3 asi_fill_in_3_valid valid Input 1
add_interface_port fill_in_3 asi_fill_in_3_ready ready Output 1
add_interface_port fill_in_3 asi_fill_in_3_data data Input AVST_DATA_WIDTH
add_interface_port fill_in_3 asi_fill_in_3_startofpacket startofpacket Input 1
add_interface_port fill_in_3 asi_fill_in_3_endofpacket endofpacket Input 1
add_interface_port fill_in_3 asi_fill_in_3_channel channel Input AVST_CHANNEL_WIDTH

add_interface fill_in_4 avalon_streaming end
set_interface_property fill_in_4 associatedClock clock
set_interface_property fill_in_4 associatedReset reset
set_interface_property fill_in_4 dataBitsPerSymbol AVST_DATA_WIDTH
set_interface_property fill_in_4 maxChannel 15
set_interface_property fill_in_4 readyLatency 0
add_interface_port fill_in_4 asi_fill_in_4_valid valid Input 1
add_interface_port fill_in_4 asi_fill_in_4_ready ready Output 1
add_interface_port fill_in_4 asi_fill_in_4_data data Input AVST_DATA_WIDTH
add_interface_port fill_in_4 asi_fill_in_4_startofpacket startofpacket Input 1
add_interface_port fill_in_4 asi_fill_in_4_endofpacket endofpacket Input 1
add_interface_port fill_in_4 asi_fill_in_4_channel channel Input AVST_CHANNEL_WIDTH

add_interface fill_in_5 avalon_streaming end
set_interface_property fill_in_5 associatedClock clock
set_interface_property fill_in_5 associatedReset reset
set_interface_property fill_in_5 dataBitsPerSymbol AVST_DATA_WIDTH
set_interface_property fill_in_5 maxChannel 15
set_interface_property fill_in_5 readyLatency 0
add_interface_port fill_in_5 asi_fill_in_5_valid valid Input 1
add_interface_port fill_in_5 asi_fill_in_5_ready ready Output 1
add_interface_port fill_in_5 asi_fill_in_5_data data Input AVST_DATA_WIDTH
add_interface_port fill_in_5 asi_fill_in_5_startofpacket startofpacket Input 1
add_interface_port fill_in_5 asi_fill_in_5_endofpacket endofpacket Input 1
add_interface_port fill_in_5 asi_fill_in_5_channel channel Input AVST_CHANNEL_WIDTH

add_interface fill_in_6 avalon_streaming end
set_interface_property fill_in_6 associatedClock clock
set_interface_property fill_in_6 associatedReset reset
set_interface_property fill_in_6 dataBitsPerSymbol AVST_DATA_WIDTH
set_interface_property fill_in_6 maxChannel 15
set_interface_property fill_in_6 readyLatency 0
add_interface_port fill_in_6 asi_fill_in_6_valid valid Input 1
add_interface_port fill_in_6 asi_fill_in_6_ready ready Output 1
add_interface_port fill_in_6 asi_fill_in_6_data data Input AVST_DATA_WIDTH
add_interface_port fill_in_6 asi_fill_in_6_startofpacket startofpacket Input 1
add_interface_port fill_in_6 asi_fill_in_6_endofpacket endofpacket Input 1
add_interface_port fill_in_6 asi_fill_in_6_channel channel Input AVST_CHANNEL_WIDTH

add_interface fill_in_7 avalon_streaming end
set_interface_property fill_in_7 associatedClock clock
set_interface_property fill_in_7 associatedReset reset
set_interface_property fill_in_7 dataBitsPerSymbol AVST_DATA_WIDTH
set_interface_property fill_in_7 maxChannel 15
set_interface_property fill_in_7 readyLatency 0
add_interface_port fill_in_7 asi_fill_in_7_valid valid Input 1
add_interface_port fill_in_7 asi_fill_in_7_ready ready Output 1
add_interface_port fill_in_7 asi_fill_in_7_data data Input AVST_DATA_WIDTH
add_interface_port fill_in_7 asi_fill_in_7_startofpacket startofpacket Input 1
add_interface_port fill_in_7 asi_fill_in_7_endofpacket endofpacket Input 1
add_interface_port fill_in_7 asi_fill_in_7_channel channel Input AVST_CHANNEL_WIDTH

add_interface fill_out avalon_streaming start
set_interface_property fill_out associatedClock clock
set_interface_property fill_out associatedReset reset
set_interface_property fill_out dataBitsPerSymbol AVST_DATA_WIDTH
set_interface_property fill_out maxChannel 15
set_interface_property fill_out readyLatency 0
add_interface_port fill_out aso_hist_fill_out_valid valid Output 1
add_interface_port fill_out aso_hist_fill_out_ready ready Input 1
add_interface_port fill_out aso_hist_fill_out_data data Output AVST_DATA_WIDTH
add_interface_port fill_out aso_hist_fill_out_startofpacket startofpacket Output 1
add_interface_port fill_out aso_hist_fill_out_endofpacket endofpacket Output 1
add_interface_port fill_out aso_hist_fill_out_channel channel Output AVST_CHANNEL_WIDTH

add_interface debug_1 avalon_streaming end
set_interface_property debug_1 associatedClock clock
set_interface_property debug_1 associatedReset reset
set_interface_property debug_1 dataBitsPerSymbol 16
add_interface_port debug_1 asi_debug_1_valid valid Input 1
add_interface_port debug_1 asi_debug_1_data data Input 16

add_interface debug_2 avalon_streaming end
set_interface_property debug_2 associatedClock clock
set_interface_property debug_2 associatedReset reset
set_interface_property debug_2 dataBitsPerSymbol 16
add_interface_port debug_2 asi_debug_2_valid valid Input 1
add_interface_port debug_2 asi_debug_2_data data Input 16

add_interface debug_3 avalon_streaming end
set_interface_property debug_3 associatedClock clock
set_interface_property debug_3 associatedReset reset
set_interface_property debug_3 dataBitsPerSymbol 16
add_interface_port debug_3 asi_debug_3_valid valid Input 1
add_interface_port debug_3 asi_debug_3_data data Input 16

add_interface debug_4 avalon_streaming end
set_interface_property debug_4 associatedClock clock
set_interface_property debug_4 associatedReset reset
set_interface_property debug_4 dataBitsPerSymbol 16
add_interface_port debug_4 asi_debug_4_valid valid Input 1
add_interface_port debug_4 asi_debug_4_data data Input 16

add_interface debug_5 avalon_streaming end
set_interface_property debug_5 associatedClock clock
set_interface_property debug_5 associatedReset reset
set_interface_property debug_5 dataBitsPerSymbol 16
add_interface_port debug_5 asi_debug_5_valid valid Input 1
add_interface_port debug_5 asi_debug_5_data data Input 16

add_interface debug_6 avalon_streaming end
set_interface_property debug_6 associatedClock clock
set_interface_property debug_6 associatedReset reset
set_interface_property debug_6 dataBitsPerSymbol 16
add_interface_port debug_6 asi_debug_6_valid valid Input 1
add_interface_port debug_6 asi_debug_6_data data Input 16

proc bool_param {name} {
    return [expr {[string equal -nocase [get_parameter_value $name] "true"] ? 1 : 0}]
}

proc set_optional_stream {ifname enable data_w ch_w} {
    set_interface_property $ifname ENABLED [expr {$enable ? "true" : "false"}]
    if {$enable} {
        set_interface_property $ifname dataBitsPerSymbol $data_w
        set_interface_property $ifname maxChannel [expr {(1 << $ch_w) - 1}]
    }
}

proc elaborate {} {
    set data_w [get_parameter_value AVST_DATA_WIDTH]
    set chan_w [get_parameter_value AVST_CHANNEL_WIDTH]
    set n_ports [get_parameter_value N_PORTS]
    set n_debug [get_parameter_value N_DEBUG_INTERFACE]

    set_optional_stream hist_fill_in 1 $data_w $chan_w
    for {set idx 1} {$idx <= 7} {incr idx} {
        set_optional_stream fill_in_$idx [expr {$n_ports > $idx}] $data_w $chan_w
    }
    set_optional_stream fill_out [bool_param ENABLE_PACKET] $data_w $chan_w

    for {set idx 1} {$idx <= 6} {incr idx} {
        set_interface_property debug_$idx ENABLED [expr {$n_debug >= $idx ? "true" : "false"}]
    }

    set_parameter_value M10K_BINS_DERIVED 2
    set_parameter_value M10K_COAL_DERIVED 1
    set_parameter_value M10K_TOTAL_DERIVED 3
    set_parameter_value DSP_COUNT_DERIVED 0
    set_parameter_value EST_ALM_DERIVED 710
}

proc validate {} {
    set_parameter_value M10K_BINS_DERIVED 2
    set_parameter_value M10K_COAL_DERIVED 1
    set_parameter_value M10K_TOTAL_DERIVED 3
    set_parameter_value DSP_COUNT_DERIVED 0
    set_parameter_value EST_ALM_DERIVED 710

    if {[get_parameter_value SAR_TICK_WIDTH] < [get_parameter_value SAR_KEY_WIDTH]} {
        send_message error "SAR_TICK_WIDTH must be greater than or equal to SAR_KEY_WIDTH."
    }
}
