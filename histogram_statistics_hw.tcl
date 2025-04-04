
# 
# histogram_statistics "Histogram Statistics Mu3e IP" v24.0.1213
# Yifeng Wang 2024.08.09.16:45:33
# This IP generates the histogram on-the-fly by filling the predefined bins.
# 

# 25.0.0228 - add <debug_[2-5]> interface 
# 25.0.0306 - add <debug_[2-6]> interface 

# 
# request TCL package from ACDS 16.1
# 
package require -exact qsys 16.1


# 
# module histogram_statistics
# 
set_module_property DESCRIPTION "Generate on-chip real-time histogram from a data stream"
set_module_property NAME histogram_statistics
set_module_property VERSION 25.0.0306
set_module_property INTERNAL false
set_module_property OPAQUE_ADDRESS_MAP true
set_module_property GROUP "Mu3e Data Plane/Debug"
set_module_property AUTHOR "Yifeng Wang"
set_module_property DISPLAY_NAME "Histogram Statistics Mu3e IP"
set_module_property INSTANTIATE_IN_SYSTEM_MODULE true
set_module_property EDITABLE true
set_module_property REPORT_TO_TALKBACK false
set_module_property ALLOW_GREYBOX_GENERATION false
set_module_property REPORT_HIERARCHY false
set_module_property ELABORATION_CALLBACK my_elaborate


# 
# file sets
# 
add_fileset QUARTUS_SYNTH QUARTUS_SYNTH "" ""
set_fileset_property QUARTUS_SYNTH TOP_LEVEL histogram_statistics
set_fileset_property QUARTUS_SYNTH ENABLE_RELATIVE_INCLUDE_PATHS false
set_fileset_property QUARTUS_SYNTH ENABLE_FILE_OVERWRITE_MODE false
add_fileset_file histogram_statistics.vhd VHDL PATH histogram_statistics.vhd TOP_LEVEL_FILE
add_fileset_file shift_reg_with_dsp.vhd VHDL PATH shift_reg_with_dsp.vhd
add_fileset_file b2o_encoder.v VERILOG PATH b2o_encoder.v
add_fileset_file alt_dpram_true.vhd VHDL PATH alt_dpram/alt_dpram_true.vhd


# 
# parameters
# 
############################## UPDATE_KEY_BIT_HI ##############################
add_parameter UPDATE_KEY_BIT_HI NATURAL 29
set_parameter_property UPDATE_KEY_BIT_HI DEFAULT_VALUE 29
set_parameter_property UPDATE_KEY_BIT_HI DISPLAY_NAME "Update key msb location"
set_parameter_property UPDATE_KEY_BIT_HI TYPE NATURAL
set_parameter_property UPDATE_KEY_BIT_HI UNITS None
set_parameter_property UPDATE_KEY_BIT_HI ALLOWED_RANGES 0:255
set_parameter_property UPDATE_KEY_BIT_HI HDL_PARAMETER true
set descpt \
"
<html>
    Set the bit location of <b>msb</b> of the update key in the snooped data stream.
</html>
"
set_parameter_property UPDATE_KEY_BIT_HI DESCRIPTION $descpt
set_parameter_property UPDATE_KEY_BIT_HI LONG_DESCRIPTION $descpt

############################## UPDATE_KEY_BIT_LO ##############################
add_parameter UPDATE_KEY_BIT_LO NATURAL 17
set_parameter_property UPDATE_KEY_BIT_LO DEFAULT_VALUE 17
set_parameter_property UPDATE_KEY_BIT_LO DISPLAY_NAME "Update key lsb location"
set_parameter_property UPDATE_KEY_BIT_LO TYPE NATURAL
set_parameter_property UPDATE_KEY_BIT_LO UNITS None
set_parameter_property UPDATE_KEY_BIT_LO ALLOWED_RANGES 0:255
set_parameter_property UPDATE_KEY_BIT_LO HDL_PARAMETER true
set descpt \
"
<html>
    Set the bit location of <b>lsb</b> of the update key in the snooped data stream.
</html>
"
set_parameter_property UPDATE_KEY_BIT_LO DESCRIPTION $descpt
set_parameter_property UPDATE_KEY_BIT_LO LONG_DESCRIPTION $descpt

############################## UPDATE_KEY_REPRESENTATION ##############################
add_parameter UPDATE_KEY_REPRESENTATION STRING UNSIGNED ""
set_parameter_property UPDATE_KEY_REPRESENTATION DEFAULT_VALUE UNSIGNED
set_parameter_property UPDATE_KEY_REPRESENTATION DISPLAY_NAME "Update key data type"
set_parameter_property UPDATE_KEY_REPRESENTATION TYPE STRING
set_parameter_property UPDATE_KEY_REPRESENTATION UNITS None
set_parameter_property UPDATE_KEY_REPRESENTATION ALLOWED_RANGES {"UNSIGNED: unsigned" "SIGNED: signed"}
set descpt \
"
<html>
    Set the data type of the update key. No floating point format supported. <b>Signed</b> will be in 2's complement format. 
</html>
"
set_parameter_property UPDATE_KEY_REPRESENTATION DESCRIPTION $descpt
set_parameter_property UPDATE_KEY_REPRESENTATION LONG_DESCRIPTION $descpt
set_parameter_property UPDATE_KEY_REPRESENTATION HDL_PARAMETER true

############################## FILTER_KEY_BIT_HI ##############################
add_parameter FILTER_KEY_BIT_HI NATURAL 38
set_parameter_property FILTER_KEY_BIT_HI DEFAULT_VALUE 38
set_parameter_property FILTER_KEY_BIT_HI DISPLAY_NAME "Filter key msb location"
set_parameter_property FILTER_KEY_BIT_HI TYPE NATURAL
set_parameter_property FILTER_KEY_BIT_HI UNITS None
set_parameter_property FILTER_KEY_BIT_HI ALLOWED_RANGES 0:255
set_parameter_property FILTER_KEY_BIT_HI HDL_PARAMETER true
set descpt \
"
<html>
    Set the bit location of <b>msb</b> of the filter key in the snooped data stream.
</html>
"
set_parameter_property FILTER_KEY_BIT_HI DESCRIPTION $descpt
set_parameter_property FILTER_KEY_BIT_HI LONG_DESCRIPTION $descpt

############################## FILTER_KEY_BIT_LO ##############################
add_parameter FILTER_KEY_BIT_LO NATURAL 35
set_parameter_property FILTER_KEY_BIT_LO DEFAULT_VALUE 35
set_parameter_property FILTER_KEY_BIT_LO DISPLAY_NAME "Filter key lsb location"
set_parameter_property FILTER_KEY_BIT_LO TYPE NATURAL
set_parameter_property FILTER_KEY_BIT_LO UNITS None
set_parameter_property FILTER_KEY_BIT_LO ALLOWED_RANGES 0:255
set_parameter_property FILTER_KEY_BIT_LO HDL_PARAMETER true
set descpt \
"
<html>
    Set the bit location of <b>lsb</b> of the filter key in the snooped data stream.
</html>
"
set_parameter_property FILTER_KEY_BIT_LO DESCRIPTION $descpt
set_parameter_property FILTER_KEY_BIT_LO LONG_DESCRIPTION $descpt

############################## SAR_TICK_WIDTH ##############################
add_parameter SAR_TICK_WIDTH NATURAL 32
set_parameter_property SAR_TICK_WIDTH DEFAULT_VALUE 32
set_parameter_property SAR_TICK_WIDTH DISPLAY_NAME "Sampling resolution"
set_parameter_property SAR_TICK_WIDTH TYPE NATURAL
set_parameter_property SAR_TICK_WIDTH UNITS Bits
set_parameter_property SAR_TICK_WIDTH ALLOWED_RANGES 1:127
set_parameter_property SAR_TICK_WIDTH HDL_PARAMETER true
set descpt \
"
<html>
    Set the sampling resolution (SAR quantizer tick width). Affects the bin boundary resolution. Must be equal or larger than <b>SAR_KEY_WIDTH</b>.
</html>
"
set_parameter_property SAR_TICK_WIDTH DESCRIPTION $descpt
set_parameter_property SAR_TICK_WIDTH LONG_DESCRIPTION $descpt

############################## SAR_KEY_WIDTH ##############################
add_parameter SAR_KEY_WIDTH NATURAL 16
set_parameter_property SAR_KEY_WIDTH DEFAULT_VALUE 16
set_parameter_property SAR_KEY_WIDTH DISPLAY_NAME "Maximum key width"
set_parameter_property SAR_KEY_WIDTH TYPE NATURAL
set_parameter_property SAR_KEY_WIDTH UNITS Bits
set_parameter_property SAR_KEY_WIDTH ALLOWED_RANGES 1:127
set_parameter_property SAR_KEY_WIDTH HDL_PARAMETER true
set descpt \
"
<html>
    Set the maximum width of the update and filter key. Default is 16, but you can change to 32 or higher, which needs tuning of pipeline parameter of \"key getter\". 
</html>
"
set_parameter_property SAR_KEY_WIDTH DESCRIPTION $descpt
set_parameter_property SAR_KEY_WIDTH LONG_DESCRIPTION $descpt

############################## N_BINS ##############################
add_parameter N_BINS NATURAL 256
set_parameter_property N_BINS DEFAULT_VALUE 256
set_parameter_property N_BINS DISPLAY_NAME "Number of bins in the histogram"
set_parameter_property N_BINS TYPE NATURAL
set_parameter_property N_BINS UNITS None
set_parameter_property N_BINS ALLOWED_RANGES 1:2048
set_parameter_property N_BINS HDL_PARAMETER true
set descpt \
"
<html>
    Set the number of bins in the histogram. More bins will require more cycles to flush. <br>
    Resouce estimation: two M10K (= 1 true-dpram) can host up to 512 bins. 
</html>
"
set_parameter_property N_BINS DESCRIPTION $descpt
set_parameter_property N_BINS LONG_DESCRIPTION $descpt

############################## MAX_COUNT_BITS ##############################
add_parameter MAX_COUNT_BITS NATURAL 40
set_parameter_property MAX_COUNT_BITS DEFAULT_VALUE 40
set_parameter_property MAX_COUNT_BITS DISPLAY_NAME "Width of the histogram bin counter"
set_parameter_property MAX_COUNT_BITS TYPE NATURAL
set_parameter_property MAX_COUNT_BITS UNITS Bits
set_parameter_property MAX_COUNT_BITS ALLOWED_RANGES 1:72
set_parameter_property MAX_COUNT_BITS HDL_PARAMETER true
set descpt \
"
<html>
    Set the histogram bin counter width. Default is 40. <br>
    <b>Warning</b>: if <b>hist_bin</b> interface has readdata width less than histogram, you cannot read the full bin, as only lower bits are read out. Meanwhile, the overflow protection is only active when the maximum count has reached, so you should consider aligning this parameter with readdata width of <b>hist_bin</b> interface.<br>
    Resource estimation: two M10K (= 1 true-dpram) support maximum of 40 bits. four M10K (= 1 true-dpram) support maximum of 72 bits.
</html>
"
set_parameter_property MAX_COUNT_BITS DESCRIPTION $descpt
set_parameter_property MAX_COUNT_BITS LONG_DESCRIPTION $descpt

############################## N_DEBUG_INTERFACE ##############################
add_parameter N_DEBUG_INTERFACE NATURAL 6
set_parameter_property N_DEBUG_INTERFACE DISPLAY_NAME "Number of <debug> interfaces"
set_parameter_property N_DEBUG_INTERFACE TYPE NATURAL
set_parameter_property N_DEBUG_INTERFACE ALLOWED_RANGES 1:32
set_parameter_property N_DEBUG_INTERFACE HDL_PARAMETER false
set descpt \
"
<html>
    Set number of <debug> interface. Must match what the HDL file supports. <br>
</html>
"
set_parameter_property MAX_COUNT_BITS DESCRIPTION $descpt
set_parameter_property MAX_COUNT_BITS LONG_DESCRIPTION $descpt


add_parameter DEF_LEFT_BOUND INTEGER -1000
set_parameter_property DEF_LEFT_BOUND DEFAULT_VALUE -1000
set_parameter_property DEF_LEFT_BOUND DISPLAY_NAME DEF_LEFT_BOUND
set_parameter_property DEF_LEFT_BOUND TYPE INTEGER
set_parameter_property DEF_LEFT_BOUND UNITS None
set_parameter_property DEF_LEFT_BOUND ALLOWED_RANGES -2147483648:2147483647
set_parameter_property DEF_LEFT_BOUND HDL_PARAMETER true
add_parameter DEF_BIN_WIDTH NATURAL 16
set_parameter_property DEF_BIN_WIDTH DEFAULT_VALUE 16
set_parameter_property DEF_BIN_WIDTH DISPLAY_NAME DEF_BIN_WIDTH
set_parameter_property DEF_BIN_WIDTH TYPE NATURAL
set_parameter_property DEF_BIN_WIDTH UNITS None
set_parameter_property DEF_BIN_WIDTH ALLOWED_RANGES 0:2147483647
set_parameter_property DEF_BIN_WIDTH HDL_PARAMETER true
add_parameter AVS_ADDR_WIDTH NATURAL 8
set_parameter_property AVS_ADDR_WIDTH DEFAULT_VALUE 8
set_parameter_property AVS_ADDR_WIDTH DISPLAY_NAME AVS_ADDR_WIDTH
set_parameter_property AVS_ADDR_WIDTH TYPE NATURAL
set_parameter_property AVS_ADDR_WIDTH UNITS None
set_parameter_property AVS_ADDR_WIDTH ALLOWED_RANGES 0:2147483647
set_parameter_property AVS_ADDR_WIDTH HDL_PARAMETER true
add_parameter ENABLE_REPLAY BOOLEAN false
set_parameter_property ENABLE_REPLAY DEFAULT_VALUE false
set_parameter_property ENABLE_REPLAY DISPLAY_NAME ENABLE_REPLAY
set_parameter_property ENABLE_REPLAY TYPE BOOLEAN
set_parameter_property ENABLE_REPLAY UNITS None
set_parameter_property ENABLE_REPLAY HDL_PARAMETER true
add_parameter AVST_DATA_WIDTH NATURAL 39
set_parameter_property AVST_DATA_WIDTH DEFAULT_VALUE 39
set_parameter_property AVST_DATA_WIDTH DISPLAY_NAME AVST_DATA_WIDTH
set_parameter_property AVST_DATA_WIDTH TYPE NATURAL
set_parameter_property AVST_DATA_WIDTH UNITS None
set_parameter_property AVST_DATA_WIDTH ALLOWED_RANGES 0:2147483647
set_parameter_property AVST_DATA_WIDTH HDL_PARAMETER true
add_parameter AVST_CHANNEL_WIDTH NATURAL 4
set_parameter_property AVST_CHANNEL_WIDTH DEFAULT_VALUE 4
set_parameter_property AVST_CHANNEL_WIDTH DISPLAY_NAME AVST_CHANNEL_WIDTH
set_parameter_property AVST_CHANNEL_WIDTH TYPE NATURAL
set_parameter_property AVST_CHANNEL_WIDTH UNITS None
set_parameter_property AVST_CHANNEL_WIDTH ALLOWED_RANGES 0:2147483647
set_parameter_property AVST_CHANNEL_WIDTH HDL_PARAMETER true
add_parameter ENABLE_PACKET BOOLEAN true
set_parameter_property ENABLE_PACKET DEFAULT_VALUE true
set_parameter_property ENABLE_PACKET DISPLAY_NAME ENABLE_PACKET
set_parameter_property ENABLE_PACKET TYPE BOOLEAN
set_parameter_property ENABLE_PACKET UNITS None
set_parameter_property ENABLE_PACKET HDL_PARAMETER true
add_parameter SNOOP_EN BOOLEAN true
set_parameter_property SNOOP_EN DEFAULT_VALUE true
set_parameter_property SNOOP_EN DISPLAY_NAME SNOOP_EN
set_parameter_property SNOOP_EN TYPE BOOLEAN
set_parameter_property SNOOP_EN UNITS None
set_parameter_property SNOOP_EN HDL_PARAMETER true
add_parameter DEBUG NATURAL 1
set_parameter_property DEBUG DEFAULT_VALUE 1
set_parameter_property DEBUG DISPLAY_NAME DEBUG
set_parameter_property DEBUG TYPE NATURAL
set_parameter_property DEBUG UNITS None
set_parameter_property DEBUG ALLOWED_RANGES 0:2147483647
set_parameter_property DEBUG HDL_PARAMETER true


# 
# display items
# 


# 
# connection point hist_bin
# 
add_interface hist_bin avalon end
set_interface_property hist_bin addressUnits WORDS
set_interface_property hist_bin associatedClock clock_interface
set_interface_property hist_bin associatedReset reset_interface
set_interface_property hist_bin bitsPerSymbol 8
set_interface_property hist_bin burstOnBurstBoundariesOnly false
set_interface_property hist_bin burstcountUnits WORDS
set_interface_property hist_bin explicitAddressSpan 0
set_interface_property hist_bin holdTime 0
set_interface_property hist_bin linewrapBursts false
set_interface_property hist_bin maximumPendingReadTransactions 1
set_interface_property hist_bin maximumPendingWriteTransactions 1
set_interface_property hist_bin readLatency 0
set_interface_property hist_bin readWaitTime 1
set_interface_property hist_bin setupTime 0
set_interface_property hist_bin timingUnits Cycles
set_interface_property hist_bin writeWaitTime 0
set_interface_property hist_bin ENABLED true
set_interface_property hist_bin EXPORT_OF ""
set_interface_property hist_bin PORT_NAME_MAP ""
set_interface_property hist_bin CMSIS_SVD_VARIABLES ""
set_interface_property hist_bin SVD_ADDRESS_GROUP ""

add_interface_port hist_bin avs_hist_bin_readdata readdata Output 32
add_interface_port hist_bin avs_hist_bin_read read Input 1
add_interface_port hist_bin avs_hist_bin_address address Input avs_addr_width
add_interface_port hist_bin avs_hist_bin_waitrequest waitrequest Output 1
add_interface_port hist_bin avs_hist_bin_write write Input 1
add_interface_port hist_bin avs_hist_bin_writedata writedata Input 32
add_interface_port hist_bin avs_hist_bin_burstcount burstcount Input avs_addr_width
add_interface_port hist_bin avs_hist_bin_readdatavalid readdatavalid Output 1
add_interface_port hist_bin avs_hist_bin_writeresponsevalid writeresponsevalid Output 1
add_interface_port hist_bin avs_hist_bin_response response Output 2
set_interface_assignment hist_bin embeddedsw.configuration.isFlash 0
set_interface_assignment hist_bin embeddedsw.configuration.isMemoryDevice 0
set_interface_assignment hist_bin embeddedsw.configuration.isNonVolatileStorage 0
set_interface_assignment hist_bin embeddedsw.configuration.isPrintableDevice 0


# 
# connection point csr
# 
add_interface csr avalon end
set_interface_property csr addressUnits WORDS
set_interface_property csr associatedClock clock_interface
set_interface_property csr associatedReset reset_interface
set_interface_property csr bitsPerSymbol 8
set_interface_property csr burstOnBurstBoundariesOnly false
set_interface_property csr burstcountUnits WORDS
set_interface_property csr explicitAddressSpan 0
set_interface_property csr holdTime 0
set_interface_property csr linewrapBursts false
set_interface_property csr maximumPendingReadTransactions 0
set_interface_property csr maximumPendingWriteTransactions 0
set_interface_property csr readLatency 0
set_interface_property csr readWaitTime 1
set_interface_property csr setupTime 0
set_interface_property csr timingUnits Cycles
set_interface_property csr writeWaitTime 0
set_interface_property csr ENABLED true
set_interface_property csr EXPORT_OF ""
set_interface_property csr PORT_NAME_MAP ""
set_interface_property csr CMSIS_SVD_VARIABLES ""
set_interface_property csr SVD_ADDRESS_GROUP ""

add_interface_port csr avs_csr_readdata readdata Output 32
add_interface_port csr avs_csr_read read Input 1
add_interface_port csr avs_csr_address address Input 4
add_interface_port csr avs_csr_waitrequest waitrequest Output 1
add_interface_port csr avs_csr_write write Input 1
add_interface_port csr avs_csr_writedata writedata Input 32
set_interface_assignment csr embeddedsw.configuration.isFlash 0
set_interface_assignment csr embeddedsw.configuration.isMemoryDevice 0
set_interface_assignment csr embeddedsw.configuration.isNonVolatileStorage 0
set_interface_assignment csr embeddedsw.configuration.isPrintableDevice 0


# 
# connection point hist_fill_in
# 
add_interface hist_fill_in avalon_streaming end
set_interface_property hist_fill_in associatedClock clock_interface
set_interface_property hist_fill_in associatedReset reset_interface
set_interface_property hist_fill_in dataBitsPerSymbol 39
set_interface_property hist_fill_in errorDescriptor {"tserr"}
set_interface_property hist_fill_in firstSymbolInHighOrderBits true
set_interface_property hist_fill_in maxChannel 15
set_interface_property hist_fill_in readyLatency 0
set_interface_property hist_fill_in ENABLED true
set_interface_property hist_fill_in EXPORT_OF ""
set_interface_property hist_fill_in PORT_NAME_MAP ""
set_interface_property hist_fill_in CMSIS_SVD_VARIABLES ""
set_interface_property hist_fill_in SVD_ADDRESS_GROUP ""

add_interface_port hist_fill_in asi_hist_fill_in_ready ready Output 1
add_interface_port hist_fill_in asi_hist_fill_in_valid valid Input 1
add_interface_port hist_fill_in asi_hist_fill_in_data data Input avst_data_width
add_interface_port hist_fill_in asi_hist_fill_in_startofpacket startofpacket Input 1
add_interface_port hist_fill_in asi_hist_fill_in_endofpacket endofpacket Input 1
add_interface_port hist_fill_in asi_hist_fill_in_channel channel Input avst_channel_width
add_interface_port hist_fill_in asi_hist_fill_in_error error Input 1


# 
# connection point hist_fill_out
# 
add_interface hist_fill_out avalon_streaming start
set_interface_property hist_fill_out associatedClock clock_interface
set_interface_property hist_fill_out associatedReset reset_interface
set_interface_property hist_fill_out dataBitsPerSymbol 39
set_interface_property hist_fill_out errorDescriptor {"tserr"}
set_interface_property hist_fill_out firstSymbolInHighOrderBits true
set_interface_property hist_fill_out maxChannel 15
set_interface_property hist_fill_out readyLatency 0
set_interface_property hist_fill_out ENABLED true
set_interface_property hist_fill_out EXPORT_OF ""
set_interface_property hist_fill_out PORT_NAME_MAP ""
set_interface_property hist_fill_out CMSIS_SVD_VARIABLES ""
set_interface_property hist_fill_out SVD_ADDRESS_GROUP ""

add_interface_port hist_fill_out aso_hist_fill_out_ready ready Input 1
add_interface_port hist_fill_out aso_hist_fill_out_valid valid Output 1
add_interface_port hist_fill_out aso_hist_fill_out_data data Output avst_data_width
add_interface_port hist_fill_out aso_hist_fill_out_startofpacket startofpacket Output 1
add_interface_port hist_fill_out aso_hist_fill_out_endofpacket endofpacket Output 1
add_interface_port hist_fill_out aso_hist_fill_out_channel channel Output avst_channel_width
add_interface_port hist_fill_out aso_hist_fill_out_error error Output 1

# 
# connection point clock_interface
# 
add_interface clock_interface clock end
set_interface_property clock_interface clockRate 0
set_interface_property clock_interface ENABLED true
set_interface_property clock_interface EXPORT_OF ""
set_interface_property clock_interface PORT_NAME_MAP ""
set_interface_property clock_interface CMSIS_SVD_VARIABLES ""
set_interface_property clock_interface SVD_ADDRESS_GROUP ""

add_interface_port clock_interface i_clk clk Input 1


# 
# connection point reset_interface
# 
add_interface reset_interface reset end
set_interface_property reset_interface associatedClock clock_interface
set_interface_property reset_interface synchronousEdges DEASSERT
set_interface_property reset_interface ENABLED true
set_interface_property reset_interface EXPORT_OF ""
set_interface_property reset_interface PORT_NAME_MAP ""
set_interface_property reset_interface CMSIS_SVD_VARIABLES ""
set_interface_property reset_interface SVD_ADDRESS_GROUP ""

add_interface_port reset_interface i_rst reset Input 1


# 
# connection point ctrl
# 
add_interface ctrl avalon_streaming end
set_interface_property ctrl associatedClock clock_interface
set_interface_property ctrl associatedReset reset_interface
set_interface_property ctrl dataBitsPerSymbol 9
set_interface_property ctrl errorDescriptor ""
set_interface_property ctrl firstSymbolInHighOrderBits true
set_interface_property ctrl maxChannel 0
set_interface_property ctrl readyLatency 0
set_interface_property ctrl ENABLED true
set_interface_property ctrl EXPORT_OF ""
set_interface_property ctrl PORT_NAME_MAP ""
set_interface_property ctrl CMSIS_SVD_VARIABLES ""
set_interface_property ctrl SVD_ADDRESS_GROUP ""

add_interface_port ctrl asi_ctrl_data data Input 9
add_interface_port ctrl asi_ctrl_valid valid Input 1
add_interface_port ctrl asi_ctrl_ready ready Output 1


# 
# connection point debug_1
# 
# note: do it in elaboration callback 
#add_interface debug_1 avalon_streaming end
#set_interface_property debug_1 associatedClock clock_interface
#set_interface_property debug_1 associatedReset reset_interface
#set_interface_property debug_1 dataBitsPerSymbol 16
#set_interface_property debug_1 errorDescriptor ""
#set_interface_property debug_1 firstSymbolInHighOrderBits true
#set_interface_property debug_1 maxChannel 0
#set_interface_property debug_1 readyLatency 0
#set_interface_property debug_1 ENABLED true
#set_interface_property debug_1 EXPORT_OF ""
#set_interface_property debug_1 PORT_NAME_MAP ""
#set_interface_property debug_1 CMSIS_SVD_VARIABLES ""
#set_interface_property debug_1 SVD_ADDRESS_GROUP ""
#
#add_interface_port debug_1 asi_debug_1_valid valid Input 1
#add_interface_port debug_1 asi_debug_1_data data Input 16


# 
# connection point debug_2
# 


proc my_elaborate {} {
    set nDebug [get_parameter_value N_DEBUG_INTERFACE]
    for {set i 1} {[expr $i < $nDebug+1]} {incr i} {
        set interfaceName "debug_${i}"
        add_interface $interfaceName avalon_streaming end
        set_interface_property $interfaceName associatedClock clock_interface
        set_interface_property $interfaceName associatedReset reset_interface
        set_interface_property $interfaceName dataBitsPerSymbol 16
        set_interface_property $interfaceName errorDescriptor ""
        set_interface_property $interfaceName maxChannel 0
        set_interface_property $interfaceName readyLatency 0
        set_interface_property $interfaceName ENABLED true
        
    
        add_interface_port $interfaceName "asi_${interfaceName}_valid" valid Input 1
        add_interface_port $interfaceName "asi_${interfaceName}_data" data Input 16
        
    }

}

