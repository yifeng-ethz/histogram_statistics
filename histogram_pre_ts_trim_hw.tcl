package require -exact qsys 16.1

set VERSION_MAJOR_DEFAULT_CONST 26
set VERSION_MINOR_DEFAULT_CONST 0
set VERSION_PATCH_DEFAULT_CONST 0
set BUILD_DEFAULT_CONST         514
set VERSION_DATE_DEFAULT_CONST  20260514

set VERSION_STRING_DEFAULT_CONST [format "%d.%d.%d.%04d" \
    $VERSION_MAJOR_DEFAULT_CONST \
    $VERSION_MINOR_DEFAULT_CONST \
    $VERSION_PATCH_DEFAULT_CONST \
    $BUILD_DEFAULT_CONST]

set_module_property NAME                         histogram_pre_ts_trim
set_module_property DISPLAY_NAME                 "Histogram Pre Timestamp Trim"
set_module_property VERSION                      $VERSION_STRING_DEFAULT_CONST
set_module_property DESCRIPTION                  "Trims the 48-bit true hit timestamp sideband from pre-rbCAM Type-1 hits"
set_module_property GROUP                        "Mu3e Data Plane/Modules"
set_module_property AUTHOR                       "OpenAI Codex"
set_module_property INTERNAL                     false
set_module_property OPAQUE_ADDRESS_MAP           true
set_module_property INSTANTIATE_IN_SYSTEM_MODULE true
set_module_property EDITABLE                     true
set_module_property REPORT_TO_TALKBACK           false
set_module_property ALLOW_GREYBOX_GENERATION     false
set_module_property REPORT_HIERARCHY             false

add_display_item "" "Overview" GROUP
add_display_item "Overview" trim_help TEXT {Accepts pre-rbCAM Type-1 hits as data[86:0] = {true_hit_ts[47:0], legacy_payload[38:0]}. The output forwards only data[38:0] plus the original Avalon-ST packet sideband for rbCAM.}

add_fileset QUARTUS_SYNTH QUARTUS_SYNTH "" ""
set_fileset_property QUARTUS_SYNTH TOP_LEVEL histogram_pre_ts_trim
set_fileset_property QUARTUS_SYNTH ENABLE_RELATIVE_INCLUDE_PATHS false
set_fileset_property QUARTUS_SYNTH ENABLE_FILE_OVERWRITE_MODE false
add_fileset_file rtl/histogram_pre_ts_trim.vhd VHDL PATH rtl/histogram_pre_ts_trim.vhd TOP_LEVEL_FILE

add_fileset SIM_VHDL SIM_VHDL "" ""
set_fileset_property SIM_VHDL TOP_LEVEL histogram_pre_ts_trim
set_fileset_property SIM_VHDL ENABLE_RELATIVE_INCLUDE_PATHS false
set_fileset_property SIM_VHDL ENABLE_FILE_OVERWRITE_MODE false
add_fileset_file rtl/histogram_pre_ts_trim.vhd VHDL PATH rtl/histogram_pre_ts_trim.vhd TOP_LEVEL_FILE

add_parameter VERSION_MAJOR NATURAL $VERSION_MAJOR_DEFAULT_CONST
set_parameter_property VERSION_MAJOR HDL_PARAMETER true
set_parameter_property VERSION_MAJOR VISIBLE false

add_parameter VERSION_MINOR NATURAL $VERSION_MINOR_DEFAULT_CONST
set_parameter_property VERSION_MINOR HDL_PARAMETER true
set_parameter_property VERSION_MINOR VISIBLE false

add_parameter VERSION_PATCH NATURAL $VERSION_PATCH_DEFAULT_CONST
set_parameter_property VERSION_PATCH HDL_PARAMETER true
set_parameter_property VERSION_PATCH VISIBLE false

add_parameter BUILD NATURAL $BUILD_DEFAULT_CONST
set_parameter_property BUILD HDL_PARAMETER true
set_parameter_property BUILD VISIBLE false

add_parameter VERSION_DATE NATURAL $VERSION_DATE_DEFAULT_CONST
set_parameter_property VERSION_DATE HDL_PARAMETER true
set_parameter_property VERSION_DATE VISIBLE false

add_interface clock clock end
set_interface_property clock clockRate 0
set_interface_property clock ENABLED true
add_interface_port clock csi_clock_clk clk Input 1

add_interface reset reset end
set_interface_property reset associatedClock clock
set_interface_property reset synchronousEdges DEASSERT
set_interface_property reset ENABLED true
add_interface_port reset rsi_reset_reset reset Input 1

add_interface in avalon_streaming end
set_interface_property in associatedClock clock
set_interface_property in associatedReset reset
set_interface_property in dataBitsPerSymbol 87
set_interface_property in symbolsPerBeat 1
set_interface_property in readyLatency 0
set_interface_property in firstSymbolInHighOrderBits true
set_interface_property in maxChannel 15
set_interface_property in ENABLED true
add_interface_port in asi_hit_data data Input 87
add_interface_port in asi_hit_valid valid Input 1
add_interface_port in asi_hit_ready ready Output 1
add_interface_port in asi_hit_startofpacket startofpacket Input 1
add_interface_port in asi_hit_endofpacket endofpacket Input 1
add_interface_port in asi_hit_channel channel Input 4
add_interface_port in asi_hit_empty empty Input 1
add_interface_port in asi_hit_error error Input 1

add_interface out avalon_streaming start
set_interface_property out associatedClock clock
set_interface_property out associatedReset reset
set_interface_property out dataBitsPerSymbol 39
set_interface_property out symbolsPerBeat 1
set_interface_property out readyLatency 0
set_interface_property out firstSymbolInHighOrderBits true
set_interface_property out maxChannel 15
set_interface_property out ENABLED true
add_interface_port out aso_hit_data data Output 39
add_interface_port out aso_hit_valid valid Output 1
add_interface_port out aso_hit_ready ready Input 1
add_interface_port out aso_hit_startofpacket startofpacket Output 1
add_interface_port out aso_hit_endofpacket endofpacket Output 1
add_interface_port out aso_hit_channel channel Output 4
add_interface_port out aso_hit_empty empty Output 1
add_interface_port out aso_hit_error error Output 1
