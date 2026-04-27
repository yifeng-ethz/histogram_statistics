package require -exact qsys 16.1

set VERSION_MAJOR_DEFAULT_CONST 26
set VERSION_MINOR_DEFAULT_CONST 0
set VERSION_PATCH_DEFAULT_CONST 2
set BUILD_DEFAULT_CONST         425
set VERSION_DATE_DEFAULT_CONST  20260425
set VERSION_GIT_DEFAULT_CONST   481097348
set VERSION_GIT_HEX_DEFAULT_CONST 0x1CACF684
set IP_UID_DEFAULT_CONST        1212764994
set INSTANCE_ID_DEFAULT_CONST   0

set VERSION_STRING_DEFAULT_CONST [format "%d.%d.%d.%04d" \
    $VERSION_MAJOR_DEFAULT_CONST \
    $VERSION_MINOR_DEFAULT_CONST \
    $VERSION_PATCH_DEFAULT_CONST \
    $BUILD_DEFAULT_CONST]

set_module_property NAME                         histogram_ingress_bridge
set_module_property DISPLAY_NAME                 "Histogram Ingress Bridge"
set_module_property VERSION                      $VERSION_STRING_DEFAULT_CONST
set_module_property DESCRIPTION                  "Histogram Ingress Bridge Mu3e IP Core"
set_module_property GROUP                        "Mu3e Data Plane/Modules"
set_module_property AUTHOR                       "OpenAI Codex"
set_module_property INTERNAL                     false
set_module_property OPAQUE_ADDRESS_MAP           true
set_module_property INSTANTIATE_IN_SYSTEM_MODULE true
set_module_property EDITABLE                     true
set_module_property REPORT_TO_TALKBACK           false
set_module_property ALLOW_GREYBOX_GENERATION     false
set_module_property REPORT_HIERARCHY             false
set_module_property ELABORATION_CALLBACK         elaborate
set_module_property VALIDATION_CALLBACK          validate

proc add_html_text {group_name item_name html_text} {
    add_display_item $group_name $item_name TEXT ""
    set_display_item_property $item_name DISPLAY_HINT html
    set_display_item_property $item_name TEXT $html_text
}

proc validate {} {
    set default_select_post [get_parameter_value DEFAULT_SELECT_POST]
    set enable_post_forward [get_parameter_value ENABLE_POST_FORWARD]
    set filter_post_hits    [get_parameter_value FILTER_POST_HIT_WORDS]
    set ip_uid_value        [get_parameter_value IP_UID]
    set instance_id         [get_parameter_value INSTANCE_ID]
    if {$default_select_post < 0 || $default_select_post > 1} {
        send_message error "DEFAULT_SELECT_POST must stay in the range 0..1."
    }
    if {$enable_post_forward < 0 || $enable_post_forward > 1} {
        send_message error "ENABLE_POST_FORWARD must stay in the range 0..1."
    }
    if {$filter_post_hits < 0 || $filter_post_hits > 1} {
        send_message error "FILTER_POST_HIT_WORDS must stay in the range 0..1."
    }
    if {$ip_uid_value < 0 || $ip_uid_value > 2147483647} {
        send_message error "IP_UID must stay in the signed 31-bit Platform Designer integer range."
    }
    if {$instance_id < 0 || $instance_id > 2147483647} {
        send_message error "INSTANCE_ID must stay in the signed 31-bit Platform Designer integer range."
    }
}

proc elaborate {} {
    catch {
        set default_post [get_parameter_value DEFAULT_SELECT_POST]
        set enable_post_forward [get_parameter_value ENABLE_POST_FORWARD]
        set filter_post_hits [get_parameter_value FILTER_POST_HIT_WORDS]
        set default_label [expr {$default_post ? "post-hit-stack" : "pre-hit-stack"}]
        set post_forward_label [expr {$enable_post_forward ? "and the post-hit-stack stream to <b>post_out</b>" : "while the post-hit-stack side acts as a histogram tap only"}]
        set filter_label [expr {$filter_post_hits ? "enabled" : "disabled"}]
        set_display_item_property routing_overview_html TEXT [format {<html><b>Forward paths stay intact</b><br/>The bridge always forwards the pre-hit-stack stream to <b>pre_out</b> %s.<br/><br/><b>Histogram source</b><br/>The histogram tap is routed from the <b>%s</b> path after reset. Software may request the other path later through <b>CONTROL.select_post</b>. A source switch takes effect only when both packet streams are idle.<br/><br/><b>Post hit filter</b><br/>Post-stream hit-only filtering is <b>%s</b>. When enabled, K28.5/K28.4 frame words, debug words, K23.7 subheaders, and zero-hit subheaders are drained without histogram updates.</html>} $post_forward_label $default_label $filter_label]
    }
}

add_fileset QUARTUS_SYNTH QUARTUS_SYNTH "" ""
set_fileset_property QUARTUS_SYNTH TOP_LEVEL histogram_ingress_bridge
set_fileset_property QUARTUS_SYNTH ENABLE_RELATIVE_INCLUDE_PATHS false
set_fileset_property QUARTUS_SYNTH ENABLE_FILE_OVERWRITE_MODE false
add_fileset_file rtl/histogram_ingress_bridge.vhd VHDL PATH rtl/histogram_ingress_bridge.vhd TOP_LEVEL_FILE

add_fileset SIM_VHDL SIM_VHDL "" ""
set_fileset_property SIM_VHDL TOP_LEVEL histogram_ingress_bridge
set_fileset_property SIM_VHDL ENABLE_RELATIVE_INCLUDE_PATHS false
set_fileset_property SIM_VHDL ENABLE_FILE_OVERWRITE_MODE false
add_fileset_file rtl/histogram_ingress_bridge.vhd VHDL PATH rtl/histogram_ingress_bridge.vhd TOP_LEVEL_FILE

add_parameter DEFAULT_SELECT_POST NATURAL 0
set_parameter_property DEFAULT_SELECT_POST DISPLAY_NAME "Default Select Post"
set_parameter_property DEFAULT_SELECT_POST ALLOWED_RANGES 0:1
set_parameter_property DEFAULT_SELECT_POST HDL_PARAMETER true
set_parameter_property DEFAULT_SELECT_POST DESCRIPTION "0 selects the pre-hit-stack tap after reset. 1 selects the post-hit-stack tap after reset."

add_parameter ENABLE_POST_FORWARD NATURAL 1
set_parameter_property ENABLE_POST_FORWARD DISPLAY_NAME "Enable Post Forward"
set_parameter_property ENABLE_POST_FORWARD ALLOWED_RANGES 0:1
set_parameter_property ENABLE_POST_FORWARD HDL_PARAMETER true
set_parameter_property ENABLE_POST_FORWARD DESCRIPTION "1 forwards post_in to post_out. 0 makes post_in a histogram tap only and drains the unselected post copy locally."

add_parameter FILTER_POST_HIT_WORDS NATURAL 0
set_parameter_property FILTER_POST_HIT_WORDS DISPLAY_NAME "Filter Post Hit Words"
set_parameter_property FILTER_POST_HIT_WORDS ALLOWED_RANGES 0:1
set_parameter_property FILTER_POST_HIT_WORDS HDL_PARAMETER true
set_parameter_property FILTER_POST_HIT_WORDS DESCRIPTION "1 forwards only post-hit-stack data beats after K23.7 subheaders to hist_out. Frame headers, debug words, subheaders, and trailers are drained locally."

add_parameter IP_UID NATURAL $IP_UID_DEFAULT_CONST
set_parameter_property IP_UID DISPLAY_NAME "UID"
set_parameter_property IP_UID ALLOWED_RANGES 0:2147483647
set_parameter_property IP_UID HDL_PARAMETER true
set_parameter_property IP_UID DISPLAY_HINT hexadecimal
set_parameter_property IP_UID DESCRIPTION {Software-visible IP identifier at CSR word 0. Default corresponds to ASCII "HISB" (0x48495342).}

add_parameter VERSION_MAJOR NATURAL $VERSION_MAJOR_DEFAULT_CONST
set_parameter_property VERSION_MAJOR DISPLAY_NAME "Version Major"
set_parameter_property VERSION_MAJOR HDL_PARAMETER true
set_parameter_property VERSION_MAJOR VISIBLE false

add_parameter VERSION_MINOR NATURAL $VERSION_MINOR_DEFAULT_CONST
set_parameter_property VERSION_MINOR DISPLAY_NAME "Version Minor"
set_parameter_property VERSION_MINOR HDL_PARAMETER true
set_parameter_property VERSION_MINOR VISIBLE false

add_parameter VERSION_PATCH NATURAL $VERSION_PATCH_DEFAULT_CONST
set_parameter_property VERSION_PATCH DISPLAY_NAME "Version Patch"
set_parameter_property VERSION_PATCH HDL_PARAMETER true
set_parameter_property VERSION_PATCH VISIBLE false

add_parameter BUILD NATURAL $BUILD_DEFAULT_CONST
set_parameter_property BUILD DISPLAY_NAME "Build"
set_parameter_property BUILD HDL_PARAMETER true
set_parameter_property BUILD VISIBLE false

add_parameter VERSION_DATE NATURAL $VERSION_DATE_DEFAULT_CONST
set_parameter_property VERSION_DATE DISPLAY_NAME "Version Date"
set_parameter_property VERSION_DATE HDL_PARAMETER true
set_parameter_property VERSION_DATE VISIBLE false

add_parameter VERSION_GIT NATURAL $VERSION_GIT_DEFAULT_CONST
set_parameter_property VERSION_GIT DISPLAY_NAME "Version Git"
set_parameter_property VERSION_GIT HDL_PARAMETER true
set_parameter_property VERSION_GIT DISPLAY_HINT hexadecimal
set_parameter_property VERSION_GIT VISIBLE false

add_parameter INSTANCE_ID NATURAL $INSTANCE_ID_DEFAULT_CONST
set_parameter_property INSTANCE_ID DISPLAY_NAME "Instance ID"
set_parameter_property INSTANCE_ID ALLOWED_RANGES 0:2147483647
set_parameter_property INSTANCE_ID HDL_PARAMETER true

set TAB_CONFIGURATION "Configuration"
set TAB_IDENTITY      "Identity"
set TAB_INTERFACES    "Interfaces"
set TAB_REGMAP        "Register Map"

add_display_item "" $TAB_CONFIGURATION GROUP tab
add_display_item $TAB_CONFIGURATION "Overview" GROUP
add_display_item $TAB_CONFIGURATION "Source Select" GROUP
add_html_text "Overview" routing_overview_html {<html><b>Forward paths stay intact</b><br/>The bridge forwards both datapath streams and only mirrors the selected one into the histogram sink.</html>}
add_html_text "Source Select" select_help_html {<html><b>CONTROL.select_post</b><br/>0 = pre-hit-stack source, 1 = post-hit-stack source.<br/><br/>The requested source only becomes live after both packet streams are idle. Read <b>STATUS</b> to see the live selection and whether a switch is pending.</html>}
add_display_item "Source Select" DEFAULT_SELECT_POST parameter
add_display_item "Source Select" ENABLE_POST_FORWARD parameter
add_display_item "Source Select" FILTER_POST_HIT_WORDS parameter

add_display_item "" $TAB_IDENTITY GROUP tab
add_display_item $TAB_IDENTITY "Delivered Profile" GROUP
add_display_item $TAB_IDENTITY "Versioning" GROUP
add_html_text "Delivered Profile" profile_html [format {<html><b>Catalog revision</b><br/>This release is packaged as <b>%s</b>.<br/><br/><b>Common identity header</b><br/>Word <b>0</b> is <b>UID</b> (default ASCII "HISB").<br/>Word <b>1</b> is <b>META</b>: write 0=VERSION, 1=DATE, 2=GIT, 3=INSTANCE_ID.</html>} $VERSION_STRING_DEFAULT_CONST]
add_html_text "Versioning" versioning_html [format {<html><b>VERSION encoding</b><br/>VERSION[31:24] = MAJOR, VERSION[23:16] = MINOR, VERSION[15:12] = PATCH, VERSION[11:0] = BUILD.<br/><br/><b>Packaged git stamp</b><br/>Default <b>VERSION_GIT</b> = <b>%s</b>.</html>} $VERSION_GIT_HEX_DEFAULT_CONST]
add_display_item "Versioning" IP_UID parameter
add_display_item "Versioning" INSTANCE_ID parameter

add_display_item "" $TAB_INTERFACES GROUP tab
add_display_item $TAB_INTERFACES "Clock / Reset" GROUP
add_display_item $TAB_INTERFACES "Data Path" GROUP
add_display_item $TAB_INTERFACES "Control Path" GROUP
add_html_text "Clock / Reset" clock_html {<html><b>clock</b> and <b>reset</b><br/>Single synchronous domain for datapath forwarding, histogram tap selection, and CSR logic.</html>}
add_html_text "Data Path" datapath_html {<html><b>pre_in / pre_out</b><br/>39-bit packetized pre-hit-stack stream with channel, empty, and error sidebands.<br/><br/><b>post_in / post_out</b><br/>36-bit packetized post-hit-stack stream. Set <b>Enable Post Forward</b> to <b>0</b> when the post input is only a split histogram tap copy.<br/><br/><b>hist_out</b><br/>39-bit histogram source. Post-hit-stack data are zero-extended to 39 bits and presented on channel 0. With <b>Filter Post Hit Words</b> enabled, only real post-hit-stack hit beats after K23.7 subheaders assert <b>hist_out.valid</b>.</html>}
add_html_text "Control Path" control_html {<html><b>csr</b><br/>Four-word Avalon-MM CSR aperture: UID, META, CONTROL, STATUS.</html>}

add_display_item "" $TAB_REGMAP GROUP tab
add_display_item $TAB_REGMAP "CSR Window" GROUP
add_html_text "CSR Window" csr_html {<html><table border="1" cellpadding="3" width="100%">
<tr><th>Word</th><th>Name</th><th>Access</th><th>Description</th></tr>
<tr><td>0x00</td><td>UID</td><td>RO</td><td>Software-visible IP identifier. Default ASCII <b>HISB</b>.</td></tr>
<tr><td>0x01</td><td>META</td><td>RW/RO</td><td>Read-multiplexed metadata word. Write 0=VERSION, 1=DATE, 2=GIT, 3=INSTANCE_ID.</td></tr>
<tr><td>0x02</td><td>CONTROL</td><td>RW</td><td>Bit 0 requests the active histogram source: 0=pre-hit-stack, 1=post-hit-stack.</td></tr>
<tr><td>0x03</td><td>STATUS</td><td>RO</td><td>Bit 0 = live source, bit 1 = requested source, bit 2 = switch pending, bit 8 = pre packet active, bit 9 = post packet active, bit 10 = post hit filter enabled, bit 11 = post hit region.</td></tr>
</table></html>}

add_interface clock clock end
set_interface_property clock clockRate 0
set_interface_property clock ENABLED true
add_interface_port clock csi_clock_clk clk Input 1

add_interface reset reset end
set_interface_property reset associatedClock clock
set_interface_property reset synchronousEdges DEASSERT
set_interface_property reset ENABLED true
add_interface_port reset rsi_reset_reset reset Input 1

add_interface csr avalon end
set_interface_property csr addressUnits WORDS
set_interface_property csr associatedClock clock
set_interface_property csr associatedReset reset
set_interface_property csr bitsPerSymbol 8
set_interface_property csr burstOnBurstBoundariesOnly false
set_interface_property csr explicitAddressSpan 0
set_interface_property csr holdTime 0
set_interface_property csr linewrapBursts false
set_interface_property csr maximumPendingReadTransactions 0
set_interface_property csr readLatency 0
set_interface_property csr readWaitTime 1
set_interface_property csr setupTime 0
set_interface_property csr timingUnits Cycles
set_interface_property csr writeWaitTime 0
set_interface_property csr ENABLED true
add_interface_port csr avs_csr_address address Input 2
add_interface_port csr avs_csr_write write Input 1
add_interface_port csr avs_csr_read read Input 1
add_interface_port csr avs_csr_writedata writedata Input 32
add_interface_port csr avs_csr_readdata readdata Output 32
add_interface_port csr avs_csr_waitrequest waitrequest Output 1

add_interface pre_in avalon_streaming end
set_interface_property pre_in associatedClock clock
set_interface_property pre_in associatedReset reset
set_interface_property pre_in dataBitsPerSymbol 39
set_interface_property pre_in symbolsPerBeat 1
set_interface_property pre_in readyLatency 0
set_interface_property pre_in maxChannel 15
set_interface_property pre_in errorDescriptor ""
set_interface_property pre_in firstSymbolInHighOrderBits true
set_interface_property pre_in ENABLED true
add_interface_port pre_in asi_pre_data data Input 39
add_interface_port pre_in asi_pre_valid valid Input 1
add_interface_port pre_in asi_pre_ready ready Output 1
add_interface_port pre_in asi_pre_startofpacket startofpacket Input 1
add_interface_port pre_in asi_pre_endofpacket endofpacket Input 1
add_interface_port pre_in asi_pre_channel channel Input 4
add_interface_port pre_in asi_pre_empty empty Input 1
add_interface_port pre_in asi_pre_error error Input 1

add_interface pre_out avalon_streaming start
set_interface_property pre_out associatedClock clock
set_interface_property pre_out associatedReset reset
set_interface_property pre_out dataBitsPerSymbol 39
set_interface_property pre_out symbolsPerBeat 1
set_interface_property pre_out readyLatency 0
set_interface_property pre_out maxChannel 15
set_interface_property pre_out errorDescriptor ""
set_interface_property pre_out firstSymbolInHighOrderBits true
set_interface_property pre_out ENABLED true
add_interface_port pre_out aso_pre_data data Output 39
add_interface_port pre_out aso_pre_valid valid Output 1
add_interface_port pre_out aso_pre_ready ready Input 1
add_interface_port pre_out aso_pre_startofpacket startofpacket Output 1
add_interface_port pre_out aso_pre_endofpacket endofpacket Output 1
add_interface_port pre_out aso_pre_channel channel Output 4
add_interface_port pre_out aso_pre_empty empty Output 1
add_interface_port pre_out aso_pre_error error Output 1

add_interface post_in avalon_streaming end
set_interface_property post_in associatedClock clock
set_interface_property post_in associatedReset reset
set_interface_property post_in dataBitsPerSymbol 36
set_interface_property post_in symbolsPerBeat 1
set_interface_property post_in readyLatency 0
set_interface_property post_in firstSymbolInHighOrderBits true
set_interface_property post_in ENABLED true
add_interface_port post_in asi_post_data data Input 36
add_interface_port post_in asi_post_valid valid Input 1
add_interface_port post_in asi_post_ready ready Output 1
add_interface_port post_in asi_post_startofpacket startofpacket Input 1
add_interface_port post_in asi_post_endofpacket endofpacket Input 1

add_interface post_out avalon_streaming start
set_interface_property post_out associatedClock clock
set_interface_property post_out associatedReset reset
set_interface_property post_out dataBitsPerSymbol 36
set_interface_property post_out symbolsPerBeat 1
set_interface_property post_out readyLatency 0
set_interface_property post_out firstSymbolInHighOrderBits true
set_interface_property post_out ENABLED true
add_interface_port post_out aso_post_data data Output 36
add_interface_port post_out aso_post_valid valid Output 1
add_interface_port post_out aso_post_ready ready Input 1
add_interface_port post_out aso_post_startofpacket startofpacket Output 1
add_interface_port post_out aso_post_endofpacket endofpacket Output 1

add_interface hist_out avalon_streaming start
set_interface_property hist_out associatedClock clock
set_interface_property hist_out associatedReset reset
set_interface_property hist_out dataBitsPerSymbol 39
set_interface_property hist_out symbolsPerBeat 1
set_interface_property hist_out readyLatency 0
set_interface_property hist_out maxChannel 15
set_interface_property hist_out firstSymbolInHighOrderBits true
set_interface_property hist_out ENABLED true
add_interface_port hist_out aso_hist_data data Output 39
add_interface_port hist_out aso_hist_valid valid Output 1
add_interface_port hist_out aso_hist_ready ready Input 1
add_interface_port hist_out aso_hist_startofpacket startofpacket Output 1
add_interface_port hist_out aso_hist_endofpacket endofpacket Output 1
add_interface_port hist_out aso_hist_channel channel Output 4
