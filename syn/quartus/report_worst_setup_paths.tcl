set path_report_file "output_files/histogram_statistics_v2_standalone.worst_setup_paths.rpt"

set fh [open $path_report_file w]
puts $fh "histogram_statistics_v2_standalone worst setup path by operating condition"
puts $fh ""
close $fh

foreach_in_collection op [get_available_operating_conditions] {
    set_operating_conditions $op
    update_timing_netlist

    set op_name [get_operating_conditions_info $op -display_name]
    set op_model [get_operating_conditions_info $op -model]
    set op_voltage [get_operating_conditions_info $op -voltage]
    set op_temperature [get_operating_conditions_info $op -temperature]

    set fh [open $path_report_file a]
    puts $fh "Operating condition: $op_name"
    puts $fh "Model: $op_model, voltage: ${op_voltage}mV, temperature: ${op_temperature}C"
    puts $fh ""
    close $fh

    report_timing \
        -setup \
        -npaths 1 \
        -detail full_path \
        -from_clock i_clk \
        -to_clock i_clk \
        -file $path_report_file \
        -append

    set fh [open $path_report_file a]
    puts $fh ""
    close $fh
}

post_message "Wrote detailed setup paths to $path_report_file"
