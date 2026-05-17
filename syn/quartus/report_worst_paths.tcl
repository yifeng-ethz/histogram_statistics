project_open histogram_statistics_v2_signoff -revision histogram_statistics_v2_standalone
create_timing_netlist -model slow
read_sdc
update_timing_netlist
report_timing -setup -npaths 10 -detail full_path -file output_files/worst_setup_paths.rpt
report_timing -hold -npaths 3 -detail summary -file output_files/worst_hold_paths.rpt
project_close
