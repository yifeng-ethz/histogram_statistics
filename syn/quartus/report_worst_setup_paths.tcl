set path_report_file "output_files/histogram_statistics_v2_standalone.worst_setup_paths.rpt"

report_timing \
    -setup \
    -npaths 30 \
    -detail full_path \
    -from_clock i_clk \
    -to_clock i_clk \
    -file $path_report_file

post_message "Wrote detailed setup paths to $path_report_file"
