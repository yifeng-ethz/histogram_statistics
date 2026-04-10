# Standalone timing signoff for histogram_statistics_v2
# Target: 125 MHz (8.0 ns) with 10% margin -> 137.5 MHz (7.273 ns)

create_clock -name i_clk -period 7.273 [get_ports {i_clk}]

set_false_path -from [remove_from_collection [all_inputs] [get_ports {i_clk}]]
set_false_path -to [all_outputs]
