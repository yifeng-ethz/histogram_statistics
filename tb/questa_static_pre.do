if {[catch {vlib altera_mf} msg]} { puts "vlib altera_mf: $msg" }
vmap altera_mf altera_mf
vcom -2008 -work altera_mf {/data1/intelFPGA_pro/23.1/quartus/eda/sim_lib/altera_mf_components.vhd}
vcom -2008 -work altera_mf {/data1/intelFPGA_pro/23.1/quartus/eda/sim_lib/altera_mf.vhd}
