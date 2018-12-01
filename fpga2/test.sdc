create_clock -period 20 [get_ports clk_clk]
derive_pll_clocks
derive_clock_uncertainty

set_false_path -from [get_ports {sw_export[0]}] -to *
set_false_path -from [get_ports {sw_export[1]}] -to *
set_false_path -from [get_ports {sw_export[2]}] -to *
set_false_path -from [get_ports {sw_export[3]}] -to *
set_false_path -from * -to [get_ports {led_export[0]}]
set_false_path -from * -to [get_ports {led_export[1]}]
set_false_path -from * -to [get_ports {led_export[2]}]
set_false_path -from * -to [get_ports {led_export[3]}]
set_false_path -from * -to [get_ports {led_export[4]}]
set_false_path -from * -to [get_ports {led_export[5]}]
set_false_path -from * -to [get_ports {led_export[6]}]
set_false_path -from * -to [get_ports {led_export[7]}]
