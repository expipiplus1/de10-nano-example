# TCL File Generated by Component Editor 18.1
# Thu Jan 01 07:30:01 SGT 1970
# DO NOT MODIFY


# 
# uart_test "uart_test" v1.0
#  1970.01.01.07:30:01
# 
# 

# 
# request TCL package from ACDS 16.1
# 
package require -exact qsys 16.1


# 
# module uart_test
# 
set_module_property DESCRIPTION ""
set_module_property NAME uart_test
set_module_property VERSION 1.0
set_module_property INTERNAL false
set_module_property OPAQUE_ADDRESS_MAP true
set_module_property AUTHOR ""
set_module_property DISPLAY_NAME uart_test
set_module_property INSTANTIATE_IN_SYSTEM_MODULE true
set_module_property EDITABLE true
set_module_property REPORT_TO_TALKBACK false
set_module_property ALLOW_GREYBOX_GENERATION false
set_module_property REPORT_HIERARCHY false


# 
# file sets
# 
add_fileset QUARTUS_SYNTH QUARTUS_SYNTH "" ""
set_fileset_property QUARTUS_SYNTH TOP_LEVEL uart_test
set_fileset_property QUARTUS_SYNTH ENABLE_RELATIVE_INCLUDE_PATHS false
set_fileset_property QUARTUS_SYNTH ENABLE_FILE_OVERWRITE_MODE false
add_fileset_file empty.v VERILOG PATH empty.v TOP_LEVEL_FILE


# 
# parameters
# 


# 
# display items
# 


# 
# connection point loan_io
# 
add_interface loan_io conduit end
set_interface_property loan_io associatedClock ""
set_interface_property loan_io associatedReset ""
set_interface_property loan_io ENABLED true
set_interface_property loan_io EXPORT_OF ""
set_interface_property loan_io PORT_NAME_MAP ""
set_interface_property loan_io CMSIS_SVD_VARIABLES ""
set_interface_property loan_io SVD_ADDRESS_GROUP ""

add_interface_port loan_io loan_in in Input 67
add_interface_port loan_io loan_out out Output 67
add_interface_port loan_io loan_oe oe Output 67


# 
# connection point boot_from_fpga
# 
add_interface boot_from_fpga conduit end
set_interface_property boot_from_fpga associatedClock ""
set_interface_property boot_from_fpga associatedReset ""
set_interface_property boot_from_fpga ENABLED true
set_interface_property boot_from_fpga EXPORT_OF ""
set_interface_property boot_from_fpga PORT_NAME_MAP ""
set_interface_property boot_from_fpga CMSIS_SVD_VARIABLES ""
set_interface_property boot_from_fpga SVD_ADDRESS_GROUP ""

add_interface_port boot_from_fpga boot_from_fpga_on_failure boot_from_fpga_on_failure Output 1
add_interface_port boot_from_fpga boot_from_fpga_ready boot_from_fpga_ready Output 1


# 
# connection point clock_clk
# 
add_interface clock_clk clock end
set_interface_property clock_clk clockRate 32000000
set_interface_property clock_clk ENABLED true
set_interface_property clock_clk EXPORT_OF ""
set_interface_property clock_clk PORT_NAME_MAP ""
set_interface_property clock_clk CMSIS_SVD_VARIABLES ""
set_interface_property clock_clk SVD_ADDRESS_GROUP ""

add_interface_port clock_clk clock_clk clk Input 1


# 
# connection point reset_reset
# 
add_interface reset_reset reset end
set_interface_property reset_reset associatedClock clock_clk
set_interface_property reset_reset synchronousEdges DEASSERT
set_interface_property reset_reset ENABLED true
set_interface_property reset_reset EXPORT_OF ""
set_interface_property reset_reset PORT_NAME_MAP ""
set_interface_property reset_reset CMSIS_SVD_VARIABLES ""
set_interface_property reset_reset SVD_ADDRESS_GROUP ""

add_interface_port reset_reset reset_reset reset Input 1


# 
# connection point led_export
# 
add_interface led_export conduit end
set_interface_property led_export associatedClock clock_clk
set_interface_property led_export associatedReset ""
set_interface_property led_export ENABLED true
set_interface_property led_export EXPORT_OF ""
set_interface_property led_export PORT_NAME_MAP ""
set_interface_property led_export CMSIS_SVD_VARIABLES ""
set_interface_property led_export SVD_ADDRESS_GROUP ""

add_interface_port led_export led_export pio Output 8
