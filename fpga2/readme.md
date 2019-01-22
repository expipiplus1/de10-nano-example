
## Getting UART working notes

Table 3-17 Pin Assignment of UART Interface
| Signal Name    | FPGA Pin No. | Description          | I/O Standard |
|----------------|--------------|----------------------|--------------|
| HPS_UART_RX    | PIN_A22      | HPS UART Receiver    | 3.3V         |
| HPS_UART_TX    | PIN_B21      | HPS UART Transmitter | 3.3V         |
| HPS_CONV_USB_N | PIN_C6       | Reserve              | 3.3V         |

Table 3-14 Pin Assignment of LEDs, Switches and Push-buttons
| Signal Name | FPGA Pin No | HPS GPIO | Register/bit | Function |
|-------------|-------------|----------|--------------|----------|
| HPS_LED     | PIN_A20     | GPIO53   | GPIO1[24]    | I/O      |
| HPS_KEY     | PIN_J18     | GPIO54   | GPIO1[25]    | I/O      |

## Useful links

- https://github.com/intel-iot-devkit/terasic-de10-nano-kit
  - Good example on setting up the hps qsys component

## TODO:

Altera_pll often fails to generate with some error about $TMP not being
writeable. To fix this remove the `sopc_altera_pll*` directory left over from
the previous build.

## BSP generation

```
bsp-create-settings --settings bsp/bsp.settings --type spl --bsp-dir bsp --preloader-settings-dir hps_isw_handoff/top_hps_0 --set spl.boot.EXE_ON_FPGA 1
bsp-generate-files --bsp-dir bsp --settings bsp/bsp.settings
cd bsp
/nix/store/71a4yijrl2rvrk6zl2ci98lm51dfzx23-SoCEDS/embedded/embedded_command_shell.sh
make
arm-altera-eabi-objcopy -O ihex --adjust-vma -0xc0000000 uboot-socfpga/spl/u-boot-spl ../preloader.hex
```
