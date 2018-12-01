#!/usr/bin/env bash

set -e

n=test

qsys-generate --synthesis=verilog top.qsys
quartus_map "$n"
quartus_cdb -merge "$n"
quartus_fit "$n"
quartus_sta "$n"
quartus_asm "$n"
quartus_cpf -c -o bitstream_compression=on "output_files/$n.sof" fpga.rbf

sopc2dts \
  --input top.sopcinfo \
  --output fpga.dts \
  --type dts \
  --bridge-removal all \
  --bridge-ranges bridge \
  --overlay-target "/soc/base-fpga-region" \
  --pov hps_0_bridges \
  --pov-type overlay \
  --no-timestamp \
  --firmware-name fpga.rbf \
  --verbose

sed -i 's|/dts-v1/ /plugin/|/dts-v1/;\n/plugin/|' fpga.dts

dtc --out-format dtb --out fpga.dtbo fpga.dts --symbols
