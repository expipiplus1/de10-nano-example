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

