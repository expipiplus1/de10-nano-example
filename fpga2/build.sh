#!/usr/bin/env bash

set -e

qsys-generate --synthesis=verilog top.qsys
quartus_map test
quartu_cdb -merge test
quartus_fit test
quartus_tan test
quartus_asm test
