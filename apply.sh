#!/usr/bin/env bash

FIRMWARE=/lib/firmware
DTBO=/home/j/fpga.dtbo
OVERLAY_NAME=test
CONFIGFS=/sys/kernel/config
OVERLAY=$CONFIGFS/device-tree/overlays/$OVERLAY_NAME

mkdir -p $FIRMWARE
cp /home/j/fpga.rbf $FIRMWARE
mkdir $OVERLAY
cat $DTBO > $CONFIGFS/device-tree/overlays/$OVERLAY_NAME/dtbo
