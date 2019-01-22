#!/usr/bin/env bash

FIRMWARE=/lib/firmware
DTBO=/home/j/test.dtbo
OVERLAY_NAME=test
CONFIGFS=/sys/kernel/config
OVERLAY=$CONFIGFS/device-tree/overlays/$OVERLAY_NAME

mkdir -p $FIRMWARE
cp /home/j/test.rbf $FIRMWARE
mkdir $OVERLAY
cat $DTBO > $CONFIGFS/device-tree/overlays/$OVERLAY_NAME/dtbo
