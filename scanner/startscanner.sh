#!/bin/sh
#sleep 30


// cat /proc/bus/input/devices
SNANNER_DEV=$(jq -r '.scannerDev' /var/snap/m1tfd1/current/config.json)
echo "sudo $SNAP/usr/local/bin/scanner -e $SNANNER_DEV"
sudo $SNAP/usr/local/bin/scanner -e $SNANNER_DEV
