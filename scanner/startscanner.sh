#!/bin/sh
#sleep 30


if [ -z "$EVENT_DEV" ]; then
    sleep 1
    exit 1
fi
SNANNER_DEV=$(jq -r '.snapperDev' /var/snap/m1tfd1/current/config.json)
echo "sudo $SNAP/usr/local/bin/scanner -e $SNANNER_DEV"
sudo $SNAP/usr/local/bin/scanner -e $SNANNER_DEV
