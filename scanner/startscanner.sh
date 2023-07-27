#!/bin/sh
#sleep 30


if [ -z "$EVENT_DEV" ]; then
    sleep 1
    exit 1
fi
echo "sudo $SNAP/usr/local/bin/scanner -e /dev/input/$EVENT_DEV"
sudo $SNAP/usr/local/bin/scanner -e /dev/input/$EVENT_DEV
