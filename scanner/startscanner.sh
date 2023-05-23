#!/bin/sh
#sleep 30
EVENT_DEV=$(cat /proc/bus/input/devices | awk '/SCANNER/{for(a=0;a>=0;a++){getline;{if(/kbd/==1){ print $NF;exit 0;}}}}')

if [ -z "$EVENT_DEV" ]; then
    EVENT_DEV=$(cat /proc/bus/input/devices | awk '/Scanner/{for(a=0;a>=0;a++){getline;{if(/kbd/==1){ print $4;exit 0;}}}}')
fi

if [ -z "$EVENT_DEV" ]; then
    sleep 1
    exit 1
fi

echo "sudo $SNAP/usr/local/bin/scanner -e /dev/input/$EVENT_DEV"
sudo $SNAP/usr/local/bin/scanner -e /dev/input/$EVENT_DEV
