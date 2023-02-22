#!/bin/sh

while [ 1 ]; do

/usr/bin/node ./bin/icttest.js -w true -s 8351897 -l /home/lbuchman/logs/ -d /dev/teensylink -m /dev/m1term -f /home/lbuchman/fw/fsbl.stm32

if [ "$?" = "17" ]; then 
    exit;
fi


done
