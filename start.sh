#!/bin/bash

./dist/build/haskellboard/haskellboard &

while true; do
    sleep 2;
    if [ ! -f /dev/input/event0 ]; then
        { echo -n; echo -e 'power on\n'; sleep 1; echo -e 'connect \t\n'; sleep 1; echo -e 'quit\n'; } | bluetoothctl; 
    fi
done