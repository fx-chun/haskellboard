#!/bin/bash

./dist/build/haskellboard/haskellboard &

while true; do
    sleep 1;
    if [ ! -e /dev/input/event0 ]; then
        { echo -n; echo -e 'disconnect 60:6B:FF:67:18:57\n'; sleep 10; echo -e 'connect 60:6B:FF:67:18:57\n'; sleep 5; echo -e 'quit\n'; } | bluetoothctl; 
    fi
done