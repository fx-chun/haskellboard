#!/bin/bash

./dist/build/haskellboard/haskellboard &

while true; do
    sleep 1;
    { echo -n; echo -e 'power on\n'; sleep 1; echo -e 'connect \t\n'; sleep 1; echo -e 'quit\n'; } | bluetoothctl; 
done