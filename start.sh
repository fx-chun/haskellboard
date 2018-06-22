#!/bin/bash

./dist/build/haskellboard/haskellboard

while true; do
    sleep 3
    echo -e 'power on\nconnect 60\t \nquit' | bluetoothctl
done