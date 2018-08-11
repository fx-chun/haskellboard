[Haskellboard](https://lolc.at/assets/img/haskellboard/header.png)

Control an electric longboard using a Joy-Con on the Raspberry Pi.

## Features
- systemd service to maintain a connection to the Joy-Con
- variable speed
    - press `Trigger`+`Joystick Up` for normal (cruising) speed
    - press `Shoulder`+`Joystick Up` for fast speed. 
- ESC programming mode
    - press `DPad Right` to enter programming mode
        - hold `Dpad Up` for a full-throttle PWM output
        - hold `Dpad Left` for a half-throttle PWM output
        - hold `Dpad Down` for a zero PWM output
        - press `Dpad Right` to exit programming mode

## Building & Installing
1. clone to your Pi home directory
2. install wiringPi
3. `cabal install --only-dependencies`
4. `cabal build`
6. (optional) install the service by linking it into the appropriate directory
7. (optional) pair your Joy-Con in `bluetoothctl` and set your Joy-Con MAC address in `start.sh`

## Todos
- install script 
- factor into modules
- load config from a file