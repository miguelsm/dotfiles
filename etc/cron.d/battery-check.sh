#! /usr/bin/env nix-shell
#! nix-shell --pure -i bash --packages acpi bash libnotify

set -euxo pipefail

# https://unix.stackexchange.com/a/185400/173507

export DISPLAY=:0
XAUTHORITY=$HOME/.Xauthority

if [ -r "$HOME/.dbus/Xdbus" ]; then
    . "$HOME/.dbus/Xdbus"
fi

battery_level=`acpi -b | grep -P -o '[0-9]+(?=%)' | head -n 1`

if [ $battery_level -le 15 ]
then
    notify-send -u critical "Battery low" "Battery level is ${battery_level}%!"
fi
