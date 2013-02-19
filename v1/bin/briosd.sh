#!/bin/sh
brightness=`cat /sys/class/backlight/acpi_video0/actual_brightness`
brillo=`expr $brightness "*" 100 "/" 9`
notify-send " " -i notification-display-brightness-low -h int:value:$brillo -h string:x-canonical-private-synchronous:brightness &
