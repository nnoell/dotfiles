#!/bin/bash

#Control
BRIFILE=$(cat /sys/class/backlight/acpi_video0/actual_brightness)

#Layout
DZEN_SEC=2
DZEN_Y="17"
DZEN_X="540"
DZEN_H="16"
DZEN_W="200"
DZEN_TA="l"
BAR_H=9
BIGBAR_W=140

#Colors and font
CRIT="#99cc66"
BAR_FG="#3955c4"
BAR_BG="#363636"
DZEN_FG="#9d9d9d"
DZEN_FG2="#444444"
DZEN_BG="#020202"
DZEN_FONT="-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*"

DZEN_ARGS="-fn $DZEN_FONT -p $DZEN_SEC -ta $DZEN_TA -y ${DZEN_Y} -x ${DZEN_X} -w $DZEN_W -h $DZEN_H  -bg $DZEN_BG -fg $DZEN_FG -e ''"


Perc=$(expr $BRIFILE \* 10)
echo " ^fg($DZEN_FG2)BRI $(echo $Perc | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl) ^fg()$Perc%" | dzen2 $DZEN_ARGS
exit 0
