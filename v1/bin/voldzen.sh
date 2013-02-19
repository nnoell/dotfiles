#!/bin/bash

#Control
MIXER="Master"
STEP=5

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

if [ "$1" = "+" -o "$1" = "-" ]; then
	amixer set $MIXER $STEP%${1}
	if [[ $# == 2 && $2 == "-d" ]]; then
		Perc=$(amixer get Master | grep "Mono:" | awk '{print $4}' | tr -d '[]%')
		echo " ^fg($DZEN_FG2)VOL $(echo $Perc | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl) ^fg()$Perc%" | dzen2 $DZEN_ARGS
	fi
	exit 0
fi

if [ "$1" = "t" ]; then
	if amixer get Master | grep "\[on\]"; then
		amixer set $MIXER mute
		if [[ $# == 2 && $2 == "-d" ]]; then
			Perc=$(amixer get Master | grep "Mono:" | awk '{print $4}' | tr -d '[]%')
			echo " ^fg($DZEN_FG2)VOL $(echo $Perc | gdbar -fg $CRIT -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl) ^fg()off" | dzen2 $DZEN_ARGS
		fi
		exit 0
	else
		amixer set $MIXER unmute
		if [[ $# == 2 && $2 == "-d" ]]; then
			Perc=$(amixer get Master | grep "Mono:" | awk '{print $4}' | tr -d '[]%')
			echo " ^fg($DZEN_FG2)VOL $(echo $Perc | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl) ^fg()$Perc%" | dzen2 $DZEN_ARGS
		fi
		exit 0
	fi
fi

exit -1
