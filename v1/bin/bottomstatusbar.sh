#!/bin/bash
# Author: nnoell <nnoell3@gmail.com>
# Depends: dzen2-xft-xpm-xinerama-svn
# Desc: dzen2 bar for XMonad, ran within xmonad.hs via spawnPipe

#Layout
BAR_H=9
BIGBAR_W=65
WIDTH=410
HEIGHT=16
X_POS=956
Y_POS=752

#Colors and font
FONT="-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*"
DZEN_BG="#020202"
DZEN_FG="#9d9d9d"
DZEN_FG2="#444444"
CRIT="#99cc66"
BAR_FG="#3955c4"
BAR_BG="#363636"
COLOR_SEP=$DZEN_FG2

#Options
INTERVAL=5
WIFISIGNAL=0

printDiskInfo() {
	RFSP=$(df -h / | tail -1 | awk '{ print $5 }' | tr -d '%')
	HFSP=$(df -h /home | tail -1 | awk '{ print $5 }' | tr -d '%')
	echo -n "^fg($DZEN_FG2) ROOT ^fg($BAR_FG)${RFSP}% "
	echo -n "^fg($DZEN_FG2)HOME ^fg($BAR_FG)${HFSP}%"
	return
}

printBattery() {
	BatPresent=$(acpi -b | wc -l)
	ACPresent=$(acpi -a | grep -c on-line)
	if [[ $BatPresent == "0" ]]; then
		echo -n "^fg($DZEN_FG2)AC ^fg($BAR_FG)on ^fg($DZEN_FG2)BAT ^fg($BAR_FG)off"
		return
	else
		RPERC=$(acpi -b | awk '{print $4}' | tr -d "%,")
		echo -n "^fg($DZEN_FG2)BAT "
		if [[ $ACPresent == "1" ]]; then
			echo -n "$(echo $RPERC | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl)"
		else
			echo -n "$(echo $RPERC | gdbar -fg $CRIT -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl)"
		fi
		echo -n " ^fg($DZEN_FG2)$RPERC%"
	fi
	return
}

printBrightnessInfo() {
	BRIFILE=$(cat /sys/class/backlight/acpi_video0/actual_brightness)
	Bri=$(expr $BRIFILE \* 10)
	echo -n "^fg($DZEN_FG2)BRI ^fg($BAR_FG)${Bri}%"
	return
}

printWifiInfo() {
	WIFIDOWN=$(wicd-cli --wireless -d | wc -l)
	WIFISIGNAL=0
#	[[ $WIFIDOWN -ne "1" ]] && WIFISIGNAL=$(wicd-cli --wireless -d | grep Quality | awk '{print $2}')
	echo -n "^fg($DZEN_FG2)WIFI "
	if [[ $WIFIDOWN -ne "1" ]]; then
		WIFISIGNAL=$(wicd-cli --wireless -d | grep Quality | awk '{print $2}')
		echo -n "$(echo $WIFISIGNAL | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl) "
		echo -n "^fg()$WIFISIGNAL% "
	else
		echo -n "^fg($CRIT)N/A "
	fi
	return
}

printSpace() {
	echo -n " ^fg($COLOR_SEP)|^fg() "
	return
}

printBottomBar() {
	while true; do
		printDiskInfo
		printSpace
		printBattery
		printSpace
		printBrightnessInfo
		printSpace
		printWifiInfo
		echo
		sleep $INTERVAL
	done
	return
}

#Print all and pipe into dzen2
printBottomBar | dzen2 -x $X_POS -y $Y_POS -w $WIDTH -h $HEIGHT -fn $FONT -ta 'r' -bg $DZEN_BG -fg $DZEN_FG -p -e ''
#tray
