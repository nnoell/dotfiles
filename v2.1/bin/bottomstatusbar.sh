#!/bin/bash
# Author: nnoell <nnoell3@gmail.com>
# Depends: dzen2-xft-xpm-xinerama-svn && trayer
# Desc: dzen2 bar for XMonad, ran within xmonad.hs via spawnPipe

#Layout
XRES=1366
YRES=768
PANELBOXHEIGHT=12
HEIGHT=16
X_POS=946
BAR_H=9

#Colors and font
FONT="-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*"
DZEN_BG="#020202"
DZEN_BG2="#101010"
DZEN_FG="#9d9d9d"
DZEN_FG2="#444444"
CRIT="#66ff66" #green
BAR_FG="#44aacc" #blue
BAR_BG="#363636"
COLOR_SEP=$DZEN_FG2

#Options
ICONPATH="${HOME}/.icons/xbm_icons/subtle/"
INTERVAL=5
WIFISIGNAL=0

textBox() {
	echo -n "^fg("$3")^i("$ICONPATH"boxleft.xbm)^ib(1)^r("$XRES"x"$PANELBOXHEIGHT")^p(-"$XRES")^fg("$2")"$1"^fg("$3")^i("$ICONPATH"boxright.xbm)^fg("$4")^r("$XRES"x"$PANELBOXHEIGHT")^p(-"$XRES")^fg()^ib(0)"
}

printDiskInfo() {
	local RFSP=$(df -h / | tail -1 | awk '{ print $5 }' | tr -d '%')
	local HFSP=$(df -h /home | tail -1 | awk '{ print $5 }' | tr -d '%')
	textBox "ROOT ^fg($BAR_FG)"${RFSP}"% ^fg($DZEN_FG2)HOME ^fg($BAR_FG)${HFSP}%" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG}
	return
}

printBattery() {
	local BatPresent=$(acpi -b | wc -l)
	local ACPresent=$(acpi -a | grep -c on-line)
	if [[ $BatPresent == "0" ]]; then
		textBox "AC ^fg($BAR_FG)on ^fg($DZEN_FG2)BAT ^fg($CRIT)off" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG}
		return
	else
		RPERC=$(acpi -b | awk '{print $4}' | tr -d "%,")
		if [[ $ACPresent == "1" ]]; then
			textBox "BATTERY $RPERC%" ${CRIT} ${DZEN_BG2} ${DZEN_BG}
		else
			textBox "BATTERY $RPERC%" ${DZEN_BG} ${CRIT} ${DZEN_BG}
		fi
	fi
	return
}

printBrightnessInfo() {
	local BRIVALUE=$(expr $(expr $(cat /sys/class/backlight/acpi_video0/actual_brightness) \* 100) / 15)
	textBox "BRIGNESS ^fg($BAR_FG)${BRIVALUE}%" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG}
	return
}

printWifiInfo() {
	local WIFIDOWN=$(wicd-cli --wireless -d | wc -l)
	if [[ $WIFIDOWN -ne "1" ]]; then
		WIFISIGNAL=$(wicd-cli --wireless -d | grep Quality | awk '{print $2}')
		textBox "WIFI ^fg($CRIT)$WIFISIGNAL%" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG}
	else
		textBox "WIFI ^fg($CRIT)N/A " ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG}
	fi
	return
}

printBottomBar() {
	while true; do
		printDiskInfo
		printBrightnessInfo
		printWifiInfo
		printBattery
		echo
		sleep $INTERVAL
	done
	return
}

if [[ $# -ge 5 ]]; then
	XRES=$1
	YRES=$2
	PANELBOXHEIGHT=$3
	HEIGHT=$4
	X_POS=$5
fi

Y_POS=$(expr $YRES - $HEIGHT)
WIDTH=$(expr $XRES - $X_POS)

#Print all and pipe into dzen2
printBottomBar | dzen2 -x $X_POS -y $Y_POS -w $WIDTH -h $HEIGHT -fn $FONT -ta 'r' -bg $DZEN_BG -fg $DZEN_FG -p -e 'onstart=lower'
