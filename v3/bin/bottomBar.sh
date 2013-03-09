#!/bin/bash
# Author: nnoell <nnoell3[at]gmail.com>
# Depends: dzen2-xft-xpm-xinerama-svn && conky
# Desc: dzen2 bar for XMonad, ran within xmonad.hs via spawnPipe

#Default flags
XRES=1366
YRES=768
PANELBOXHEIGHT=12
HEIGHT=16
WIDTH_L=380

#Colors, fonts and paths
CRIT="#66ff66" #green
CRIT2="#e0105f" #red
BAR_FG="#44aacc" #blue
BAR_BG="#363636"
DZEN_FG="#9d9d9d"
DZEN_FG2="#444444"
DZEN_BG="#020202"
DZEN_BG2="#101010"
FONT="-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*"
LEFTBOXICON="${HOME}/.icons/xbm_icons/subtle/boxleft.xbm"
RIGHTBOXICON="${HOME}/.icons/xbm_icons/subtle/boxright.xbm"
CONKYFILE="${HOME}/.config/conky/conkyXmonad"

#Default values
IFS='|'
INTERVAL=1
CPUTemp=0
GPUTemp=0
CPULoad0=0
CPULoad1=0
CPULoad2=0
CPULoad3=0
X_POS_L=0
Y_POS=0

#==========================================
#FUNCTIONS
#==========================================

textBox() {
	echo -n "^fg("$3")^i("$LEFTBOXICON")^ib(1)^r("$XRES"x"$PANELBOXHEIGHT")^p(-"$XRES")^fg("$2")"$1"^fg("$3")^i("$RIGHTBOXICON")^fg("$4")^r("$XRES"x"$PANELBOXHEIGHT")^p(-"$XRES")^fg()^ib(0)"
}

printVolInfo() {
	local Perc=$(amixer get Master | grep "Mono:" | awk '{print $4}' | tr -d '[]%')
	local Mute=$(amixer get Master | grep "Mono:" | awk '{print $6}')
	if [[ $Mute == "[off]" ]]; then
		textBox "^ca(1,$VOL_TOGGLE_CMD)^ca(4,$VOL_UP_CMD)^ca(5,$VOL_DOWN_CMD)VOLUME ^fg(${DZEN_FG2})${Perc}%^ca()^ca()^ca()" ${DZEN_FG2} ${CRIT2} ${DZEN_BG}
	else
		textBox "^ca(1,$VOL_TOGGLE_CMD)^ca(4,$VOL_UP_CMD)^ca(5,$VOL_DOWN_CMD)VOLUME ^fg(${CRIT2})${Perc}%^ca()^ca()^ca()" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG}
	fi
	return
}

printCPUInfo() {
	[[ $CPULoad0 -gt 70 ]] && CPULoad0="^fg($CRIT)$CPULoad0^fg()"
	[[ $CPULoad1 -gt 70 ]] && CPULoad1="^fg($CRIT)$CPULoad1^fg()"
	[[ $CPULoad2 -gt 70 ]] && CPULoad2="^fg($CRIT)$CPULoad2^fg()"
	[[ $CPULoad3 -gt 70 ]] && CPULoad3="^fg($CRIT)$CPULoad3^fg()"
	local VALUE=$(textBox "^fg($BAR_FG)${CPULoad0}%^fg($DZEN_FG2)/^fg($BAR_FG)${CPULoad1}%^fg($DZEN_FG2)/^fg($BAR_FG)${CPULoad2}%^fg($DZEN_FG2)/^fg($BAR_FG)${CPULoad3}%" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG})
	local LABEL=$(textBox "CPU" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG})
	echo -n ${LABEL}${VALUE}
	return
}

printTempInfo() {
	local CPUTemp0=$(acpi --thermal  | grep "0:" | awk '{print substr($4,0,2)}')
	local CPUTemp1=$(acpi --thermal  | grep "1:" | awk '{print substr($4,0,2)}')
	[[ $CPUTemp0 -gt 70 ]] && CPUTemp0="^fg($CRIT)$CPUTemp0^fg()"
	[[ $CPUTemp1 -gt 70 ]] && CPUTemp1="^fg($CRIT)$CPUTemp1^fg()"
	local VALUE=$(textBox "^fg($BAR_FG)${CPUTemp0}°^fg($DZEN_FG2)/^fg($BAR_FG)${CPUTemp1}°" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG})
	local LABEL=$(textBox "TEMP" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG})
	echo -n ${LABEL}${VALUE}
	return
}

printMemInfo() {
	[[ $MemPerc -gt 70 ]] && MemTemp="^fg($CRIT)$MemPerc^fg()"
	local VALUE=$(textBox "^fg($BAR_FG)${MemPerc}%^fg($DZEN_FG2)/^fg($BAR_FG)${MemUsed}" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG})
	local LABEL=$(textBox "MEM" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG})
	echo -n ${LABEL}${VALUE}
	return
}

printBatteryInfo() {
	local BATStatus=$([ -f /sys/class/power_supply/BAT0/status ] && /usr/bin/cat /sys/class/power_supply/BAT0/status || echo 'AC Conection')
	local BATCap=$([ -f /sys/class/power_supply/BAT0/capacity ] && $(/usr/bin/cat /sys/class/power_supply/BAT0/capacity)"%" || echo 'N/A')
	local VALUEC=$(textBox "^fg($CRIT)${BATStatus}" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG})
	local VALUE=$(textBox "^fg($BAR_FG)${BATCap}" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG})
	local LABEL=$(textBox "BATTERY" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG})
	echo -n ${LABEL}${VALUE}${VALUEC}
}

printWifiInfo() {
	local WIFISignal="N/A"
	if [[ $(/usr/sbin/iwconfig wlan0 | grep -c 'ESSID:off/any') == "0" ]]; then
		WIFISignal=$(/usr/sbin/iwconfig wlan0 | awk -F '=' '/Quality/ {print $2}' | cut -d '/' -f 1)"%"
	fi
	local VALUE=$(textBox "^fg($BAR_FG)${WIFISignal}" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG})
	local LABEL=$(textBox "WIFI" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG})
	echo -n ${LABEL}${VALUE}${VALUEC}
}

printRight() {
	while true; do
		read CPULoad0 CPULoad1 CPULoad2 CPULoad3 CPUFreq MemUsed MemPerc
		printCPUInfo
		echo -n " "
		printMemInfo
		echo -n " "
		printTempInfo
		echo -n " "
		printWifiInfo
		echo -n " "
		printBatteryInfo
		echo
	done
	return
}


#==========================================
#MAIN
#==========================================

#Print all and pipe into dzen
conky -c $CONKYFILE -u $INTERVAL | printRight | dzen2 -x 380 -y 752 -w 986 -h $HEIGHT -fn $FONT -ta 'r' -bg $DZEN_BG -fg $DZEN_FG -p -e 'onstart=lower'
