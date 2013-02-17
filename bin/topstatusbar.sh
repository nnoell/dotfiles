#!/bin/bash
# Author: nnoell <nnoell3@gmail.com>
# Depends: dzen2-xft-xpm-xinerama-svn && conky
# Desc: dzen2 bar for XMonad, ran within xmonad.hs via spawnPipe

#Default flags
XRES=1366
YRES=768
PANELBOXHEIGHT=12
HEIGHT=16
WIDTH_L=910 #right alignment

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
ICONPATH="${HOME}/.icons/xbm_icons/subtle/"
CONKYFILE="${HOME}/.config/conky/conkyrc"

#Default values
IFS='|'
INTERVAL=1
CPUTemp=0
GPUTemp=0
CPULoad0=0
CPULoad1=0
CPULoad2=0
CPULoad3=0
MpdInfo=0
MpdRandom="Off"
MpdRepeat="Off"
X_POS_L=0
Y_POS=0

#clickable areas
VOL_TOGGLE_CMD="sh ${HOME}/bin/voldzen.sh t"
VOL_UP_CMD="sh ${HOME}/bin/voldzen.sh +"
VOL_DOWN_CMD="sh ${HOME}/bin/voldzen.sh -"
DROP_START_CMD="dropbox start"
DROP_STOP_CMD="dropbox stop"
MPD_REP_CMD="mpc -h 127.0.0.1 repeat"
MPD_RAND_CMD="mpc -h 127.0.0.1 random"
MPD_TOGGLE_CMD="ncmpcpp toggle"
MPD_STOP_CMD="ncmpcpp stop"
MPD_NEXT_CMD="ncmpcpp next"
MPD_PREV_CMD="ncmpcpp prev"
CAL_CMD="sh ${HOME}/bin/dzencal.sh"


#==========================================
#FUNCTIONS
#==========================================

textBox() {
	echo -n "^fg("$3")^i("$ICONPATH"boxleft.xbm)^ib(1)^r("$XRES"x"$PANELBOXHEIGHT")^p(-"$XRES")^fg("$2")"$1"^fg("$3")^i("$ICONPATH"boxright.xbm)^fg("$4")^r("$XRES"x"$PANELBOXHEIGHT")^p(-"$XRES")^fg()^ib(0)"
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
	textBox "CPU ^fg($BAR_FG)${CPULoad0}%^fg($DZEN_FG2)/^fg($BAR_FG)${CPULoad1}%^fg($DZEN_FG2)/^fg($BAR_FG)${CPULoad2}%^fg($DZEN_FG2)/^fg($BAR_FG)${CPULoad3}%" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG}
	return
}

printTempInfo() {
	local CPUTemp0=$(acpi --thermal  | grep "0:" | awk '{print substr($4,0,2)}')
	local CPUTemp1=$(acpi --thermal  | grep "1:" | awk '{print substr($4,0,2)}')
	[[ $CPUTemp0 -gt 70 ]] && CPUTemp0="^fg($CRIT)$CPUTemp0^fg()"
	[[ $CPUTemp1 -gt 70 ]] && CPUTemp1="^fg($CRIT)$CPUTemp1^fg()"
	textBox "TEMP ^fg($BAR_FG)${CPUTemp0}°^fg($DZEN_FG2)/^fg($BAR_FG)${CPUTemp1}°" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG}
	return
}

printMemInfo() {
	[[ $MemPerc -gt 70 ]] && CPUTemp="^fg($CRIT)$MemPerc^fg()"
	textBox "MEM ^fg($BAR_FG)${MemPerc}%" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG}
	return
}

printDropBoxInfo() {
	if [[ $(ps -A | grep -c dropbox) == "0" ]]; then
		textBox "^ca(1,$DROP_START_CMD)DROPBOX ^fg()Off^ca()" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG}
	else
		textBox "^ca(1,$DROP_STOP_CMD)DROPBOX ^fg($CRIT)On^ca()" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG}
	fi
	return
}

printMpdInfo() {
	if [[ $(ps -A | grep -c mpd) == "0" ]]; then
		textBox "^ca(1,mpd)MPD^ca() ^fg()Off" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG}
	else
		[[ $MpdRepeat == "On" ]] && MpdRepeat="^fg($CRIT)$MpdRepeat^fg()"
		[[ $MpdRandom == "On" ]] && MpdRandom="^fg($CRIT)$MpdRandom^fg()"
		textBox "^ca(1,$MPD_REP_CMD)REPEAT^ca() ^fg()$MpdRepeat" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG}
		textBox "^ca(1,$MPD_RAND_CMD)RANDOM^ca() ^fg()$MpdRandom" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG}
		textBox "^ca(1,$MPD_TOGGLE_CMD)^ca(3,$MPD_STOP_CMD)^ca(4,$MPD_NEXT_CMD)^ca(5,$MPD_PREV_CMD)MPD $MpdInfo^ca()^ca()^ca()^ca()" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG}
	fi
	return
}

printDateInfo() {
	textBox "^ca(1,$CAL_CMD)^fg()$(date '+%Y^fg(#444).^fg()%m^fg(#444).^fg()%d^fg(#444)/^fg(#e0105f)%a ^fg(#44aacc)%H^fg(#444):^fg(#44aacc)%M^fg(#444):^fg(#44aacc)%S')^ca()" ${DZEN_FG2} ${DZEN_BG2} ${DZEN_BG}
	return
}

printLeft() {
	while true; do
		read CPULoad0 CPULoad1 CPULoad2 CPULoad3 CPUFreq MemUsed MemPerc MpdInfo MpdRandom MpdRepeat
		printVolInfo
		printDropBoxInfo
		printMpdInfo
		echo
	done
	return
}

printRight() {
	while true; do
		read CPULoad0 CPULoad1 CPULoad2 CPULoad3 CPUFreq MemUsed MemPerc MpdInfo MpdRandom MpdRepeat
		printCPUInfo
		printMemInfo
		printTempInfo
		printDateInfo
		echo
	done
	return
}


#==========================================
#MAIN
#==========================================

#Flags
if [[ $# -ge 5 ]]; then
	XRES=$1
	YRES=$2
	PANELBOXHEIGHT=$3
	HEIGHT=$4
	WIDTH_L=$5
fi
WIDTH_R=$(expr $XRES - $WIDTH_L) #WIDTH_L + WIDTH_R = 1366
X_POS_R=$WIDTH_L

#Print all and pipe into dzen
conky -c $CONKYFILE -u $INTERVAL | printLeft | dzen2 -x $X_POS_L -y $Y_POS -w $WIDTH_L -h $HEIGHT -fn $FONT -ta 'l' -bg $DZEN_BG -fg $DZEN_FG -p -e 'onstart=lower' &
conky -c $CONKYFILE -u $INTERVAL | printRight | dzen2 -x $X_POS_R -y $Y_POS -w $WIDTH_R -h $HEIGHT -fn $FONT -ta 'r' -bg $DZEN_BG -fg $DZEN_FG -p -e 'onstart=lower'
