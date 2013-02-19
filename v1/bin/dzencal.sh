#!/bin/bash

FONT="-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*"
CRIT="#99cc66"

TODAY=$(expr `date +'%d'` + 0)
MONTH=$(date +'%m')
YEAR=$(date +'%Y')

(
echo '^bg(#020202)^fg(#111111)'
echo '          ^fg(#444444)CALENDAR'

# current month, highlight header and today
cal -m | sed -r -e "1,2 s/.*/^fg(#3955c4)&^fg()/" -e "s/(^| )($TODAY)($| )/\1^bg()^fg($CRIT)\2^fg()^bg()\3/" -e "s/^/    /"

# next month, hilight header
[ $MONTH -eq 12 ] && YEAR=`expr $YEAR + 1`
cal -m `expr \( $MONTH + 1 \) % 12` $YEAR | sed -e "1,2 s/.*/^fg(#3955c4)&^fg()/" -e "s/^/    /"
) \
| dzen2 -p 60 -fn $FONT -x 1195 -y 17 -w 170 -l 18 -sa l -e 'onstart=uncollapse;button3=exit'
