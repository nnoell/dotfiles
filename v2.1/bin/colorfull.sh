#!/bin/bash
echo; for i in {000..007}; do echo -en "\e[0m \e[38;5;"$i"m"$i"\e[7;38;5;"$i"m"$i ; done; echo
for i in {008..015}; do echo -en "\e[0m \e[38;5;"$i"m"$i"\e[7;38;5;"$i"m"$i ; done; echo; echo
let count=0
for i in {016..231}; do
	echo -en "\e[0m \e[38;5;"$i"m"$i"\e[7;38;5;"$i"m"$i
	let count=$count+1
	if [[ $count -gt 5 ]]; then let count=0; echo; fi
done
echo; for i in {232..239}; do echo -en "\e[0m \e[38;5;"$i"m"$i"\e[7;38;5;"$i"m"$i ; done; echo
for i in {240..247}; do echo -en "\e[0m \e[38;5;"$i"m"$i"\e[7;38;5;"$i"m"$i ; done; echo
for i in {248..255}; do echo -en "\e[0m \e[38;5;"$i"m"$i"\e[7;38;5;"$i"m"$i ; done; echo -e "\033[0m"; echo
