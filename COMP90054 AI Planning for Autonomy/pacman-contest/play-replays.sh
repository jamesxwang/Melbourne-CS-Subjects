#!/bin/bash
########################################################## 
# 
# Play all the .replay files in a directory, one after 
# another 
# 
##########################################################

NO_ARGS=$#
if [[ "$NO_ARGS" < 1 ]]; then
  printf "usage: play-replays.sh <dir with replays>\n"
  exit 0
fi

for f in $(ls $1/*.replay) ; do
	x=`basename $f .replay`
	team1=$(echo "$x" | sed -e "s/\(.*\)_vs_\(.*\)_.*/\1/g")
	team2=$(echo "$x" | sed -e "s/\(.*\)_vs_\(.*\)_.*/\2/g")
	echo $team1
	echo $team2
	python capture.py -r $team1 -b $team2 --delay-step=0.01 --replay $f
done;

