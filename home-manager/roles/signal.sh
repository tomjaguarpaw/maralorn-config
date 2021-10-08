#!/usr/bin/env bash

x=$1

while true; do

if [[ "$x" == "0" ]]; then
	exit
fi

if [[ "$x" != "" ]]; then
echo "$x rings left"
x=$(( x - 1 ))
fi

current_seconds=$(date +%-S)
current_minute=$(date +%-M)

if (($current_minute < 30)); then
echo "Next ring at half hour."
sleep_seconds=$(( (29 - $current_minute)*60+ (60 - $current_seconds)))
else
echo "Next ring at full hour."
sleep_seconds=$(( (59 - $current_minute)*60+ (60 - $current_seconds)))
fi

echo "Sleeping for $sleep_seconds seconds."
sleep $sleep_seconds

echo "Ringing Bell"
mpv $HOME/media/audio/bell.opus
done
