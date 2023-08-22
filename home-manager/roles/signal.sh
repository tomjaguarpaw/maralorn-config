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

if ((current_minute < 25)); then
mpv "$HOME/media/audio/bell.opus"
echo "Next ring at 25."
sleep_seconds=$(( (24 - current_minute)*60+ (60 - current_seconds)))
elif ((current_minute < 30)); then
echo "Next ring at 30."
sleep_seconds=$(( (29 - current_minute)*60+ (60 - current_seconds)))
elif ((current_minute < 55)); then
mpv "$HOME/media/audio/bell.opus"
echo "Next ring at 55."
sleep_seconds=$(( (54 - current_minute)*60+ (60 - current_seconds)))
else
echo "Next ring at 0."
sleep_seconds=$(( (59 - current_minute)*60+ (60 - current_seconds)))
fi

echo "Sleeping for $sleep_seconds seconds."
sleep $sleep_seconds

echo "Ringing Bell"
mpv "$HOME/media/audio/bell.opus"
done
