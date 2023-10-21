#!/usr/bin/env bash
keymap (){
  hyprctl devices -j | jq -r '.keyboards | map(select(.name == "keyboardio-model-100-keyboard")) | .[].active_keymap'
}

keymap

socat -u "UNIX-CONNECT:/tmp/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock" - | while read -r; do
  keymap
done
