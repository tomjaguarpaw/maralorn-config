#!/usr/bin/env bash
activeworkspace (){
  hyprctl monitors -j | jq '.[] | select(.focused) | .activeWorkspace.id'
}
activeworkspace
socat -u "UNIX-CONNECT:/tmp/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock" - | while read -r; do
  activeworkspace
done
