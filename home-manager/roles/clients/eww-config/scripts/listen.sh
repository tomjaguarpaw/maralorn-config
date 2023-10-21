#!/usr/bin/env bash
while true; do socat -u "UNIX-CONNECT:/run/user/1000/status/$1" -; sleep 1s; done
