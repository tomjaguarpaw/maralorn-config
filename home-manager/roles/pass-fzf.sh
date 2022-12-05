#!/usr/bin/env bash

## Adapted from https://gist.github.com/heywoodlh/4c1e27f477a896bc3b0f6d55e2748d26

## Assumes that the user is using otp plugin
## Install fzf and make sure pass is configured beforehand

## If you want to grab the otp code, use like so: `pass-fzf.sh otp`, otherwise script will assume you want password

cd ~/.password-store || exit

if [[ $1 == 'otp' ]]
then
	type="otp"
else
	type="password"
fi

selection="$(find -L . -name '*.gpg' | sed -e 's/.\///' -e 's/.gpg//' | fzf)"

if [ ${type} == "otp" ]
then
	pass otp "${selection}" -c

else
	pass "${selection}" -c
fi

