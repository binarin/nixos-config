#!/usr/bin/env zsh
local url="$1"

if [[ $url =~ 'booking\.com'
       || $url =~ 'booking\.facebook\.com'
       || $url =~ '#label/Lists%2Fsp\.data\.cron'
       || $url =~ 'booking\.workplace\.com'
   ]]; then
    exec google-chrome-stable "$@"
else
    exec firefox "$@"
fi
