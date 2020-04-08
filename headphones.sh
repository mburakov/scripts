#!/bin/bash

echo 0 | sudo dd of=/sys/module/snd_hda_intel/parameters/power_save
if [ "$XDG_SESSION_DESKTOP" == "sway" ]
then
  message='Ожидаю манипуляции с наушниками. Нажмите Enter после их завершения.'
  bemenu -b --fn 'Sans Serif 11' \
    --tf '#323232' --tb '#323232' --fb '#323232' --ff '#ffffff' \
    --nf '#323232' --nb '#5c5c5c' --hb '#285577' --hf '#ffffff' \
    <<< "$message"
else
  message='Ожидаю манипуляции с наушниками.\nНажмите OK после их завершения.'
  zenity --info --no-wrap --text="$message"
fi
echo 1 | sudo dd of=/sys/module/snd_hda_intel/parameters/power_save
