#!/bin/bash

if [ "$#" -eq "0" ]
then
  file /usr/bin/python
  exit 0
fi

sudo rm /usr/bin/python &&
sudo ln -s /usr/bin/python$1 /usr/bin/python
