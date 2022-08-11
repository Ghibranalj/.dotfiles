#!/usr/bin/env bash

# echo $1 $2 $3 $4 $5

TYPE=$3
ID=$2

if [ "$TYPE" == "XISlavePointer" ]; then
    echo "Mouse Added"
    xinput --set-prop "$ID" 'libinput Accel Profile Enabled' 0, 1
fi
# disables mouse acceleration
