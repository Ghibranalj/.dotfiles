#!/usr/bin/env bash

# echo $1 $2 $3 $4 $5

TYPE=$3
ID=$2

if [ "$TYPE" == "XISlavePointer" ]; then
    echo "Mouse Added Disabling Acceleration"
    xinput --set-prop "$ID" "libinput Accel Speed" 0 && \
        echo "Mouse Acceleration Disabled" || \
        echo "Failed to disable mouse acceleration"
fi
# disables mouse acceleration
