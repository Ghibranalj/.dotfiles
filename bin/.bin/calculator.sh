#!/bin/env bash

# run calculator in rofi and copy the result to the clipboard
value=$(rofi -show calc -modi calc -no-show-match -no-sort | cut -d "=" -f2 | cut -d "≈" -f2)

# copy the value to the clipboard if not empty
if [ -n "$value" ]; then
    echo -n "$value" | xclip -selection clipboard
fi

echo "$value"
