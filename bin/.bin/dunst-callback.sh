#!/usr/bin/env bash

DIR="$HOME/.cache/dunst"
file="$DIR/dunst.log"

mkdir -p "$DIR"
touch "$file"

IGNORE="pasystray notify-send"
if [[ $IGNORE == *"$1"* ]]; then
    exit 0
fi

echo "$1;;$2;;$3;;$(date '+%H:%M %d %b')" >> "$file"
