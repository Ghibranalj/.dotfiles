#!/usr/bin/env bash

DIR="$HOME/.cache/dunst"
file="$DIR/dunst.log"

mkdir -p "$DIR"
touch "$file"

IGNORE="pasystray notify-send"
if [[ $IGNORE == *"$1"* ]]; then
    exit 0
fi

echo "$1;;$2;;$3" >> "$file"

# echo "$1;;$2;;$3;;$4;;$5" >> "$file.tmp"
