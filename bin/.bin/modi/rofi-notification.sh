#!/usr/bin/env bash
# -*- mode: sh; -*-
DIR="$HOME/.cache/dunst"
file="$DIR/dunst.log"

CLR="Clear notifications"

if [[ "$*" == "$CLR" ]]; then
    echo -n "" >"$file"
    exit 0
elif [[ $ROFI_RETV -ne 0 ]]; then
    LINE=$ROFI_INFO
    sed -i "$LINE"d "$file"
fi

echo -en "\0prompt\x1fNotification\n"
echo -en "\0markup-rows\x1ftrue\n"
echo -en "\0no-custom\x1ftrue\n"

# cat $file
counter=1
URGENTS=""

echo $CLR
while read -r line; do
    icon=$(echo $line | awk -F';;;;' '{print $2}')
    content=$(echo $line | awk -F';;;;' '{print $1}')
    urgency=$(echo $line | awk -F';;;;' '{print $3}')

    echo -en "$content\0icon\x1f$icon\x1finfo\x1f$counter\n"

    [[ "$urgency" == "CRITICAL" ]] && URGENTS="$URGENTS $counter"

    counter=$((counter + 1))
done <$file

for i in $URGENTS; do
    echo -en "\0urgent\x1f$i\n"
done
