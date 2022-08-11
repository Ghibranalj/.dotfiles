#!/usr/bin/env bash

# this script has many dependencies, and is not meant to be run standalone
# 1. fd
# 2. gdbmtool
# 3. gdbm

IGNORE="notify-send blueman"

DIR="$HOME/.cache/dunst"
file="$DIR/dunst.log"
mkdir -p "$DIR"
touch "$file"

DATASTORE="$DIR/notificons.gdbm"

put() {
    KEY="$1"
    VAL="$2"
    gdbmcmd="store $KEY $VAL"
    gdbmtool -s $DATASTORE <<<"$gdbmcmd"
}
get() {
    KEY="$1"
    gdbmcmd="fetch $KEY"
    out=$(gdbmtool -s $DATASTORE <<<"$gdbmcmd")
    [[ $out == *"No uch item found"* ]] && echo -n "" || echo "$out"
}

getIcon() {
    ICON_NAME=$(echo "$1" | awk '{print tolower($0) }')
    ICON_NAME=$(awk '{$1=$1}1' OFS="-" <<<"$ICON_NAME")
    ICON=$(get "$ICON_NAME")
    if [[ $ICON == "" ]]; then
        ICON=$(fd -j8 -c never "$ICON_NAME" /usr/share/icons | grep apps | head -1)
        if [ "$ICON" == "" ]; then
            ICON=$(fd -j8 -1 -c never "$ICON_NAME" /usr/share/pixmaps)
        fi
        [[ $ICON == "" ]] && ICON="-"
        put "$ICON_NAME" "$ICON"
    fi

    echo "$ICON"
}

if [[ $IGNORE == *"$1"* ]]; then
    exit 0
fi
ICON=$(getIcon "$1")
if [[ "$ICON" == "-" ]]; then
    TITLE="<big>$1</big> "
fi
NOTIF="$TITLE$2 : <small>$3</small> <sub>$(date '+%H:%M %d/%m/%y')</sub>;;;;$ICON;;;;$5"
NOTIF=${NOTIF//\&/&amp;}

if [[ "$(cat "$file")" == "" ]]; then
    echo -e "$NOTIF" >"$file"
else
    sed -i "1 i$NOTIF" "$file"
fi
