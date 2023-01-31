#!/usr/bin/env bash

CAFFEINE_FILE=$HOME/.cache/caffeine

if [[ -f $CAFFEINE_FILE ]]; then
    rm "$CAFFEINE_FILE"
    exit 0
fi

touch "$CAFFEINE_FILE"

