#! /bin/env python3
import re

f = open("my_layout.xkbd", "r")

xkbs = f.read().strip().removeprefix("xkb_keymap {\n").removesuffix("\n};")

match = re.findall("xkb_symbols.+[\s\S]+};", xkbs)[0]
print(match)