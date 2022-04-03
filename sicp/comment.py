#!/usr/bin/env python3
import sys

MAX_LINE_LEN = 80

symbols = {
    "\ue039": "ft",
    "\ue049": "Th",
    "\ue03c": "tt"
    }

def format_string(s):
    for k, v in symbols.items():
        s = s.replace(k, v)

    s = s.replace("\n", " ")
    result = ""

    i = 0
    while i < len(s):
        end = i+MAX_LINE_LEN
        ss = s[i : end]
        last_ws = ss.rindex(" ")
        last_ws = end if last_ws == 0 else last_ws
        result += ss[:last_ws].strip() + '\n '
        i += last_ws

    #lines = len(s) // MAX_LINE_LEN
    #for i in range(lines):
    #    start = i * MAX_LINE_LEN
    #    _end = (i+1) * MAX_LINE_LEN
    #    end = s[start:_end].rindex(" ")
    #    result += s[start:end].strip() + "\n "

    #result += s[lines * MAX_LINE_LEN:]

    return result


if __name__ == "__main__":
    if len(sys.argv) == 1:
        print("Expected 1 argument.")
        sys.exit(1)
    arg = sys.argv[1]
    formatted = format_string(arg)
    print()
    print(formatted)
