#!/bin/bash

[ -z "$1" ] && { echo "Missing file name"; exit -1; }
[ -f "$1" ] || { echo "$1 does not exist"; exit -1; }

filesize=$(wc -c < "$1")

# [ $filesize -eq 32768 ] || { echo "$1 has wrong size (not 32K)"; exit -1; }

minipro -p AT29C256@DIP28 -E
minipro -p AT29C256@DIP28 -b
minipro -p AT29C256@DIP28 -s -w $1

