#!/bin/sh

set -ue

cargo run --quiet -- $1 > /dev/null
gcc a.out helper.c -o "$1.bin" > /dev/null
./"$1.bin"
