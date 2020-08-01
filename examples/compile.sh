#!/bin/sh
set -eu

OBJECT_FILE=compile.o
OUTPUT=$(cargo run --quiet -- $1 -o ${OBJECT_FILE})
if [ $? -ne 0 ]; then
	echo $OUTPUT
	exit -1
fi

OUTPUT=$(gcc ${OBJECT_FILE} helper.c -o test)
if [ $? -ne 0 ]; then
	echo $OUTPUT
	exit -1
fi

./test
