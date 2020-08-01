#!/bin/sh
set -eu

OBJECT_FILE=compile.o
EXECUTABLE_FILE=example.out
OUTPUT=$(cargo run --quiet -- $1 -o ${OBJECT_FILE})
if [ $? -ne 0 ]; then
	echo $OUTPUT
	exit -1
fi

OUTPUT=$(gcc ${OBJECT_FILE} helper.c -o ${EXECUTABLE_FILE})
if [ $? -ne 0 ]; then
	echo $OUTPUT
	exit -1
fi

./${EXECUTABLE_FILE}
