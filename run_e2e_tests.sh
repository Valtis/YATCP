#!/bin/sh
set -ue

TEST_NAME=${1:-""}

OUTPUT=$(cargo build --quiet)
if [ $? -ne 0 ]; then
	echo $OUTPUT
	exit -1
fi

cd tests/test_runner

OUTPUT=$(cargo build --quiet)
if [ $? -ne 0 ]; then
	echo $OUTPUT
	exit -1
fi

cargo run 2> /dev/null -- $TEST_NAME
