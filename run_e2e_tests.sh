#!/bin/sh
set -ue

TEST_NAME=${1:-""}
cargo build --quiet 2> /dev/null
cd tests/test_runner
cargo run 2> /dev/null -- $TEST_NAME
