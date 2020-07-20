#!/bin/sh
set -ue

cargo build --quiet 2> /dev/null
cd tests/test_runner
cargo run 2> /dev/null
