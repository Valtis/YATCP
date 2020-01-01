#!/bin/sh
set -ue

cargo build 2> /dev/null
cd tests/test_runner
cargo run