#!/bin/bash

set -ueo pipefail


WORKSPACE="/code"

docker run --env CODE_DIR="$WORKSPACE" -v $(pwd):$WORKSPACE  $1 test-local


