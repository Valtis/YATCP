#!/bin/bash

set -ueo pipefail

docker run --env GITHUB_WORKSPACE="/code" -v $(pwd):/code  $1 test 


