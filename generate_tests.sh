#!/bin/bash

set -ueo pipefail

find -ipath "*generated*/*yaml" -exec rm  {} \;
find tests -iname *py -exec bash -c 'cd $(dirname {}) && python3 $(basename {})' \;

