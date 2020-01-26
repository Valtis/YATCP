#!/bin/bash

set -ueo pipefail

find tests -iname *py -exec bash -c 'cd $(dirname {}) && python3 $(basename {})' \;

