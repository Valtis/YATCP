#!/bin/bash
find tests -iname *py -exec bash -c 'cd $(dirname {}) && python3 $(basename {})' \;

