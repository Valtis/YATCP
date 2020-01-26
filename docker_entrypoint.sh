#!/usr/bin/env bash

set -ueo pipefail

ACTION=$1
# rustup does not update the PATH
PATH="/root/.cargo/bin:$PATH"


WORKDIR="/tmp/YATCP_WORKDIR"


case "$ACTION" in
	test)
		cd $GITHUB_WORKSPACE
		rustup default stable # github pipeline needs this
		cargo test
		./generate_tests.sh	
		./run_e2e_tests.sh
	;;
	test-local)
		# We end up generating root owned files, and the repo is usually volume mounted.
		# To prevent any headaches when running locally, copy $GITHUB_WORKSPACE to /tmp and run tests there
		mkdir $WORKDIR
		cp -R $CODE_DIR $WORKDIR
		cd ${WORKDIR}/${CODE_DIR}

		cargo test

		./generate_tests.sh	
		./run_e2e_tests.sh
	;;
	*)
		echo "Unknown action '$ACTION'"
		exit 1
	;;
esac



