#!/usr/bin/env bash

set -ueo pipefail

ACTION=$1
# rustup does not update the PATH
PATH="$HOME/.cargo/bin:$PATH"


WORKDIR="/tmp/YATCP_WORKDIR"



case "$ACTION" in
	test)
		# We end up generating root owned files, and the repo is usually volume mounted.
		# To prevent any headaches when running locally, copy $GITHUB_WORKSPACE to /tmp and run tests there
		
		mkdir $WORKDIR
		cp -R $GITHUB_WORKSPACE $WORKDIR
		cd $WORKDIR/$GITHUB_WORKSPACE
		cargo test
		./generate_tests.sh	
		./run_e2e_tests.sh
	;;
	*)
		echo "Unknown action '$ACTION'"
		exit 1
	;;
esac



