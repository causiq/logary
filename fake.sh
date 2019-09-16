#!/usr/bin/env bash
source .env
set -eu
set -o pipefail

./tools/install-tools
.fake/fake "$@"
