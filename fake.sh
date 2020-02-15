#!/usr/bin/env bash
[[ -r .env ]] && source .env
set -eu
set -o pipefail

./tools/install-tools
.fake/fake "$@"
