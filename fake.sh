#!/usr/bin/env bash
[[ -r .env ]] && source .env
set -eu
set -o pipefail

dotnet tool restore
dotnet tool run fake $@