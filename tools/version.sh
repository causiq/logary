#!/usr/bin/env sh
set -o pipefail
set -e
BASEDIR=$(dirname "$0")
head -1 $BASEDIR/../RELEASE_NOTES.md | sed 's/#### /v/g'
