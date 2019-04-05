#!/usr/bin/env bash
set -o pipefail
set -e
head -1 RELEASE_NOTES.md | sed 's/#### //g'
