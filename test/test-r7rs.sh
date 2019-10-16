#!/bin/bash
set -eu
cd "$(dirname "$0")"
d="$(cd .. && pwd)"
case "$1" in
chibi-scheme)
    $1 -A "$d" test-r7rs.scm
    ;;
larceny)
    $1 -r7rs -A "$d" test-r7rs.scm
    ;;
esac
