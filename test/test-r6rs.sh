#!/bin/bash
set -eu
cd "$(dirname "$0")"
d="$(cd .. && pwd)"
export CHEZSCHEMELIBDIRS="$d"
export IKARUS_LIBRARY_PATH="$d"
export IRONSCHEME_LIBRARY_PATH="$d"
export LARCENY_LIBPATH="$d"
export LOKO_LIBRARY_PATH="$d"
export MOSH_LOADPATH="$d"
export VICARE_SOURCE_PATH="$d"
export YPSILON_SITELIB="$d"
test -e "../srfi/%3a177.sls" || ln -s "177.sls" "../srfi/%3a177.sls"
test -e "../srfi/:177.sls" || ln -s "177.sls" "../srfi/:177.sls"
case "$1" in
chez | chez-scheme | chezscheme | scheme)
    $1 --program test-r6rs.sps
    ;;
ikarus)
    $1 test-r6rs.sps
    ;;
ironscheme | isc)
    $1 test-r6rs.sps
    ;;
larceny)
    $1 -r6rs test-r6rs.sps
    ;;
loko)
    exec -a scheme-script loko test-r6rs.sps
    ;;
mosh | nmosh)
    $1 test-r6rs.sps
    ;;
vicare)
    $1 --r6rs-script test-r6rs.sps
    ;;
ypsilon)
    $1 test-r6rs.sps
    ;;
esac
