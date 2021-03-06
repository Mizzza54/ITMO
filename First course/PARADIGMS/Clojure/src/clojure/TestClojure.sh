#!/bin/bash
set -euo pipefail

if [[ -z "$1" ]] ; then
    echo Usage: $(basename "$0") \<full.test.ClassName\>
    exit 1
fi

TEST="$1"
CLOJURE="$(dirname "$0")"
REPO="$(dirname "$CLOJURE")"
LIB="$CLOJURE/lib"

javac \
    -d "__out" \
    "--class-path=$LIB/*:$REPO/java:$REPO/javascript:$REPO/clojure" \
    "$CLOJURE/${TEST//\./\/}.java"
java \
    -ea \
    "--class-path=$LIB/*:__out" \
    "$TEST"
