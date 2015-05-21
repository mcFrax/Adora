#!/bin/bash

printf "Testing \"$1\"..."

if ./interpreter "$1" </dev/null >/dev/null 2>/dev/null
    then printf " Failed\n"
        echo Unexpected compilation success.
        exit 1
    else printf " OK\n"
    fi
