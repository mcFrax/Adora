#!/bin/bash

printf "Testing \"$1\"..."

expected_out=$(sed -n ':1;s/# EXPECTED OUTPUT #//;t2;n;b1;:2;n;:3;s/^# //1;p;n;b3' "$1")
actual_out=$(./interpreter "$1")

if [ "$expected_out" == "$actual_out" ]
    then printf " OK\n"
    else printf " Failed\n"
        echo Expected output:
        echo "$expected_out"
        echo Actual output:
        echo "$actual_out"
        exit 1
    fi
