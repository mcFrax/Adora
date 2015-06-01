#!/usr/bin/sh

set -e
waitsecs=2
echo Wait "$waitsecs" seconds...
sleep "$waitsecs"  # wait for timestamps to be in past in students time
cd ~/jpp/adora
rm -rf franciszek_boehlke
tar -xf franciszek_boehlke.tar.gz
cd franciszek_boehlke
make test
