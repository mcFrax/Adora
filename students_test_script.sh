#!/usr/bin/sh

set -e
cd ~/jpp/adora
rm -rf franciszek_boehlke
tar -xf franciszek_boehlke.zip
cd franciszek_boehlke
make test-good
