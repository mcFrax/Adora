#!/usr/bin/sh

set -e
cd ~/jpp/adora
rm -rf franciszek_boehlke
tar -xf franciszek_boehlke.tar.gz
cd franciszek_boehlke
make test
