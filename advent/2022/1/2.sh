#!/bin/sh
tr '\n' '+' </tmp/input.txt |sed -e 's/++/\n/g' -e 's/+$/\n/' |bc |sort -n |tail -3 |tr '\n' '+' |sed -e 's/+$/\n/' |bc

