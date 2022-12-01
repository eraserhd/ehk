#!/bin/sh
tr '\n' '+' </tmp/input.txt |sed -e 's/++/\n/g' -e 's/+$//' |bc |sort -n |tail -1
