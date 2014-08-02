#!/bin/sh
#
# dist.sh: build distribution archive

#XXX write this in scheme

set -e
date=`date +%Y%m%d`
distname=grass-${date}

rm -fr $distname
mkdir -p $distname/bootstrap

cp chicken-grass.c build/numbers.c $distname/bootstrap

for f in `cat MANIFEST`; do
    dir=`dirname $f`
    mkdir -p $distname/$dir
    cp -v $f $distname/$dir
done

tar cfz grass.tar.gz $distname
rm -fr $distname
