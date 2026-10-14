#!/bin/sh
set -xe
PKG_NAME=JuicyPixels-blp 

rm -rf ./dist-newstyle/sdist
cabal new-sdist
cabal upload --publish ./dist-newstyle/sdist/$PKG_NAME-*.tar.gz
