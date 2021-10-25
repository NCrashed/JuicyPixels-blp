#!/bin/sh
set -xe
PKG_NAME=JuicyPixels-blp 

rm ./dist-newstyle/$PKG_NAME-*-docs.tar.gz || true

# assumes cabal 2.4 or later
cabal v2-haddock --haddock-for-hackage --enable-doc

cabal upload -d --publish ./dist-newstyle/$PKG_NAME-*.tar.gz
