set -xe
PKG_NAME=JuicyPixels-blp 

rm ./dist-newstyle/sdist -rf | true
cabal new-sdist
cabal upload --publish ./dist-newstyle/sdist/$PKG_NAME-*.tar.gz
