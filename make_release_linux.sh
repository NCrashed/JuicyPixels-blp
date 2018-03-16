runhaskell Setup.hs configure --prefix=/usr --package-db=./.cabal-sandbox/x86_64-linux-ghc-8.0.2-packages.conf.d
runhaskell Setup.hs build
runhaskell Setup.hs haddock
runhaskell Setup.hs register --gen-script
runhaskell Setup.hs unregister --gen-script
mkdir -p /tmp/JuicyPixels-blp
runhaskell Setup.hs copy --destdir=/tmp/JuicyPixels-blp
dr=$PWD
cd /tmp/JuicyPixels-blp && rm -rf ./home && tar -czf $dr/JuicyPixels-blp.tar.gz .
