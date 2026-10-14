JuicyPixels-blp
===============

[![CI](https://github.com/NCrashed/JuicyPixels-blp/actions/workflows/ci.yml/badge.svg)](https://github.com/NCrashed/JuicyPixels-blp/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/JuicyPixels-blp.svg)](https://hackage.haskell.org/package/JuicyPixels-blp)

Read and write BLP1, the texture format of Warcraft III, with images as
[JuicyPixels](https://hackage.haskell.org/package/JuicyPixels) types. The
package also ships `blp2any`, a command-line converter between BLP and
common image formats.

For the layout of a BLP file and how the library handles each part, see
[docs/format.md](docs/format.md).

## Library

```haskell
import Codec.Picture
import Codec.Picture.Blp

main :: IO ()
main = do
  -- Decode: the result is always ImageRGBA8.
  Right img <- readBlp "war3mapMap.blp"
  writePng "war3mapMap.png" (convertRGBA8 img)

  -- Encode: JPEG at quality 90, with all mip levels the encoder makes.
  Right png <- readImage "icon.png"
  writeBlpJpeg "icon.blp" 90 (mipMapsUpTo 1 png) png
```

- `readBlp` / `decodeBlp` return the full size image.
- `readBlpMipmaps` / `decodeBlpMipmaps` return the image followed by its
  mipmaps.
- Three encoders are available:
  - `encodeBlpJpeg` / `writeBlpJpeg`: JPEG. The texture gets alpha when the
    input image has it.
  - `encodeBlpUncompressedWithAlpha` / `writeBlpUncompressedWithAlpha`: 256
    colour palette with 8-bit alpha.
  - `encodeBlpUncompressedWithoutAlpha` / `writeBlpUncompressedWithoutAlpha`:
    256 colour palette, no alpha.

The `Codec.Picture.Blp.Internal.*` modules expose the parser, encoder and
file structure. They are useful for tools, but their API may change between
minor versions.

## What works

- **Decoding** all BLP1 variants Warcraft III uses: JPEG, paletted with 8-bit
  alpha, and paletted without alpha, including their mipmaps.
- **Damaged and non-standard files** that are common in community maps and
  ports of World of Warcraft assets:
  - odd header values;
  - short palettes;
  - garbage in unused mipmap slots;
  - truncated or corrupt mipmaps. These cost only the levels from the damaged
    one onwards; a damaged full size image still fails the file.

  The decoder reports malformed input as `Left`; the test suite checks this
  for every kind of damage it covers.
- **Encoding** to JPEG or a 256 colour palette, with or without alpha. The
  JPEG header is shared between mipmaps, as in Blizzard's own files.

## Limitations

- **BLP0 and BLP2.** Only BLP1 is supported. BLP0 keeps mipmaps in separate
  `.b00`–`.b15` files; BLP2 is the World of Warcraft variant
  ([#3](https://github.com/NCrashed/JuicyPixels-blp/issues/3)).
- **1-bit and 4-bit alpha.** Paletted files with these decode as opaque
  ([#25](https://github.com/NCrashed/JuicyPixels-blp/issues/25)).
- **How alpha is chosen.** The decoder picks with-alpha or without-alpha
  from the `extra` header field instead of `alphaBits`. It also rejects
  `extra` values it does not know
  ([#25](https://github.com/NCrashed/JuicyPixels-blp/issues/25)).
- **Incomplete mip chain.** The encoder stops the mip chain when either side
  reaches 1 pixel, for example at 4×1 for a 32×8 texture, instead of going on
  to 1×1 ([#26](https://github.com/NCrashed/JuicyPixels-blp/issues/26)).
  Warcraft III draws missing levels as transparent black.
- **Lossy palette.** Uncompressed encoding quantises to 256 colours with
  dithering, so it is lossy for images with more colours.
- **Fixed header values.** The encoder always writes `extra` and
  `hasMipmaps` as 3 or 5 and 5.

## blp2any

```
blp2any convert INPUT_PATH OUTPUT_PATH [-i FORMAT] [-f FORMAT] [-q QUALITY]
                [-b jpg|uncompressedWithAlpha|uncompressedWithoutAlpha]
                [--min-mipmap-size N] [-s] [-p]
blp2any stats DIRECTORY OUTPUT_PATH
```

- **`convert`** turns a file or a whole directory from one format into
  another. Supported formats are `blp`, `png`, `jpg`, `tiff`, `gif` and
  `bmp`.
  - Formats are inferred from the file names, or given with `-i` and `-f`.
  - With a directory it recurses unless `-s` is given, and it recreates the
    subdirectories unless `-p` is given.
  - When writing BLP, `-b` picks the internal format (JPEG by default) and
    `--min-mipmap-size` the smallest mip level to keep. The default is 1,
    which keeps every level. A larger value leaves textures without their
    smallest levels, which is only safe for interface images.
- **`stats`** groups the BLP files in a directory by their header values
  and copies one example of each group to `OUTPUT_PATH`. It helps when
  hunting for unusual files.

```
blp2any convert war3/ textures-png/ -f png
blp2any convert icons/ icons-blp/ -i png -f blp -b jpg -q 85
```

## Building and testing

With [Nix](https://nixos.org) (flakes enabled), each supported GHC has its
own development shell:

```
nix develop .#ghc912      # or ghc94, ghc96, ghc98, ghc910, ghc914
cabal build all
cabal test
```

`nix develop` without a name gives a shell with haskell-language-server.
`nix build` builds the package, and `nix run . -- --help` runs `blp2any`.
Plain `cabal` works as well. `stack.yaml` is kept for Stack users, but CI
does not test it.

- **Test suite.** It builds its BLP fixtures in code (`test/Fixture.hs`):
  paletted files byte by byte, and JPEG files from the encoder, patched where
  needed. A test that pins down a fix names its PR or issue.
- **CI.** It builds and tests GHC 9.4 to 9.14 with `-Werror`.
