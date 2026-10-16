JuicyPixels-blp
===============

[![CI](https://github.com/NCrashed/JuicyPixels-blp/actions/workflows/ci.yml/badge.svg)](https://github.com/NCrashed/JuicyPixels-blp/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/JuicyPixels-blp.svg)](https://hackage.haskell.org/package/JuicyPixels-blp)

Read and write BLP1, the texture format of Warcraft III, and BLP2, its
World of Warcraft variant, with images as
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
  -- Decode: the result is an Image PixelRGBA8, whatever the content.
  Right img <- readBlp "war3mapMap.blp"
  writePng "war3mapMap.png" img

  -- Encode: JPEG at quality 90 with every mip level down to 1x1.
  Right png <- readImage "icon.png"
  result <- writeBlpWith "icon.blp" defaultBlpEncodeOptions png
  either print return result
```

- `readBlp` / `decodeBlp` return the full size image of a BLP1 or BLP2 file
  as `Image PixelRGBA8`.
- `readBlpMipmaps` / `decodeBlpMipmaps` return the image followed by its
  mipmaps.
- Files they cannot read give a `BlpDecodeError`, which says why: not a BLP
  file, a header cut short, unsupported content, an invalid size, a size
  above the limit, no image data, or a damaged full size image.
  `displayBlpDecodeError` turns it into a message.
- `encodeBlpWith` / `writeBlpWith` encode with `BlpEncodeOptions`:
  - `blpVersion`: `Blp1` (Warcraft III, the default) or `Blp2` (World of
    Warcraft).
  - `blpFormat`: in BLP1, `BlpFormatJpeg quality` (1 to 100; the texture
    gets alpha when the image has it); in BLP2, `BlpFormatDxt1` (1-bit
    alpha when some alpha is below 128), `BlpFormatDxt3`, `BlpFormatDxt5`
    or `BlpFormatBgra` (lossless); in both, `BlpFormatPaletted withAlpha`
    (256 colour palette, with 8-bit alpha or none).
  - `blpMipLevels`: how many mip levels to write, clamped to 1 and the full
    chain. `mipMapsUpTo` computes it from a smallest side length.
  - `blpDither`: whether paletted textures are dithered (on by default;
    turn it off for icons and pixel art).
  - `blpMaxSize`: halve the image until its larger side is at most this.
    Warcraft III shows mip levels up to 512 pixels only, so larger ones only
    make the file bigger.

  Start from `defaultBlpEncodeOptions` and change the fields you need.

  They return `Left` for images they cannot encode (`BlpEncodeError`): an
  empty image, a side above 65535 (after `blpMaxSize`), an invalid JPEG
  quality or size limit, a format the version does not have (such as
  JPEG in BLP2), or a file over 4 GiB.

The `Codec.Picture.Blp.Internal.*` modules expose the parser, decoder,
encoder and file structure. They are useful for tools, but their API may
change between minor versions.

## What works

- **Decoding** all BLP1 variants: JPEG, and paletted with 0, 1, 4 or 8-bit
  alpha, including their mipmaps.
- **Decoding BLP2** (World of Warcraft): DXT1, DXT3, DXT5, BGRA and
  paletted content, including mipmaps. `blpInfo` tells the versions and
  content types apart.
- **Damaged and non-standard files** that are common in community maps and
  ports of World of Warcraft assets:
  - odd header values;
  - short palettes;
  - garbage in unused mipmap slots;
  - truncated or corrupt mipmaps. These cost only the levels from the damaged
    one onwards; a damaged full size image still fails the file.

  The decoder reports malformed input as a `BlpDecodeError`, including
  errors raised inside JuicyPixels' JPEG decoder. Memory use follows the
  size in the file header, so decoding refuses images above 8192×8192 by
  default; check untrusted files with `blpInfo` and set your own limit with
  `decodeBlpWith (BlpDecodeOptions n)`. One known exception: some corrupt
  JPEG streams make JuicyPixels loop forever
  ([Juicy.Pixels#230](https://github.com/haskell-github-trust/Juicy.Pixels/issues/230)),
  so decoding untrusted files needs a timeout.
- **Encoding** BLP1 to JPEG or a 256 colour palette, with or without alpha.
  The JPEG header is shared between mipmaps, as in Blizzard's own files.
- **Encoding BLP2** to DXT1, DXT3, DXT5, BGRA or a palette. The DXT encoder
  fits each block along its principal axis and refines it by least squares;
  on test images it is 0.2–0.6 dB below libsquish's cluster fit (1.8 dB on a
  small icon) and 0.7–2.5 dB above a plain range fit.

## Limitations

- **Lossy palette.** Uncompressed encoding quantises to 256 colours, so it is
  lossy for images with more colours. Mipmaps after the first are mapped to
  the first level's palette.
- **Fixed header values.** The encoder writes `extra` as 3 with alpha and 5
  without, and `hasMipmaps` as 5, or 0 for a single level.

## Out of scope

The library reads and writes every BLP version in use, BLP1 and BLP2.
These are left out on purpose, not for lack of time:

- **BLP0**, which keeps mipmaps in separate `.b00`–`.b15` files and appeared
  only in the Reign of Chaos beta.
- **BLP2 with JPEG content** (`type` 0): no known file uses it, so it is
  rejected rather than guessed at.
- **Writing 1-bit or 4-bit alpha, or chosen `extra` and `hasMipmaps`
  values,** through the public encoder. `Codec.Picture.Blp.Internal.Encoder`
  can write any `BlpStruct`.
- **Archives and other texture formats**, such as MPQ, CASC or the DDS
  textures of Warcraft III: Reforged.

## blp2any

Ready-made binaries for Windows, macOS (Apple Silicon) and Linux are
attached to each [GitHub release](https://github.com/NCrashed/JuicyPixels-blp/releases)
from 0.5.0.0 on. The Linux binary needs glibc 2.35 or newer (Ubuntu 22.04,
Debian 12 and later), plus zlib and GMP, which almost every distribution
has. To build it yourself instead, use `cabal install JuicyPixels-blp`
(from Hackage) or `nix run github:NCrashed/JuicyPixels-blp`.

```
blp2any convert INPUT_PATH OUTPUT_PATH [-i FORMAT] [-f FORMAT] [-q QUALITY]
                [-b jpg|uncompressedWithAlpha|uncompressedWithoutAlpha|dxt1|dxt3|dxt5|bgra]
                [--blp2]
                [--min-mipmap-size N] [--max-size PIXELS] [--no-dither]
                [-s|--shallow] [-p|--flatten] [--timeout SECONDS]
blp2any stats DIRECTORY OUTPUT_PATH
```

- **`convert`** turns a file or a whole directory from one format into
  another. Supported formats are `blp`, `png`, `jpg`, `tiff` (`tif`), `gif`
  and `bmp`.
  - Formats are inferred from the file names, or given with `-i` and `-f`.
  - With a directory it recurses unless `-s` is given, and it recreates the
    subdirectories unless `-p` (`--flatten`) is given. A file that fails is
    reported on stderr and skipped; at the end it prints how many files were
    converted and failed, and exits with status 1 if any failed.
  - `--timeout SECONDS` gives up on a file that takes longer; use it on
    untrusted input, where some corrupt JPEG data makes decoding hang. The
    output never overwrites its input.
  - When writing BLP:
    - `-b` picks the internal format, JPEG by default. `dxt1`, `dxt3`,
      `dxt5` and `bgra` write BLP2; the others write BLP1 unless `--blp2`
      is given, which writes a paletted BLP2.
    - `--max-size PIXELS` halves the image until its larger side fits;
      Warcraft III shows mip levels up to 512 pixels only.
    - `--no-dither` turns off dithering for paletted output, which suits
      icons and pixel art.
    - `--min-mipmap-size` is the smallest mip level to keep. The default is
      1, which writes every level down to 1×1. A larger value leaves
      textures without their smallest levels; Warcraft III draws missing
      levels as transparent black, so this is only safe for interface
      images.
- **`stats`** groups the BLP files in a directory by their header values
  and copies one example of each group to `OUTPUT_PATH`, named after the
  group: content type, `alphaBits`, `extra` and `hasMipmaps` for BLP1, and
  `alphaDepth`, `alphaType` and `hasMips` for BLP2. Files it cannot read are
  reported and skipped. It helps when hunting for unusual files.

```
blp2any convert war3/ textures-png/ -f png
blp2any convert icons/ icons-blp/ -i png -f blp -b jpg -q 85
```

## Building and testing

See [CONTRIBUTING.md](CONTRIBUTING.md) for what a pull request needs.

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
- **CI.** It builds and tests GHC 9.4 to 9.14 with `-Werror`. The benchmarks are built
  in CI but not run.

### Benchmarks

`bench/Main.hs` (tasty-bench) times decoding and encoding at several sizes
for each content type, plus a few internals. The inputs are generated in
code. To compare a change against the current state:

```
cabal bench --ghc-options=-fproc-alignment=64 --benchmark-options '--csv before.csv'
# ... change the code ...
cabal bench --ghc-options=-fproc-alignment=64 --benchmark-options '--baseline before.csv'
```

Use `-p` to run a subset, for example `-p decode` or `-p '/jpeg alpha/'`.

Two things make the numbers noisy:

- **Code alignment.** Without `-fproc-alignment=64`, a change in one module
  can move the tight loops of another and shift its timings by 20–25%. This
  happened to the paletted decoder while the JPEG code was being changed.
- **Machine load.** Compare runs made under the same conditions, ideally by
  alternating the two builds.
