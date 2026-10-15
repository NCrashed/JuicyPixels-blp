Unreleased
==========

- Decoding refuses images above `blpMaxPixels` pixels, 8192x8192 by default,
  before allocating anything. `decodeBlpWith` and `decodeBlpMipmapsWith` take
  `BlpDecodeOptions` to set the limit. The 0.4.0.0 notes claimed that small
  files could no longer demand gigabytes; that was wrong. A JPEG frame
  smaller than its level is padded to the level size, and the level size
  comes from the header, so a 493-byte file claiming 65535x65535 pixels
  still made the decoder allocate about 17 GB.
- `blpInfo` reads the header (content type, size, alpha bits, usable mip
  levels) without decoding any pixels, so callers can reject textures
  before decoding them.
- `BlpCompression` is exported from `Codec.Picture.Blp`.
- blp2any:
  - `--timeout SECONDS` gives up on a file that takes too long, so a corrupt
    JPEG that makes decoding hang no longer stalls a batch.
  - An output name with an unknown extension is an error; it used to be
    written as PNG regardless, and `.tif` output really was PNG.
  - It refuses to write over its input, rejects `-q` outside 1..100, prints
    encoding errors in words, and `stats` finds `*.BLP` in upper case.
- Lower bounds now match the oldest versions CI tests: JuicyPixels 3.3.6,
  vector 0.12.3, attoparsec 0.14.4, binary 0.8.9, bytestring 0.11.5 and
  deepseq 1.4.8 (and for blp2any directory 1.3.7, filepath 1.4.2,
  optparse-simple 0.1, transformers 0.5.6). The old bounds allowed versions
  that were never tested and do not build with any supported GHC.
- Fixed the documentation of `decodeBlpMipmaps`, which rendered as "Element
  @ is always mip level @".

0.4.0.0 (2026-10-15)
====================

Breaking changes:

- New encoding API: `encodeBlpWith` and `writeBlpWith` take
  `BlpEncodeOptions` (a `BlpFormat` and a mip level count) and return
  `Either BlpEncodeError`. Empty images, sides above 65535, JPEG quality
  outside 1..100 and files over 4 GiB are errors instead of crashes or
  silent wrap-around (#31). `encodeBlpJpeg`, `encodeBlpUncompressedWithAlpha`,
  `encodeBlpUncompressedWithoutAlpha` and their `writeBlp*` variants still
  work as deprecated wrappers; they clamp the JPEG quality, which used to
  wrap modulo 256.
- The decoder takes alpha from the `alphaBits` header field, as the format
  specifies, instead of from the `extra` field. Paletted files with 1-bit and
  4-bit alpha now decode, files whose `extra` field says otherwise keep their
  alpha, and unknown `extra` values are no longer rejected (#25).
- Non-square textures get the full mip chain down to 1x1 instead of
  stopping when the shorter side reaches one pixel, and `mipMapsUpTo` counts
  levels by the larger side, capped at 16 (#26, #31).
- Mipmap colour is averaged with alpha as the weight, so transparent pixels
  no longer darken the edges of sprites, and the alpha of paletted mipmaps
  is averaged instead of sampled. Opaque textures encode exactly as before
  (#32).
- `Codec.Picture.Blp.Internal.*`:
  - `BlpStruct` keeps the raw header values in `blpAlphaBits`, `blpExtra` and
    `blpHasMipmaps`; `blpCompression` is a function of `blpExt`. `BlpFlag` and
    `BlpPictureType` are gone.
  - Paletted content is one `BlpPaletted` constructor holding each level's
    raw data.
  - The parser is `parseBlp` plus `mipLevels` and `jpegFrameSize`; the
    attoparsec walk over the content (`blpJpegParser`, `skipToOffset`, ...)
    is gone.
  - `encodeBlp` and `blpEncoder` take just the `BlpStruct`; `toBlpStruct`
    takes a `BlpFormat`.

Decoding untrusted files (#30):

- Errors that JuicyPixels' JPEG decoder raises from pure code are returned
  as `Left`; in a tail mipmap they end the list.
- Width and height must be between 1 and 65535. Larger values used to
  overflow the size checks and leave an exception in the decoded image.
- Each JPEG level's frame size is read before decoding. A frame larger than
  the level is rejected; a smaller one is padded with transparent black.
  (This was described as stopping small files from demanding gigabytes;
  it did not, see 0.4.1.0.)
- Mipmap slots are read by index. A missing or garbage slot ends the chain
  instead of shifting the later levels, and a garbage slot that points into
  the palette no longer breaks the file.
- Some corrupt JPEG streams still make JuicyPixels loop forever
  (haskell-github-trust/Juicy.Pixels#230), so decoding untrusted files needs
  a timeout.

Encoder fixes (#31):

- Unused mipmap slots are zero, as the format requires, and `hasMipmaps` is
  0 when only one level is written.
- Only the levels that are written are encoded; asking for one level of a
  2048x2048 JPEG texture used to encode all of them.

Faster (#34): JPEG decoding is 2.1–2.7x and JPEG encoding 2.2–2.8x as fast,
measured with the new benchmark suite (`bench/Main.hs`). Output is
byte-identical.

blp2any (#33):

- `-s`/`--shallow` and `-p` (now also `--flatten`) were accepted but did
  nothing; they now keep conversion to the top folder and write all output
  into one folder.
- A file that fails no longer aborts a batch conversion or `stats`: it is
  reported on stderr and skipped, and `convert` prints a summary and exits
  with status 1 if anything failed.
- An input folder given with a trailing slash no longer sends output to the
  root of the file system.
- `--version` reports the package version.

Documentation: `docs/format.md` describes the BLP1 format and what the
library does with each part; the README covers usage, `blp2any`,
limitations and benchmarking; every module has haddock (#27).

0.3.0.0 (2026-10-15)
====================

Breaking changes:

- `encodeBlpUncompressedWithAlpha` always writes an alpha plane and
  `encodeBlpUncompressedWithoutAlpha` never does. Both used to pick by the
  input image, so the two were identical and `blp2any -b` had no effect.
- `decodeBlp` and `decodeBlpMipmaps` always return `ImageRGBA8`. JPEG BLPs
  whose stream is not four-component (greyscale, YCbCr) used to come back as
  `ImageY8` or `ImageRGB8`.
- Removed unused functions from `Codec.Picture.Blp.Internal.Convert`:
  `toPngRepresentable` and `toBlpUncompressable`.
- `mipMapsUpTo` counts every level down to the minimum side; it was one short
  and returned 0 or less for tiny images.
- Removed the `TextShow` and `Hashable` instances from
  `Codec.Picture.Blp.Internal.Data`, and with them the `text-show` and
  `hashable` dependencies. `blp2any` no longer depends on
  `unordered-containers`.

The encoder always writes mip 0; a mipmap count of zero or less used to
produce an unreadable file, which `blp2any` did for 1x1 images.

The uncompressed encoder no longer hands out ByteStrings over image memory it
does not keep alive, and no longer relies on an orphan, endianness-dependent
`Storable PixelRGBA8` instance.

Decoder robustness for BLP1 files found in the wild:

- Accept picture type 6 (WoW JPEG variant) and picture type 0 (#5, #10).
- Tolerate palettes shorter than 256 entries (#6).
- Decode files labelled with-alpha whose mipmap 0 has no alpha plane as
  without-alpha instead of crashing (#8).
- A corrupt JPEG mipmap no longer fails the whole file; the mipmap list is
  cut at the first broken level (#9).
- `decodeBlpMipmaps` renders uncompressed mipmaps at their own sizes instead of
  crashing; a mip 0 too short for the image is a `Left` (#13).
- Without-alpha textures decode opaque regardless of the palette's fourth
  byte, so the encoder's own output no longer comes back transparent (#14).
- Decoding no longer dumps the parsed structure to stderr (#15).
- The JPEG encoder writes plain BGRA planes instead of dividing colour by
  alpha, and a uniformly semi-transparent alpha plane is kept; only all-0x00
  or all-0xFF planes count as unpopulated (#17).
- The JPEG encoder shares one header between mipmaps; 1x1 textures no longer
  encode to a file without image data (#18).
- Skip mipmap slots that point past the end of the file or overlap an earlier
  one (#7).
- Decode JPEG planes as BGRA instead of CMYK (#11).

Support GHC 9.4 – 9.14: relax upper bounds for `attoparsec`, `bytestring`,
`hashable`, `text-show`, `vector` and `filepath`. Minimum supported GHC is now 9.4.

0.2.1.0
=======

Relax upper boundary for `text-show-3.9`.

0.2.0.0
=======

Implement #1. Tool now preserves subfolder structure.

0.1.3.0
=======

Fix #2 index error. 

0.1.2.0
=======

Relax upper boundary for `hashable` and `text-show`.

0.1.1.0
=======

Relax upper boundary for `JuicyPixels-3.3`.

0.1.0.1
=======

Relax upper boundary for `binary-0.9.0.0`.

0.1.0.0
=======

Initial release.
