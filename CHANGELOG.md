Unreleased
==========

Decoding untrusted files is safer:

- Errors that JuicyPixels' JPEG decoder raises from pure code are returned
  as `Left` instead of escaping as exceptions, also from tail mipmaps.
- Width and height must be between 1 and 65535. Larger values used to
  overflow the size checks and leave an exception in the decoded image.
- Each JPEG level's frame size is read before decoding. A frame larger than
  the level is rejected, so an 800-byte file can no longer demand gigabytes;
  a smaller one is padded with transparent black.
- Mipmap slots are read by index. A missing or garbage slot ends the chain
  instead of shifting the later levels, and a garbage slot that points into
  the palette no longer breaks the file.

Breaking changes to `Codec.Picture.Blp.Internal.*`:

- `BlpStruct` fields `blpFlags`, `blpPictureType` and `blpPictureSubType`
  are now `blpAlphaBits`, `blpExtra` and `blpHasMipmaps`, holding the raw
  header values. `BlpFlag` and `BlpPictureType` are gone.
- `BlpUncompressed1` and `BlpUncompressed2` are merged into `BlpPaletted`,
  which keeps each mip level's raw data; `blpUncompressed1Parser` and
  `blpUncompressed2Parser` are replaced by `blpPalettedParser`.

The decoder takes alpha from `alphaBits` as the format specifies, instead of
from the `extra` field (#25). Paletted files with 1-bit and 4-bit alpha now
decode, files whose `extra` field says otherwise keep their alpha, and
unknown `extra` values are no longer rejected. Encoder output is unchanged.

The encoder writes the full mip chain down to 1x1 for non-square textures
instead of stopping when the shorter side reaches one pixel (#26), and
`mipMapsUpTo` counts levels by the larger side, so `blp2any`'s default
keeps every level.

Documentation: a description of the BLP1 format and of what the library
supports (`docs/format.md`), a rewritten README with usage, `blp2any` and
limitations, haddock for every module, and corrected `blp2any` help.
The old `Specification` file is gone; it described several header fields
and the palette alpha wrongly.

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
