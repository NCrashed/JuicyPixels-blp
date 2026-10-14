Unreleased
==========

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
