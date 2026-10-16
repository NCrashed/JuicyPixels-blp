# The BLP format

BLP ("Blizzard Picture") is the texture format of Warcraft III (BLP1) and
World of Warcraft (BLP2). This library reads and writes **BLP1** and reads
**BLP2**. This page describes the format as the community has documented
it, and states for each part what the library does with it, including where
it falls short. The sections up to [BLP2](#blp2-world-of-warcraft) are about
BLP1.

Primary sources:

- [BLP Specifications (WC3)](https://www.hiveworkshop.com/threads/blp-specifications-wc3.279306/)
  by Dr Super Good, the most complete account of how Warcraft III treats
  BLP files.
- [BLP on wowdev](https://wowdev.wiki/BLP), which covers all three versions.

The older description by Magos, which this repository used to ship as
`Specification`, gets several fields wrong: the meaning of the header dwords
at `0x08`, `0x14` and `0x18`, and the "inverted alpha" in the palette. The
names below follow the newer sources.

All values are little-endian.

## Header

The header is 156 bytes (`0x9C`) long.

| Offset | Type | Name | Library field | Meaning |
|---|---|---|---|---|
| `0x00` | `char[4]` | magic | – | `"BLP1"` |
| `0x04` | `uint32` | content | `blpCompression` | `0` = JPEG, `1` = paletted ("direct") |
| `0x08` | `uint32` | alphaBits | `blpAlphaBits` | Alpha depth: 0, 1, 4 or 8 for paletted content, 0 or 8 for JPEG |
| `0x0C` | `uint32` | width | `blpWidth` | Width of mip 0 in pixels |
| `0x10` | `uint32` | height | `blpHeight` | Height of mip 0 in pixels |
| `0x14` | `uint32` | extra | `blpExtra` | No known effect; WorldEdit writes 5 |
| `0x18` | `uint32` | hasMipmaps | `blpHasMipmaps` | `0` = only mip 0, otherwise a full mip chain |
| `0x1C` | `uint32[16]` | mipmapOffset | – | File offset of each mip level |
| `0x5C` | `uint32[16]` | mipmapSize | – | Size in bytes of each mip level |

What the library does with these fields:

- **Magic.** `BLP2` files are read as described in
  [BLP2](#blp2-world-of-warcraft); anything else that is not `BLP1`, such
  as BLP0, is rejected (see [Not supported](#not-supported)).
- **content.** Values other than 0 and 1 are rejected. The spec suggests
  treating them as JPEG; the library is stricter.
- **alphaBits.** Decides whether there is alpha and how it is packed. Values
  that are not valid for the content type mean no alpha. On paletted files,
  a mip 0 too short to hold the declared alpha list also means no alpha;
  some tools declare alpha and then write only the index list. The encoder
  writes 8 or 0.
- **extra.** Read and kept, but it has no effect on decoding. The encoder
  writes 3 with alpha and 5 without, so that readers which still pick alpha
  by this field, as older versions of this library did, read the files
  correctly.
- **hasMipmaps.** The decoder ignores it and goes by the mipmap slots. The
  encoder writes 5, or 0 when it writes a single level.

## Mipmaps

Level `l` of a `w`×`h` texture is `max 1 (w / 2^l)` by `max 1 (h / 2^l)`.
The chain is as long as the **larger** side needs to reach 1×1. For example,
32×8 has six levels: 32×8, 16×4, 8×2, 4×1, 2×1 and 1×1. Unused slots are
zero.

Warcraft III does not generate missing levels. When `hasMipmaps` is set, it
draws them as transparent black, and a model whose texture lacks them can
fail to load. It also only uses mip levels of at most 512×512; larger levels
are stored but skipped.

**Decoder.**

- Width and height must be between 1 and 65535; anything else is an error.
- Slots are read by index from slot 0. The chain ends at the first slot that
  is empty, runs past the end of the file, points into the header, or
  overlaps an earlier level, and after the last level of the full chain.
  This drops garbage in unused slots
  ([#7](https://github.com/NCrashed/JuicyPixels-blp/pull/7)), including
  slots that repeat the last level.
- `decodeBlpMipmaps` returns the levels up to the first damaged one, so
  element `i` is always level `i`
  ([#9](https://github.com/NCrashed/JuicyPixels-blp/pull/9),
  [#13](https://github.com/NCrashed/JuicyPixels-blp/issues/13)).
- A damaged mip 0 fails the file.

**Encoder.**

- It writes as many levels as asked for, at least mip 0 and at most the full
  chain down to 1×1. Unused slots are zero, and levels are encoded only
  if they are written. Downscaling averages 2×2 blocks, reusing the edge pixel
  on odd or one-pixel sides. Colour is weighted by alpha, so transparent
  pixels do not darken the edges of sprites; alpha, including the alpha list
  of paletted textures, is a plain average. Paletted levels after mip 0 are
  averaged this way and then mapped to the nearest colour of mip 0's
  palette ([#26](https://github.com/NCrashed/JuicyPixels-blp/issues/26)).
- `mipMapsUpTo` counts the levels whose larger side is at least a minimum;
  with 1 that is the full chain.
- `blp2any --min-mipmap-size N` with `N > 1` writes a deliberately short
  chain. That is fine for interface images, but risky for model
  textures, for the reason above.

## JPEG content (`content = 0`)

```
uint32 jpegHeaderSize
uint8  jpegHeader[jpegHeaderSize]
...    mip levels at mipmapOffset[i], mipmapSize[i] bytes each
```

- **Assembling a level.** Each level is `jpegHeader ++ mipData`, a complete
  four-component JFIF stream.
- **Components.** They are the texture's **B, G, R and A** channels, stored
  directly with no colour transform. The stream has no Adobe marker, so
  general JPEG decoders report it as CMYK. The library maps the components
  across unchanged ([#11](https://github.com/NCrashed/JuicyPixels-blp/pull/11),
  [#17](https://github.com/NCrashed/JuicyPixels-blp/issues/17)).
- **Frame size.** The decoder reads each level's frame size before decoding
  it. A frame larger than the level is treated as damage, and a smaller one
  is padded with transparent black to the level size, as Warcraft III does.
  The level size comes from the BLP header, which a forged file controls as
  well, so the decoder also refuses full size images above `blpMaxPixels`
  (8192×8192 by default) before allocating anything. `blpInfo` reads the
  size without decoding.
- **JuicyPixels.** The JPEG streams themselves are decoded by JuicyPixels.
  Errors it raises are turned into `Left`, but some corrupt streams make it
  loop forever
  ([Juicy.Pixels#230](https://github.com/haskell-github-trust/Juicy.Pixels/issues/230)),
  so decoding untrusted files needs a timeout.
- **Header size.** Warcraft III crashes on a `jpegHeaderSize` above 624.
  The encoder shares the longest common prefix of all levels, up to 624
  bytes, and leaves every level at least one byte of its own
  ([#18](https://github.com/NCrashed/JuicyPixels-blp/issues/18)).
- **Unused alpha plane.** Without alpha, the plane should be all `0xFF`, and
  the encoder writes it that way. Tools in the wild fill an unused plane with
  either all `0x00` or all `0xFF`. The decoder therefore treats a plane that
  is entirely `0x00` or entirely `0xFF` as opaque, and uses any other plane,
  including a uniformly semi-transparent one, as it is.
- **Other JPEG kinds.** Streams that are not four-component, such as
  greyscale or YCbCr, are decoded as ordinary JPEG.

## Paletted content (`content = 1`)

```
uint32 palette[256]          -- 0x00RRGGBB, stored as B, G, R, padding
per mip level:
  uint8 index[w * h]         -- palette index per pixel, rows top to bottom
  uint8 alpha[(w * h * alphaBits + 7) / 8]
```

- **Palette padding byte.** The fourth byte of each palette entry is padding
  and carries no alpha. The decoder ignores it
  ([#14](https://github.com/NCrashed/JuicyPixels-blp/issues/14)).
- **Alpha.** Alpha values are packed least-significant bit first. The
  decoder handles 1, 4 and 8 bits and scales them to 0–255; the encoder
  writes 8-bit alpha.
- **Short palettes.** Palettes shorter than 256 entries, where the first mip
  starts earlier than `0x9C + 1024`, are accepted and padded
  ([#6](https://github.com/NCrashed/JuicyPixels-blp/pull/6)).
- **Encoder.** It quantises colours to 256 with JuicyPixels' median-cut
  palette, with dithering unless `blpDither` is off, and writes 8-bit alpha
  or none. With alpha, the colour of fully transparent pixels is treated as
  black when building the palette, so colours hidden under them take no
  entries; they are invisible in the decoded texture either way.

## BLP2 (World of Warcraft)

The library reads BLP2 files; it does not write them. The sections above
apply where noted.

### Header

The header is 148 bytes (`0x94`) long, followed by a 256 entry palette
(1024 bytes) whether the content uses it or not.

| Offset | Type | Name | Library field | Meaning |
|---|---|---|---|---|
| `0x00` | `char[4]` | magic | – | `"BLP2"` |
| `0x04` | `uint32` | type | – | `0` = JPEG, `1` = everything else |
| `0x08` | `uint8` | encoding | `blp2Content` | `1` = paletted, `2` = DXT, `3` = BGRA |
| `0x09` | `uint8` | alphaDepth | `blp2AlphaDepth` | Alpha bits per pixel: 0, 1, 4 or 8 |
| `0x0A` | `uint8` | alphaType | `blp2AlphaType` | DXT format: `0` = DXT1, `1` = DXT3, `7` = DXT5; 8 on other content |
| `0x0B` | `uint8` | hasMips | `blp2HasMips` | `0` = only mip 0 |
| `0x0C` | `uint32` | width | `blp2Width` | Width of mip 0 in pixels |
| `0x10` | `uint32` | height | `blp2Height` | Height of mip 0 in pixels |
| `0x14` | `uint32[16]` | mipmapOffset | – | File offset of each mip level |
| `0x54` | `uint32[16]` | mipmapSize | – | Size in bytes of each mip level |
| `0x94` | `uint32[256]` | palette | – | B, G, R and padding, as in BLP1 |

What the library does with these fields:

- **type.** Only 1 is read. Type 0 holds JPEG content, which no known file
  uses; it is rejected.
- **encoding** and **alphaType.** Decide the content, below. Other values
  are rejected.
- **alphaDepth.** 0 makes every level opaque, whatever the content holds;
  this is what Pillow does. For DXT1 it also decides what index 3 of a
  three-colour block means: transparent with 1, opaque black with 0.
- **hasMips** is ignored, and the mipmap slots are read as in
  [Mipmaps](#mipmaps), with the same checks. Levels may start right after
  the header; the palette is only read for paletted content, and is cut
  short where mip 0 starts, as in BLP1.

### Paletted content (`encoding = 1`)

Laid out and decoded as [paletted BLP1](#paletted-content-content--1), with
alphaDepth in place of alphaBits.

### DXT content (`encoding = 2`)

Each level is cut into 4×4 blocks, stored row by row; levels whose sides are
not a multiple of 4 are padded to whole blocks, and the padding is dropped
when decoding. A DXT1 block is 8 bytes of colour; DXT3 and DXT5 blocks put 8
bytes of alpha first.

- **Colour.** Two RGB565 endpoints and a 2-bit index per pixel. Endpoints
  expand to 8 bits by bit replication (31 becomes 255). If the first
  endpoint is larger, or the block is DXT3 or DXT5, indices 2 and 3 are
  `(2·c0 + c1) / 3` and `(c0 + 2·c1) / 3`; otherwise index 2 is
  `(c0 + c1) / 2` and index 3 is black.
- **DXT3 alpha.** 4 bits per pixel, times 17.
- **DXT5 alpha.** Two 8-bit endpoints and a 3-bit index per pixel: six
  interpolated values between them if the first is larger, otherwise four,
  plus 0 and 255.

Divisions round down, as in Pillow. Graphics hardware is not consistent
about this, so other decoders can differ by one. Pillow's BLP plugin skips
the bit replication, so its output is up to 7 lower per channel.

### BGRA content (`encoding = 3`)

Four bytes per pixel: blue, green, red and alpha.

### Damaged levels

As in BLP1, a level 0 too short for its size fails the file, and the list of
levels stops at the first damaged mipmap.

## Deviations found in real files

The decoder accepts these, besides the ones mentioned above:

| Deviation | Handling |
|---|---|
| Unusual `extra` values, such as 6 on WoW assets ported to Warcraft III and 0 from community tools | ignored ([#5](https://github.com/NCrashed/JuicyPixels-blp/pull/5), [#10](https://github.com/NCrashed/JuicyPixels-blp/pull/10)) |
| `alphaBits` declared, but mip 0 holds only the index list | read as without alpha ([#8](https://github.com/NCrashed/JuicyPixels-blp/pull/8)) |
| Garbage in unused mipmap slots | slot skipped ([#7](https://github.com/NCrashed/JuicyPixels-blp/pull/7)) |
| Truncated or corrupt tail mipmaps | levels before it kept ([#9](https://github.com/NCrashed/JuicyPixels-blp/pull/9)) |

## Not supported

- **BLP0**, which stores each mip level in a separate `.b00` … `.b15` file.
- **Writing BLP2**, and BLP2 files with JPEG content (`type = 0`).
- **Writing 1-bit or 4-bit alpha**, or a chosen `extra` or `hasMipmaps`
  value, through the public encoders. `Codec.Picture.Blp.Internal.Encoder`
  can write any `BlpStruct`.
