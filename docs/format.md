# The BLP1 format

BLP ("Blizzard Picture") is the texture format of Warcraft III (BLP1) and
World of Warcraft (BLP2). This library reads and writes **BLP1**. This page
describes the format as the community has documented it, and states for each
part what the library does with it, including where it falls short.

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

- **Magic.** Only `BLP1` is accepted. BLP0 and BLP2 are rejected (see
  [Not supported](#not-supported)).
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
  encoder always writes 5, which is non-zero.

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

- Slots of size 0, slots running past the end of the file, and slots that
  overlap an earlier one are dropped
  ([#7](https://github.com/NCrashed/JuicyPixels-blp/pull/7)).
- The remaining slots are read in offset order.
- `decodeBlpMipmaps` returns the levels up to the first damaged one, so
  element `i` is always level `i`
  ([#9](https://github.com/NCrashed/JuicyPixels-blp/pull/9),
  [#13](https://github.com/NCrashed/JuicyPixels-blp/issues/13)).
- A damaged mip 0 fails the file.

**Encoder.**

- It writes as many levels as asked for, and always at least mip 0.
- It stops early when *either* side reaches one pixel, so 32×8 ends at 4×1
  ([#26](https://github.com/NCrashed/JuicyPixels-blp/issues/26)).
- `mipMapsUpTo` computes a level count from a minimum side length.
- `blp2any --min-mipmap-size N` with `N > 1` therefore writes a deliberately
  short chain. That is fine for interface images, but risky for model
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
  palette and dithering, and writes 8-bit alpha or none.

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
- **BLP2**, the World of Warcraft variant with DXT compression
  ([#3](https://github.com/NCrashed/JuicyPixels-blp/issues/3)).
- **Writing 1-bit or 4-bit alpha**, or a chosen `extra` or `hasMipmaps`
  value, through the public encoders. `Codec.Picture.Blp.Internal.Encoder`
  can write any `BlpStruct`.
