# Contributing

Thanks for helping. Most changes here come from real BLP files that do not
decode as they should, so the most useful contribution is a precise
description of such a file and a fix with a test.

## Building and testing

```
nix develop .#ghc914          # or any GHC from 9.4 to 9.14
cabal build all --enable-tests --enable-benchmarks --ghc-options=-Werror
cabal test
```

CI builds every supported GHC with `-Werror`, and GHC 9.4 once more with the
oldest dependency versions the bounds allow. See the README for running the
benchmarks.

## What a pull request needs

- **A test for every fix or new behaviour**, in `test/Main.hs`, that fails
  without the change. Name the issue or PR in the test, for example
  `"... (#25)"`. Build fixtures in code with `test/Fixture.hs` (byte by
  byte for paletted files, patched encoder output for JPEG) rather than
  committing binary files.
- **No new warnings.** CI fails on any.
- **A CHANGELOG entry** under `Unreleased`, written for users: what changed
  and why it matters to them.
- **Docs that match the code.** If you change how a part of the format is
  handled, update `docs/format.md`; if you change the public API, its
  haddock.

## Claims need evidence

Say in the PR description how you checked what you claim, and keep claims
to what you checked.

- **Bug fixes:** the input that triggers the bug and what happened before
  and after.
- **Performance:** `cabal bench` numbers for both versions, built with
  `--ghc-options=-fproc-alignment=64` and run alternately under the same
  load (see the README). Unrelated changes can move timings by 25% through
  code alignment alone.
- **Output changes:** whether the encoder's output changes, and for which
  inputs. Changes that keep the output byte-identical should say how that
  was checked.

## Real-world files

Many fixes come from textures in Warcraft III maps. Please do not attach or
commit Blizzard's assets or other people's work. Describe the header fields
and layout instead (`blp2any stats` groups files by header values and is a
good start), or reproduce the quirk in a synthetic file. If you have a
file you are allowed to share, link it in the issue.

## AI-assisted contributions

Pull requests written with AI tools are welcome and held to the same rules.
Before opening one, make sure the test really fails without the fix, run the
whole suite, and remove any statement in the description or the CHANGELOG
that was not verified. A wrong "this can no longer happen" in the notes costs
users more than a missing line.
