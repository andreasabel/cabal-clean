[![Hackage version](https://img.shields.io/hackage/v/cabal-clean.svg?label=Hackage&color=informational)](http://hackage.haskell.org/package/cabal-clean)
[![cabal-clean on Stackage Nightly](https://stackage.org/package/cabal-clean/badge/nightly)](https://stackage.org/nightly/package/cabal-clean)
[![Stackage LTS version](https://www.stackage.org/package/cabal-clean/badge/lts?label=Stackage)](https://www.stackage.org/package/cabal-clean)
[![Cabal build](https://github.com/andreasabel/cabal-clean/workflows/Haskell-CI/badge.svg)](https://github.com/andreasabel/cabal-clean/actions)

cabal-clean : Remove superseded artefacts of `cabal v2-build`
=============================================================

Removes compilation artefacts in `dist-newstyle/build` from older
versions of the package or superseded minor versions of GHC.

For the impatient
-----------------

### Installation

    cabal install cabal-clean

### Run

    cd $MY_PROJECT
    cabal-clean

This lists the build artifacts under `dist-newstyle/build`.
```diff
---	191M	dist-newstyle/build/x86_64-osx/ghc-7.10.3/$MY_PROJECT-2.9.2
---	 72M	dist-newstyle/build/x86_64-osx/ghc-8.10.4/$MY_PROJECT-2.9.2
---	162M	dist-newstyle/build/x86_64-osx/ghc-9.0.1/$MY_PROJECT-2.9.2
+++	135M	dist-newstyle/build/x86_64-osx/ghc-7.10.3/$MY_PROJECT-2.9.3
---	 70M	dist-newstyle/build/x86_64-osx/ghc-8.10.4/$MY_PROJECT-2.9.3
+++	145M	dist-newstyle/build/x86_64-osx/ghc-8.10.5/$MY_PROJECT-2.9.3
+++	159M	dist-newstyle/build/x86_64-osx/ghc-9.0.1/$MY_PROJECT-2.9.3
```
The superseded ones, printed in red and prefixed by dashes (`---`),
can then be removed by:

    cabal-clean --delete

Rationale
---------

`v2-cabal` (the nix-based `cabal`) maintains a directory structure for
local builds of the form
`dist-newstyle/build/$ARCH/$HC/$PACKAGE-$VERSION` (plus other stuff
that does not take up much disk space).  During active development
with several `$HC` versions and `$VERSION` bumps for the `$PACKAGE`,
lots of out-dated build artefacts accumulate over time.

A simple way to clean up is removing the whole `dist-newstyle` folder,
but one might want to keep the build artefacts of the most recent
package `$VERSION`s of the most recent versions of the Haskell
compiler (`$HC`).

Philosophy
----------

- Go for saving the most disk space with the simplest approach, rather
  than a complete clean-up.  E.g., don't care about outdated contents
  in `dist-newstyle/package-db` as they take little space.

- Keep only the most recent `$VERSION` of the package.

- Keep only the most recent major versions of `$HC`.

- Assume a monopoly of GHC, ignoring other Haskell compilers, so only
  treat `$HC`s of the form `ghc-$GHCVER`.

- Work autonomously, ignoring `cabal` files.
  This saves us parsing various home-grown `cabal` file formats.
  The latter could be easy using the `Cabal` package,
  but this package is not very stable,
  and we shy the maintenance effort of depending on `Cabal`.

- Dry-run is the default, giving the user opportunity to review the clean-up plan.

Functionality
-------------

- Read the contents of `dist-newstyle/build`,
  organizing them into a tree according to the pattern
  `$ARCH/ghc-$GHCVER/$PACKAGE-$VERSION`.

- Display the outdated versions.

  Try to get the disk usage with `du -h` and display it.
  (I could not find a Haskell library that gets the disk usage OS-agnostically.)

- With option `--delete` actually remove the respective folders.

Examples
--------

List build artifacts of current project,
marking superseded ones that can be deleded:

    cabal-clean

Actually delete superseded builds:

    cabal-clean --delete

Delete superseded builds without changing to directory:

    cabal-clean --delete path/to/my/project/dist-newstyle

Delete superseded builds in many projects:

    find . -name "dist-newstyle" -exec cabal-clean --delete {} \;

Get help:

    cabal-clean --help

Related
-------

`cabal v2-clean` (as of 2021-08-16)

  - Removes *all* build artefacts.
  - Does not have a `--dry-run` preview.
