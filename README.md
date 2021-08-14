cabal-clean : Remove outdated compilation artefacts from `dist-newstyle`
========================================================================

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
