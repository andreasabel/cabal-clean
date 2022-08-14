-- | Common data structures and types for the project.

module Types where

import Util

-- | We treat the architecture identifier as opaque.
type Arch = String

-- | A package is given by its name.
type Package = String

-- | A package version is a list of natural numbers.
type PackageVersion  = NumericVersion

-- | A GHC major version is a list of natural numbers.
type MajorVersion = NumericVersion

-- | A GHC minor version is a list of natural numbers.
type MinorVersion = NumericVersion

type CompilerVersion = (MajorVersion, MinorVersion)

type NumericVersion = [Int]

-- * Parsing directory names

type CompilerString = String

parseCompilerString :: CompilerString -> Maybe CompilerVersion
parseCompilerString s = do
  n <- findIndex (== '-') s
  case splitAt n s of
    ("ghc", _:v) -> splitAt 2 <$> parseVersionString v
    _ -> Nothing

type PackageString = String

parsePackageString :: PackageString -> Maybe (Package, PackageVersion)
parsePackageString s = do
  n <- findIndexEnd (== '-') s
  let (p, _:v) = splitAt n s
  (p,) <$> parseVersionString v

type VersionString = String

parseVersionString :: VersionString -> Maybe NumericVersion
parseVersionString = mapM readMaybe . splitWhen (== '.')

printNumericVersion :: NumericVersion -> VersionString
printNumericVersion = intercalate "." . map show

toNumericVersion :: CompilerVersion -> NumericVersion
toNumericVersion = uncurry (++)
