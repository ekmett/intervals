0.9 [2019.05.10]
----------------
* Remove the `Foldable` instances for the `Interval` types from
  `Numeric.Interval` and `Numeric.Interval.NonEmpty`.

0.8.1
-----
* Support `doctest-0.12`

0.8
---
* `Eq` and `Ord` instances are now structural
* Deprecate `elem` and `notElem` in favor of `member` and `nonMember`
* Add `iquot`, `irem`, `idiv`, and `imod` functions
* Relax `Fractional` constraint in `deflate` to `Num`
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-2.0`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.

0.7.2
-----
* Redundant constraint cleanup
* GHC 8 support
* Added a flag for building with 'herbie'

0.7.1.1
-------
* Redundant import cleanup

0.7.1
-----
* Now compatible with GHC 7.10.1-rc1
* Fixed a number of broken `#if` pragmas, fixing previously missing instances.

0.7.0.1
-------
* Removed a couple of unnecessary `Fractional` constraints.

0.7
---
* Corrected the definition of `mignitude`.
* Added a notion of `distance` between intervals

0.6
---
* Added `Numeric.Interval.Exception`. For consistency, we tend to throw exceptions now instead of rely on `NaN` when working with empty intervals.

0.5.1.1
-------
* Misc `doctest` fixes.

0.5.1
-----
* Added `interval` to facilitate the construction of known non-empty intervals.

0.5
---
* The default `Numeric.Interval` now deals more conventionally with empty intervals.
* The old "Kaucher directed interval" behavior is available as `Numeric.Interval.Kaucher`.
* Strictly Non-Empty intervals are now contained in `Numeric.Interval.NonEmpty`
* Renamed `bisection` to `bisect`.
* Added `bisectIntegral`.

0.4.2
-----
* Added `clamp`

0.4
---
* Distributive Interval

0.3
---
* Removed dependency on `numeric-extras`

