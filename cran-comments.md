## Test environments

* local R installation (Arch Linux), R 4.2.2
* windows-latest (on github actions), R release
* macOS-latest (on github actions), R release
* ubuntu-latest (on github actions), R release
* ubuntu-latest (on github actions), R devel
* ubuntu-latest (on github actions), R oldrel-1
* win-builder (release)
* debian-clang-devel (on rhub)

## R CMD check results

In all CI tests:

* checking CRAN incoming feasibility ... NOTE

In ubuntu-latest for R release, devel and oldrel-1 checks:

* checking installed package size ... NOTE
  installed size is 10.4Mb
  sub-directories of 1Mb or more:
    R      2.7Mb
    data   1.0Mb
    libs   5.6Mb

This is due to `meteoland` v2.0.0 being a bridge version, maintaining methods
with old and new spatial packages (as deprecation of `rgdal`, `rgeos` and
`maptools` will occur in June 2023). In June 2023, old methods will be hard
deprecated and sizes will go back to normal.

## Reverse/Downstream dependencies

`meteoland` has no reverse dependencies
