## Resubmission

This is a resubmission that has made two major corrections:

1. Expnaded all of the acronyms used in the documentation.
2. Does not change working directories or write to a user's workspace by default. The `nass_set_key()` is the main culprit for this and no has a default to not overwrite an existing .Renviron file with the API key needed for the package.

## Release Summary

This is the first release of 'usdarnass'. 'usdarnass' provides an interface to the Quick Stats API from the United States Department of Agriculture (USDA). The API requires a key to make any queries, which restricts this package's usage to those that have an API key. There are no restrictions from the USDA to obtain a key.

## Test environments
* local ubuntu 18.04.1, R 3.6.0
* ubuntu 14.04.5 (on travis-ci), R 3.5.2

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.