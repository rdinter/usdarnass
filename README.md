
# usdarnass

[![Travis-CI Build
Status](https://travis-ci.org/rdinter/usdarnass.svg?branch=master)](https://travis-ci.org/rdinter/usdarnass)

[![CRAN Last
Release](https://cranlogs.r-pkg.org/badges/version-last-release/usdarnass)](https://cran.r-project.org/package=usdarnass)

[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/usdarnass)](https://cran.r-project.org/package=usdarnass)

An alternative for downloading various USDA data from
<https://quickstats.nass.usda.gov/> through R. You must sign up for an
[API key](https://quickstats.nass.usda.gov/api) from the mentioned
website in order for this package to work. Disclaimer:

> This product uses the NASS API but is not endorsed or certified by
> NASS.

# Install

Thsi package is now on [CRAN](https://cran.r-project.org/) and can be
installed through the typical method:

``` r
install.packages("usdarnass")
```

Alternatively, the most up-to-date version of the package can be
installed with the `devtools` package. However, beware that this will be
a development version:

``` r
# install.packages("devtools")
devtools::install_github("rdinter/usdarnass")
```

Once installed through either method, you can load package the
conventional way:

``` r
library("usdarnass")
```

# Usage

See the [Get
started](http://robertdinterman.com/usdarnass/articles/usdarnass)
article for a more in depth discussion of the package features.

# Documentation

See the [documentation site](http://robertdinterman.com/usdarnass/).

# See Also

There are other `R` packages that work with USDA Quick Stats that may be
more useful to others:

  - [emraher/rnass](https://github.com/emraher/rnass)
  - [potterzot/rnassqs](https://github.com/potterzot/rnassqs)

If you are interested in download all of the Quick Stats data to your
local machine, then their [FTP](ftp://ftp.nass.usda.gov/quickstats/)
site is the best option for downloading all.
