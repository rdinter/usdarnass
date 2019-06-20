
# usdarnass

[![Travis-CI Build
Status](https://travis-ci.org/rdinter/usdarnass.svg?branch=master)](https://travis-ci.org/rdinter/usdarnass)

An alternative for downloading various USDA data from
<https://quickstats.nass.usda.gov/> through R. You must sign up for an
[API key](https://quickstats.nass.usda.gov/api) from the mentioned
website in order for this package to work. Disclaimer:

> This product uses the NASS API but is not endorsed or certified by
> NASS.

# Install

Eventually, this package will be uploaded to
[CRAN](https://cran.r-project.org/) at which point you should be able to
install the package through the typical method:

``` r
install.packages("usdarnass")
```

However, until the package has been submitted to CRAN the only way to
use the package is through the development version (needs devtools
installed):

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
