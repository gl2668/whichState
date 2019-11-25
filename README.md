
# whichState

<!-- badges: start -->
<!-- badges: end -->

The goal of whichState is to make it easy for you to find out which American State a phone number belongs to.

## Installation

You can install the released version of whichState from [Github](https://github.com/gl2668/whichState) with:

``` r
install.packages("devtools")
devtools::install_github("gl2668/whichState")
```

## Example

This is a basic example of how the package works. If I want to find out which state the phone number 212-123-4567 is from, I will use the following:

``` r
library(whichState)
## whichState(2121234567, usa=TRUE)
## "New York State"
```

