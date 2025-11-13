# Plot an Interactive Citation Network from a Snowball Search

This function creates a interactive snowball seaarch network using the
networkD3 package.

## Usage

``` r
plot_snowball_interactive(snowball, key_works, file)
```

## Arguments

- snowball:

  The snowball object containing the network data. The object is
  returned from the
  [oa_snowball](https://docs.ropensci.org/openalexR/reference/oa_snowball.html)
  function in the \`openalexR“ package

- key_works:

  A data frame, as returned. e.g. by `oa_fetch(entity = "works", ...`,
  containing the key-works from the snowball search which will be
  highlighted in the network.

- file:

  The file name to save the network to. TThe directory has tro esxist.
  Default: `NULL`, i.e. not saved.

## Value

A networkD3 object representing the interactive network plot.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_snowball_interactive(snowball, key_works, file)
} # }
```
