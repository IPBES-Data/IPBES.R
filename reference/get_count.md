# Get Count

This function takes a search term and a list of DOIs, and returns the
count of the search term in each DOI. If duplicate DOIs are provided,
the function will stop and throw an error.

## Usage

``` r
get_count(search_term, dois = NULL, ...)
```

## Arguments

- search_term:

  The term to search for.

- dois:

  A list of DOIs to search within. Default is NULL.

- ...:

  Additional Additional filter arguments to pass to \`oa_query\` as

## Value

A named list where the names are the DOIs and the values are the counts
of the search term in each DOI.

## Examples

``` r
if (FALSE) { # \dontrun{
get_count("climate change", c("10.1038/nature12350", "10.1126/science.1259855"))
} # }
```
