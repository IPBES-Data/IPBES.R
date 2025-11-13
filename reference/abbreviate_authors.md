# Abbreviate Authors

This function abbreviates the author names in a given data frame of
works. The output is a list of author names in the format "First Author
et al. (Year)" or "Author1 & Author2 (Year)".

## Usage

``` r
abbreviate_authors(oa_works_df)
```

## Arguments

- oa_works_df:

  A data frame containing the works. It should have columns
  'publication_year' and 'author'. 'author' should be a data frame with
  a column 'au_display_name'. One example is the data frame from
  \`openalexR\`.

## Value

A vector of abbreviated author names and publication years.

## Examples

``` r
if (FALSE) { # \dontrun{
abbreviate_authors(oa_works_df)
} # }
```
