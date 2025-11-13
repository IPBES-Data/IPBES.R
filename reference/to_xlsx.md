# Convert Snowball to XLSX

This function takes a snowball object and a filename, and converts the
snowball object to an XLSX file.

## Usage

``` r
to_xlsx(snowball, xls_filename = NULL)
```

## Arguments

- snowball:

  A snowball object containing the data to be converted.

- xls_filename:

  If not \``NULL` the name of the XLSX file to be created. if `NULL` the
  data will not be saved in a csv but only returned invisilbly

## Value

Invisibly the `data.frame` generated

## Examples

``` r
if (FALSE) { # \dontrun{
to_xlsx(snowball, "example.xlsx")
} # }
```
