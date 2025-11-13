# Check if DOIs are not retracted

This function checks if a given list of DOIs (Digital Object
Identifiers) are retracted. It uses the Crossref API to query the
Retraction Watch database.

## Usage

``` r
doi_not_retracted(dois, cache_file = NULL, email = NULL)
```

## Arguments

- dois:

  A character vector of DOIs to be checked.

- cache_file:

  A file name of the cache to be used, i.e. the downloaded retraction
  data. THe file is an \`rds\` file as downloaded from the
  retractionwatch site. If NULL, the data will not be cached.

- email:

  An optional email address to be included in the API request
  \[RECOMMENDET!\].

## Value

A named logical vector indicating whether each DOI is retracted
(\`FALSE\`) or or not (\`TRUE\`), names are the dois.

## Examples

``` r
# Check if a single DOI is retracted
doi_not_retracted("10.1234/abcd")
#> 10.1234/abcd 
#>         TRUE 

# Check if multiple DOIs are retracted
dois <- c("sbcd1234", "10.1234/abcd", "10.1002/jcb.23190", "10.47366/sabia.v5n1a3")
doi_not_retracted(dois)
#>              sbcd1234          10.1234/abcd     10.1002/jcb.23190 
#>                  TRUE                  TRUE                 FALSE 
#> 10.47366/sabia.v5n1a3 
#>                  TRUE 
```
