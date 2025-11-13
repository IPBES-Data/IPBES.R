# Check if DOIs exist

This function checks if a given list of DOIs exist by sending HTTP
requests to the DOI resolver.

## Usage

``` r
doi_exists(dois, cache_file = NULL)
```

## Arguments

- dois:

  A character vector of DOIs to check.

- cache_file:

  A file name of the cache to be used, i.e. the confirmed existing dois.
  The format is a character vector with the DOIs which exist. If the
  cache exist, it will be updated at the end. Temporary caches will be
  written after 100 checks.

## Value

A named logical vector indicating whether each DOI does exist or not,
names are the dois.

## Details

This function uses the httr package to send HTTP GET requests to the DOI
resolver and checks the response status code. A status code of 200
indicates that the DOI exists, while any other status code indicates
that the DOI does not exist.

## Examples

``` r
dois <- c("sbcd1234", "10.1234/abcd", "10.1002/jcb.23190", "10.47366/sabia.v5n1a3")
doi_exists(dois)
#> 
#> Progress: 1 of 
#> 
#> Progress: 2 of 
#> 
#> Progress: 3 of 
#> 
#> Progress: 4 of 
#> Error in httr2::req_perform(httr2::req_error(httr2::req_retry(httr2::req_throttle(httr2::req_headers(httr2::request(paste0("https://doi.org/",  : 
#>   Failed to perform HTTP request.
#> Caused by error in `curl::curl_fetch_memory()`:
#> ! Timeout was reached [ediciones.unipacifico.edu.co]:
#> Resolving timed out after 10001 milliseconds
#>              sbcd1234          10.1234/abcd     10.1002/jcb.23190 
#>                 FALSE                 FALSE                 FALSE 
#> 10.47366/sabia.v5n1a3 
#>                 FALSE 
# Output: [1] FALSE  TRUE
```
