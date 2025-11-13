# Plot Snowball

This function takes a snowball object and a name, and creates two plots:
one sized by cited_by_count and the other by cited_by_count_by_year. The
plots are saved as both PDF and PNG in the specified path.

## Usage

``` r
plot_snowball(snowball, name, path = "figures")
```

## Arguments

- snowball:

  A snowball object containing the data to be plotted.

- name:

  The name to be used in the plot titles and file names.

- path:

  The path where the plot files will be saved. Default is "figures".

## Value

No return value, called for side effects.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_snowball(snowball, "example")
} # }
```
