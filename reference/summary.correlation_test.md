# Summary function for correlation test object. Set alpha to change significance level.

Set alpha to change significance level and n_cors to change number of
pairwise correlations given.

## Usage

``` r
# S3 method for class 'correlation_test'
summary(object, alpha = 0.05, n_cors = 5, ...)
```

## Arguments

- object:

  object of class `correlation test`,

- alpha:

  significance level for counting correlation as significant.

- n_cors:

  number of pairwise correlations to list.

- ...:

  additional arguments affecting the summary produced.

## Value

a `glue` object.
