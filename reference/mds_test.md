# Test optimal number of MDS dimensions.

**\[experimental\]** Generate bootstrapped confidence intervals and
permutation based null distribution for MDS analysis. Output shows how
much stress is reduced by adding an additional dimension to the MDS
analysis of `dissimilarity_matrix`, and bootstrapped iterations of
`dissimilarity_matrix`, compared with the stress reduction expected from
a matrix with no meaningful structure. This function is inspired by
[`pca_test()`](https://nzilbb.github.io/nzilbb_vowels/reference/pca_test.md),
but is less connected with statistical literature than that function. We
currently reject additional dimensions is they reduce less stress than
we would expect by chance. That is, when the distribution from the
boostrapped analyses sits notably lower than the permuted distribution
when plotted by
[`plot_mds_test()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_mds_test.md)

## Usage

``` r
mds_test(
  dissimilarity_matrix,
  n_boots = 50,
  n_perms = 50,
  test_dimensions = 5,
  principal = TRUE,
  mds_type = "ordinal",
  spline_degree = 2,
  spline_int_knots = 2,
  ...
)
```

## Arguments

- dissimilarity_matrix:

  Square matrix of dissimilarity scores.

- n_boots:

  Number of bootstrapping iterations (default: 25).

- n_perms:

  Number of permutations (default: 25).

- test_dimensions:

  Number of MDS dimensions to test for stress reduction (default: 5).

- principal:

  Whether to apply principal axis transform to MDS (default: TRUE)

- mds_type:

  What kind of MDS to apply, see
  [`smacof::smacofSym()`](https://rdrr.io/pkg/smacof/man/smacofSym.html)
  (default: 'ordinal')

- spline_degree:

  How many spline degrees when `type` is 'mspline' (default: 2)

- spline_int_knots:

  How many internal knots when `type` is 'mspline' (default: 2)

- ...:

  Arguments passed to
  [`smacof::smacofSym()`](https://rdrr.io/pkg/smacof/man/smacofSym.html)

## Value

object of class `mds_test_results`, containing:

- `$stress_reduction` a tibble containing

- `$n_boots` Number of bootstrapping iterations.

- `$n_perms` Number of permutation iterations

- `$mds_type` Type of MDS analysis (`type` argument passed to
  [`smacof::smacofSym()`](https://rdrr.io/pkg/smacof/man/smacofSym.html))

- `$principal` Whether principal axis transformation is applied (passed
  to
  [`smacof::smacofSym()`](https://rdrr.io/pkg/smacof/man/smacofSym.html))

## Examples

``` r
# Apply interval MDS to `sim_matrix`, with 5 permutations and bootstraps
# testing up to 3 dimensions. In real usage, increase `n_boots` and `n_perms`
# to at least 50.
mds_test(
 smacof::sim2diss(sim_matrix, method="reverse"),
 n_boots = 5,
 n_perms = 5,
 test_dimensions = 3,
 mds_type = 'interval'
)
#> $stress_reduction
#> # A tibble: 33 × 6
#>    source    dims stress_dist cumulative lag_stress  diff
#>    <chr>    <int>       <dbl>      <dbl>      <dbl> <dbl>
#>  1 permuted     1       0.545          1      1     0.455
#>  2 permuted     1       0.543          2      1     0.457
#>  3 permuted     1       0.543          3      1     0.457
#>  4 permuted     1       0.548          4      1     0.452
#>  5 permuted     1       0.537          5      1     0.463
#>  6 permuted     2       0.363          1      0.545 0.182
#>  7 permuted     2       0.368          2      0.543 0.175
#>  8 permuted     2       0.360          3      0.543 0.182
#>  9 permuted     2       0.363          4      0.548 0.184
#> 10 permuted     2       0.360          5      0.537 0.177
#> # ℹ 23 more rows
#> 
#> $n_boots
#> [1] 5
#> 
#> $n_perms
#> [1] 5
#> 
#> $mds_type
#> [1] "interval"
#> 
#> $principal
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "mds_test_results"
```
