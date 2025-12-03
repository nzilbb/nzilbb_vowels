# Permutation test of pairwise correlations

Permute data a given number (n) of times, collecting pairwise
correlations and testing them for significance. See
[`plot_correlation_magnitudes()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_correlation_magnitudes.md)
and
[`plot_correlation_counts()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_correlation_counts.md)
for plotting functions which take the output of this function.

## Usage

``` r
correlation_test(pca_data, n = 100, cor.method = "pearson")
```

## Arguments

- pca_data:

  dataframe or matrix containing only continuous variables. (as accepted
  by the `prcomp` function.)

- n:

  the number of times (integer) to permute that data. **Warning:** high
  values will take a long time to compute. Default: 100.

- cor.method:

  method to use for correlations (default = "pearson"). Alternative is
  "spearman" (see [`?cor.test`](https://rdrr.io/r/stats/cor.test.html)).

## Value

object of class `correlation_test`, with attributes:

- `$permuted_correlations` A tibble of length n of pairs from the
  original data, their correlations, and the significance of each
  correlation (as p-values).

- `$actual_correlations` the correlations of each pair of variables in
  the original data and their significance (as p-values).

- `$iterations` the number of permutations carried out.

- `$cor_method` the form of correlation used.

## Examples

``` r
  # get a small sample of random intercepts.
  pca_data <- onze_intercepts |>
    dplyr::select(-speaker) |>
    dplyr::slice_sample(n=10)

  # apply correlation test with 5 permutations.
  # actual use requires at least 100.
  cor_test <- correlation_test(pca_data, n = 5, cor.method = 'pearson')
  # Return summary of significant correlations
  summary(cor_test)
#> Correlation test results.
#> Count of significant pairwise correlations in original data at alpha = 0.05: 13
#> Mean significant pairwise correlations in permuted data (n = 5) at alpha = 0.05: 9
#> Min = 4, Max = 13.
#> 
#> Top 5 pairwise correlations in original data:
#> F2_GOOSE, F2_THOUGHT: -0.9
#> F1_GOOSE, F2_THOUGHT: -0.85
#> F2_DRESS, F2_KIT: 0.8
#> F1_KIT, F1_TRAP: -0.8
#> F1_START, F1_THOUGHT: -0.75

  # use spearman correlation instead.
  cor_test_spear <- correlation_test(pca_data, n = 10, cor.method = 'spearman')
```
