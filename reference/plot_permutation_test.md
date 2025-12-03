# Create plot from [`permutation_test()`](https://nzilbb.github.io/nzilbb_vowels/reference/permutation_test.md).

**\[superseded\]** Plots results of a permutation test carried out with
the
[`permutation_test()`](https://nzilbb.github.io/nzilbb_vowels/reference/permutation_test.md)
function. Now use either
[`correlation_test()`](https://nzilbb.github.io/nzilbb_vowels/reference/correlation_test.md)
or
[`pca_test()`](https://nzilbb.github.io/nzilbb_vowels/reference/pca_test.md)
and the associated plotting functions.

## Usage

``` r
plot_permutation_test(permutation_results, violin = FALSE)
```

## Arguments

- permutation_results:

  object of class `permutation_results`.

- violin:

  Determines whether the variances explained are depicted by distinct
  violin plots for each PC or by connected lines. the advantage of lines
  is that they correctly indicate that values for each PC depend on one
  another within a given permutation. That is, if an earlier PC soaks up
  a lot of the variation in a data set, then there is less variation
  left to explain by subsequent PCs. Default value is `FALSE`.

## Value

`ggplot` object.

## Examples

``` r
onze_perm <- permutation_test(
  onze_intercepts |> dplyr::select(-speaker),
  pc_n = 5,
  n = 10,
  scale = TRUE,
  cor.method = 'pearson'
 )
plot_permutation_test(onze_perm)
```
