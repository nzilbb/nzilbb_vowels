# Run permutation test on PCA analysis.

**\[superseded\]** Permute data fed to PCA a given number of times,
collecting the number of significant pairwise correlations in the
permuted data and the variances explained for a given number of PCs.

## Usage

``` r
permutation_test(
  pca_data,
  pc_n = 5,
  n = 100,
  scale = TRUE,
  cor.method = "pearson"
)
```

## Arguments

- pca_data:

  data fed to the `prcomp` function. Remove non-continuous variables.

- pc_n:

  the number of PCs to collect variance explained from.

- n:

  the number of times to permute that data. **Warning:** high values
  will take a long time to compute.

- scale:

  whether the PCA variables should be scaled (default = TRUE).

- cor.method:

  method to use for correlations (default = "pearson"). Alternative is
  "spearman".

## Value

object of class `permutation_test`

- `$permuted_variances` n x pc_no matrix of variances explained by first
  pc_no PCs in n permutations of original data.

- `$permuted_correlations` list of length n of significant pairwise
  correlations in n permutations of the data (\<= 0.05).

- `$actual_variances` pc_n x 2 tibble of variances explained by first
  pc_n PCs with original data.

- `$actual_correlations` the number of significant pairwise correlations
  (\<= 0.05) in the original data.

## Details

This function is now superseded. Use
[`correlation_test()`](https://nzilbb.github.io/nzilbb_vowels/reference/correlation_test.md)
for pairwise correlations and
[`pca_test()`](https://nzilbb.github.io/nzilbb_vowels/reference/pca_test.md)
for variance explained and loadings.

## Examples

``` r
permutation_test(
  onze_intercepts |> dplyr::select(-speaker),
  pc_n = 5,
  n = 10,
  scale = TRUE,
  cor.method = 'pearson'
 )
#> $permuted_variances
#>           PC1        PC2        PC3        PC4        PC5
#> 1  0.09749977 0.08053709 0.07490254 0.06797534 0.06582942
#> 2  0.09614605 0.08862427 0.08143212 0.07125520 0.06540196
#> 3  0.09601274 0.08743422 0.08462001 0.07458940 0.06959122
#> 4  0.10227363 0.08548202 0.07840330 0.07660964 0.06972533
#> 5  0.09894546 0.08231594 0.07032241 0.06761381 0.06585873
#> 6  0.09676498 0.08792137 0.08101650 0.07176972 0.06380875
#> 7  0.10345031 0.08843245 0.08033364 0.07595603 0.07250766
#> 8  0.09447521 0.08765234 0.07978291 0.06734720 0.06584082
#> 9  0.09355401 0.08811057 0.07823286 0.07236188 0.06886634
#> 10 0.08942127 0.08622782 0.07959623 0.07777337 0.06815037
#> 
#> $permuted_correlations
#>  [1]  4 11 13 10  7 10 13  7  8 11
#> 
#> $actual_variances
#> # A tibble: 5 × 2
#>   PC    variance_explained
#>   <chr>              <dbl>
#> 1 PC1               0.183 
#> 2 PC2               0.143 
#> 3 PC3               0.0989
#> 4 PC4               0.0835
#> 5 PC5               0.0774
#> 
#> $actual_correlations
#> [1] 69
#> 
#> attr(,"class")
#> [1] "permutation_results"
```
