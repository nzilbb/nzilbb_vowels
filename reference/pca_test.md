# PCA with confidence intervals and null distributions

Permute and bootstrap data fed to PCA `n` times. Bootstrapped data is
used to estimate confidence bands for variance explained by each PC and
for each loading. Squared loadings are multiplied by the squared
eigenvalue of the relevant PC. This ranks the loadings of PCs which
explain a lot of variance higher than those from PCs which explain less.
This approach to PCA testing follows Carmago (2022) and Vieria (2012).
This approach differs from Carmago's PCAtest package by separating data
generation and plotting.

## Usage

``` r
pca_test(
  pca_data,
  n = 100,
  scale = TRUE,
  variance_confint = 0.95,
  loadings_confint = 0.9
)
```

## Arguments

- pca_data:

  data fed to the `prcomp` function.

- n:

  the number of times to permute and bootstrap that data. **Warning:**
  high values will take a long time to compute.

- scale:

  whether the PCA variables should be scaled (default: TRUE).

- variance_confint:

  size of confidence intervals for variance explained (default: 0.95).

- loadings_confint:

  size of confidence intervals for index loadings (default: 0.9).

## Value

object of class `pca_test_results`, containing:

- `$variance` a tibble containing the variances explained and confidence
  intervals for each PC.

- `$loadings` a tibble containing the index loadings and confidence
  intervals for each variable and PC.

- `$raw_data` a tibble containing the variance explained and loadings
  for each bootstrapped and permuted analysis.

- `$variance_confint` confidence intervals applied to variance
  explained.

- `$loadings_confint` confidence interval applied to loadings.

- `$n` the number of iterations of both permutation and bootstrapping.

## Details

Default confidence bands on variance explained at 0.95 (i.e. alpha of
0.05). In line with Vieria (2012), the default confidence bands on the
index loadings are at 0.9.

See
[`plot_loadings()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_loadings.md)
and
[`plot_variance_explained()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_variance_explained.md)
for useful plotting functions.

## References

Camargo, Arley (2022), PCAtest: testing the statistical significance of
Principal Component Analysis in R. *PeerJ* 10. e12967.
doi:10.7717/peerj.12967

Vieira, Vasco (2012): Permutation tests to estimate significances on
Principal Components Analysis. *Computational Ecology and Software* 2.
103–123.

## Examples

``` r
onze_pca <- pca_test(
  onze_intercepts |> dplyr::select(-speaker),
  n = 10,
  scale = TRUE
)
summary(onze_pca)
#> PCA Permutation and Bootstrapping Test
#> 
#> Iterations: 10
#> 
#> Significant PCs at 0.05 level: PC1, PC2, PC3, PC4, PC5.
#> 
#> Significant loadings at 0.1 level: 
#>  PC1: F1_FLEECE
#>  PC1: F1_GOOSE
#>  PC1: F1_START
#>  PC1: F1_THOUGHT
#>  PC1: F1_TRAP
#>  PC1: F2_FLEECE
#>  PC1: F2_NURSE
#>  PC1: F2_STRUT
#>  PC1: F2_THOUGHT
#>  PC2: F1_DRESS
#>  PC2: F1_FLEECE
#>  PC2: F1_NURSE
#>  PC2: F2_DRESS
#>  PC2: F2_KIT
#>  PC2: F2_LOT
#>  PC2: F2_STRUT
#>  PC2: F2_THOUGHT
#>  PC2: F2_TRAP
#>  PC3: F2_GOOSE
#>  PC3: F2_LOT
#>  PC3: F2_NURSE
#>  PC4: F1_KIT
#>  PC4: F1_LOT
#>  PC5: F1_STRUT
#>  PC5: F2_START
#>  PC5: F2_STRUT
#>  PC6: F1_DRESS
#>  PC6: F1_NURSE
#>  PC6: F2_START
#>  PC8: F1_KIT
#>  PC9: F1_LOT
#>  PC10: F1_THOUGHT
#>  PC11: F2_DRESS
```
