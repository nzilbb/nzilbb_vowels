# Plot Scores from Significant PCs Against PCA Input

It is sometimes useful to see the relationship between PCs and the raw
values of the input data fed into PCA. This function takes the results
of running `pca_test`, the scores for each speaker from the pca object,
and the raw data fed into the PCA analysis. In the usual model-to-pca
analysis pipeline, the resulting plot depicts by-speaker random
intercepts for each vowel and an indication of which variables are
significantly loaded onto the PCs. It allows the researcher to visualise
the strength of the relationship between intercepts and PC scores.

## Usage

``` r
plot_pc_input(pca_object, pca_data, pca_test)
```

## Arguments

- pca_object:

  Output of `prcomp`.

- pca_data:

  Data fed into `prcomp`. This should not include speaker identifiers.

- pca_test:

  Output of `pca_test`

## Value

a `ggplot` object.

## Examples

``` r
pca_data <- onze_intercepts |> dplyr::select(-speaker)
onze_pca <- prcomp(pca_data, scale = TRUE)
onze_pca_test <- pca_test(pca_data, n = 5) # Increase n to at least 100 in practice.
plot_pc_input(onze_pca, pca_data, onze_pca_test)
#> `geom_smooth()` using formula = 'y ~ x'

```
