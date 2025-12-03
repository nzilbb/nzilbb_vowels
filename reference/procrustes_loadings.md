# Generate distribution of (index) loadings using the bootstrap and Procrustes rotation.

**\[experimental\]** Generate distribution of loadings or signed index
loadings for Principal Components. These are used in order to estimate
confidence intervals for loadings and, if signed index loadings are
used, also a null distribution for tests of statistical significance.
Plot the results using
[`plot_procrustes_loadings()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_procrustes_loadings.md).

## Usage

``` r
procrustes_loadings(pca_data, max_pcs, index = TRUE, n = 500, scale = TRUE)
```

## Arguments

- pca_data:

  data fed to the `prcomp` function.

- max_pcs:

  maximum number of PCs to rotate.

- index:

  whether to use signed index loadings rather than loadings (default:
  TRUE)

- n:

  the number of bootstrapped and permuted samples.

- scale:

  whether the variables in `pca_data` should be scaled before PCA
  (default: TRUE)

## Value

a tibble, with columns:

- `source` either "Sampling", "Null" or "Original", identifying where
  the loadings comes from. "Original" identifies loadings from the full
  dataset, "Sampling" identifies loadings from the bootstrapped samples,
  "Null" identifes loadings from permuted versions of the data.

- `id` identifies which iteration of either permutation or bootstrapping
  the loading comes from.

- `variable` indicates the variable corresponding to the loading.

- a column containing the loading for each `PC` up to `max_pcs`.

## Examples

``` r
  proc_loadings <- procrustes_loadings(
    pca_data = onze_intercepts |> dplyr::select(-speaker),
    max_pcs = 3,
    index = TRUE,
    n = 10, # set this to at least 100 in actual use.
    scale = TRUE
   )
```
