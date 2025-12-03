# Plot PC loadings in vowel space

Plot loadings from a PCA analysis carried out on vocalic data. Vowel
positions mean values are at the mean with arrows indicating loadings.
Loadings are multiplied by the standard deviation, by vowel, of the
initial input data. This is OK for getting a quick, intuitive,
interpretation of what the PCs mean in the vowel space. When using a
model-to-PCA pipeline, it is not recommended to use these plots directly
in publications as the models should more reliably control variation in
vocalic readings than taking the standard mean and standard deviation.

## Usage

``` r
plot_pc_vs(vowel_data, pca_obj, pc_no = 1, is_sig = FALSE)
```

## Arguments

- vowel_data:

  A dataframe whose first four columns are speaker ids, vowel ids, F1
  values, and F2 values.

- pca_obj:

  The result of a call to
  [`prcomp()`](https://rdrr.io/r/stats/prcomp.html),
  [`princomp()`](https://rdrr.io/r/stats/princomp.html) or
  [`pca_test()`](https://nzilbb.github.io/nzilbb_vowels/reference/pca_test.md).

- pc_no:

  An integer, indicating which PC to plot (default is PC1).

- is_sig:

  A boolean, indicating whether only 'significant' loadings, according
  to `pca_test` should be plotted (only works with objects of class
  `pca_test_results`).

## Value

a `ggplot` object.

## Examples

``` r
  onze_pca <- prcomp(onze_intercepts |> dplyr::select(-speaker), scale=TRUE)
  # Default is to plot PC1
  plot_pc_vs(onze_vowels, onze_pca)

  # Or plot another PC with `pc_no`
  plot_pc_vs(onze_vowels, onze_pca, pc_no = 3)
```
