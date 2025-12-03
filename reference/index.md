# Package index

## Data manipulation and generation

Functions for manipulating or generating data.

- [`lobanov_2()`](https://nzilbb.github.io/nzilbb_vowels/reference/lobanov_2.md)
  : Apply Lobanov 2.0 normalisation
- [`pc_flip()`](https://nzilbb.github.io/nzilbb_vowels/reference/pc_flip.md)
  : Flip PC loadings
- [`pca_rotate_2d()`](https://nzilbb.github.io/nzilbb_vowels/reference/pca_rotate_2d.md)
  : Manually rotate two PCs around the origin
- [`pca_rotate_procrustes()`](https://nzilbb.github.io/nzilbb_vowels/reference/pca_rotate_procrustes.md)
  : Apply Procrustes rotation to PCA loadings and scores.
- [`correlation_test()`](https://nzilbb.github.io/nzilbb_vowels/reference/correlation_test.md)
  : Permutation test of pairwise correlations
- [`mds_test()`](https://nzilbb.github.io/nzilbb_vowels/reference/mds_test.md)
  **\[experimental\]** : Test optimal number of MDS dimensions.
- [`pca_test()`](https://nzilbb.github.io/nzilbb_vowels/reference/pca_test.md)
  : PCA with confidence intervals and null distributions
- [`procrustes_loadings()`](https://nzilbb.github.io/nzilbb_vowels/reference/procrustes_loadings.md)
  **\[experimental\]** : Generate distribution of (index) loadings using
  the bootstrap and Procrustes rotation.
- [`permutation_test()`](https://nzilbb.github.io/nzilbb_vowels/reference/permutation_test.md)
  **\[superseded\]** : Run permutation test on PCA analysis.
- [`summary(`*`<correlation_test>`*`)`](https://nzilbb.github.io/nzilbb_vowels/reference/summary.correlation_test.md)
  : Summary function for correlation test object. Set alpha to change
  significance level.

## Plotting functions

Functions for plotting.

- [`plot_correlation_counts()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_correlation_counts.md)
  :

  Plot of correlation counts from `correlation_test` object

- [`plot_correlation_magnitudes()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_correlation_magnitudes.md)
  :

  Plot distribution of correlations from `correlation_test` object

- [`plot_loadings()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_loadings.md)
  :

  Plot PC index loadings from `pca_test` object.

- [`plot_procrustes_loadings()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_procrustes_loadings.md)
  **\[experimental\]** :

  Plot loadings with confidence bands from
  [`procrustes_loadings()`](https://nzilbb.github.io/nzilbb_vowels/reference/procrustes_loadings.md)

- [`plot_mds_test()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_mds_test.md)
  **\[experimental\]** :

  Plot
  [`mds_test()`](https://nzilbb.github.io/nzilbb_vowels/reference/mds_test.md)
  results

- [`plot_pc_input()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_pc_input.md)
  : Plot Scores from Significant PCs Against PCA Input

- [`plot_pc_vs()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_pc_vs.md)
  : Plot PC loadings in vowel space

- [`plot_vowel_space()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_vowel_space.md)
  : Plot vowel space for speaker or speakers.

- [`plot_permutation_test()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_permutation_test.md)
  **\[superseded\]** :

  Create plot from
  [`permutation_test()`](https://nzilbb.github.io/nzilbb_vowels/reference/permutation_test.md).

- [`plot_variance_explained()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_variance_explained.md)
  :

  Create plot of variances explained from `pca_test` object

- [`pca_contrib_plot()`](https://nzilbb.github.io/nzilbb_vowels/reference/pca_contrib_plot.md)
  : PCA contribution plots

## Datasets

Included datasets.

- [`onze_vowels`](https://nzilbb.github.io/nzilbb_vowels/reference/onze_vowels.md)
  : Monophthong data for random sample of speakers from the ONZE corpus
- [`onze_vowels_full`](https://nzilbb.github.io/nzilbb_vowels/reference/onze_vowels_full.md)
  : Monophthong data for speakers from the ONZE corpus
- [`onze_intercepts`](https://nzilbb.github.io/nzilbb_vowels/reference/onze_intercepts.md)
  : Speaker random intercepts from GAMMs for 100 ONZE speakers
- [`onze_intercepts_full`](https://nzilbb.github.io/nzilbb_vowels/reference/onze_intercepts_full.md)
  : Speaker random intercepts for 418 ONZE speakers
- [`qb_vowels`](https://nzilbb.github.io/nzilbb_vowels/reference/qb_vowels.md)
  : Formants from QuakeBox 1
- [`qb_intervals`](https://nzilbb.github.io/nzilbb_vowels/reference/qb_intervals.md)
  : Formant and amplitude for intervals of QuakeBox monologues
- [`sim_matrix`](https://nzilbb.github.io/nzilbb_vowels/reference/sim_matrix.md)
  : Similarity matrix from online perception test.
