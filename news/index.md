# Changelog

## nzilbb.vowels 0.4.2

- In
  [`plot_pc_vs()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_pc_vs.md),
  allow unicode letter class and mark combinations in variable names
- More detail provided for article on use of rotations.
- Remove dependency on `vegan::procrustes()`
- Fix typo in
  [`plot_correlation_counts()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_correlation_counts.md)
- Switch to institutional email due to unreliability of previous email
  and consequent archiving of 0.4.1.

## nzilbb.vowels 0.4.1

CRAN release: 2025-05-06

- Remove `VignetteBuilder` from `DRESCRIPTION`.
- Now
  [`plot_procrustes_loadings()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_procrustes_loadings.md)
  correctly sets the y-axis label to ‘Loading’ or ‘Index loading’.

## nzilbb.vowels 0.4.0

### Breaking changes

- In
  [`mds_test()`](https://nzilbb.github.io/nzilbb_vowels/reference/mds_test.md),
  first argument changed from `similarity_matrix` to
  `dissimilarity_matrix` to match the behaviour expected by users of
  [`smacof::smacofSym()`](https://rdrr.io/pkg/smacof/man/smacofSym.html)

### New features

- New
  [`pca_rotate_2d()`](https://nzilbb.github.io/nzilbb_vowels/reference/pca_rotate_2d.md)
  takes the output of
  [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html) or
  [`stats::princomp()`](https://rdrr.io/r/stats/princomp.html) and
  rotates two specified PCs clockwise by `angle` degrees. Both scores
  and loadings are rotated and the variance explained by each component
  is updates.

- New
  [`pca_rotate_procrustes()`](https://nzilbb.github.io/nzilbb_vowels/reference/pca_rotate_procrustes.md)
  allows Procrustes rotation of the output of
  [`prcomp()`](https://rdrr.io/r/stats/prcomp.html) or
  [`princomp()`](https://rdrr.io/r/stats/princomp.html) to match a
  target configuration of either scores or loadings. Partial overlap of
  variables is enabled by `rotation_variables`. Both scores and loadings
  are rotated and the variance explained by each component is updates.

- New
  [`procrustes_loadings()`](https://nzilbb.github.io/nzilbb_vowels/reference/procrustes_loadings.md)
  generates data to enable calculation of confidence intervals of
  loadings, or confidence intervals and null distributions of index
  loadings, by bootstrapping, permutation, and Procrustes rotation.

- New
  [`plot_procrustes_loadings()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_procrustes_loadings.md)
  provided to plot the output of
  [`procrustes_loadings()`](https://nzilbb.github.io/nzilbb_vowels/reference/procrustes_loadings.md).

- New articles on package website
  <https://nzilbb.github.io/nzilbb_vowels/> covering the ‘model-to-PCA’
  workflow used at NZILBB and the use of rotation with PCA.

## nzilbb.vowels 0.3.1

CRAN release: 2024-11-29

- Documentation fixes to initial CRAN submission (`nzilbb.vowels 0.3`).

## nzilbb.vowels 0.3

- Initial submission to CRAN
- [`mds_test()`](https://nzilbb.github.io/nzilbb_vowels/reference/mds_test.md)
  and
  [`plot_mds_test()`](https://nzilbb.github.io/nzilbb_vowels/reference/plot_mds_test.md)
  added to determine number of dimensions for MDS.
- [`pc_flip()`](https://nzilbb.github.io/nzilbb_vowels/reference/pc_flip.md)
  added to reverse orientation of selected PCs in PCA analysis.

## nzilbb.vowels 0.2.1

- All functions required for [Wilson Black et
  al. (2022)](https://doi.org/10.1111/lnc3.12479)
