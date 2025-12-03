# Apply Procrustes rotation to PCA loadings and scores.

It is sometimes convenient to rotate principal components to align PCA
applied to one dataset with PCA applied to another. This function allows
for Procrustes rotation of Principal Components, without scaling. That
is, we rotate and/or flip loadings or scores so that the PCA analyses to
be rotated most closely matches the loadings (or scores) from the target
PCA analysis.

## Usage

``` r
pca_rotate_procrustes(
  to_rotate,
  target,
  max_pcs,
  rotate = "loadings",
  rotation_variables = "all"
)
```

## Arguments

- to_rotate:

  an object of class `princomp` or `prcomp`.

- target:

  an object of class `princomp` or `prcomp`

- max_pcs:

  an integer. Rotation will be applied from PC1 up to `max_pcs`.

- rotate:

  a string, either "loadings" or "scores", to identify whether the
  loadings of `to_rotate` should be aligned with `target` or the scores
  (default: "loadings")

- rotation_variables:

  a string, names of variables to be used in the rotation. Applied to
  rotation of loadings when two datasets have only partial overlap of
  variables. (default: "all", which uses all variables).

## Value

an object matching the class of `to_rotate`.

## Details

**NB: rotated components are not principal components.** They no longer
explain maximal variance. Rotated components should not be referred to
as 'principal components'. The simplest approach is just to call them
'components' after describing the rotation. This function modifies
objects of the class 'prcomp' and 'princomp', adding an additional
'note' which collects all the rotations which have been applied. This
allows any plotting function which works with the outputs of
[`prcomp()`](https://rdrr.io/r/stats/prcomp.html) or
[`princomp()`](https://rdrr.io/r/stats/princomp.html) to work. This may
result in plots which incorrectly identify rotated components as
principal components. Be careful not to include any such plot in a
research output.

## Examples

``` r
  # PCA on a subset of ONZE speakers
  onze_pca <- prcomp(
    onze_intercepts |> dplyr::select(-speaker),
    scale = TRUE
   )

   # PCA on all ONZE speakers
   onze_full <- prcomp(
    onze_intercepts_full |> dplyr::select(-speaker),
    scale = TRUE
   )

   # rotate subset to match loadings of `onze_full`
   rotated_pca <- onze_pca |>
     pca_rotate_procrustes(
       onze_full, max_pcs = 5
     )
```
