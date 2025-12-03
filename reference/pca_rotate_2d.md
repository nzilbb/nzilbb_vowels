# Manually rotate two PCs around the origin

It is sometimes convenient to rotate principal components to most
closely align with a sensible interpretation in terms of the original
variables or to compare the results of PCA applied to two distinct
datasets. This function allows for manual 2D rotations of principal
components.

## Usage

``` r
pca_rotate_2d(pca_obj, angle, pcs = c(1, 2))
```

## Arguments

- pca_obj:

  The result of a call to
  [`prcomp()`](https://rdrr.io/r/stats/prcomp.html) or
  [`princomp()`](https://rdrr.io/r/stats/princomp.html). **NB** It does
  not make sense to apply this function to the output of `pca_test`.

- angle:

  A number indicating the number of degrees to rotate around the origin
  clockwise. Negative values will rotated counterclockwise.

- pcs:

  A two-element vector identifying the two PCs to rotate.

## Value

An object matching the class of `pca_obj` with loadings, scores, and
variance explained by each component modified.

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
  pca_obj <- prcomp(onze_intercepts |> dplyr::select(-speaker), scale=TRUE)

  # Rotate PCs 3 and 6 by 10 degrees.
  rotated_pca <- pca_rotate_2d(pca_obj, 10, pcs = c(3,6))
```
