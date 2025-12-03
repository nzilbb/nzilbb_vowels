# Plot vowel space for speaker or speakers.

Given vowel data with the first column identifying speakers, the second
identifying vowels, the third containing F1 and the fourth containing F2
values, plot a vowel space using the speaker's mean values for each
vowel. Typically it is best to produce a plot from scratch. The primary
purpose of this function is to generate quick plots for interactive use,
rather than to produce plots for publication.

## Usage

``` r
plot_vowel_space(
  vowel_data,
  speakers = NULL,
  vowel_colours = NULL,
  label_size = 4,
  means_only = TRUE,
  ellipses = FALSE,
  point_alpha = 0.1,
  facet = TRUE
)
```

## Arguments

- vowel_data:

  data frame of vowel tokens as described above.

- speakers:

  list of speaker identifiers for speaker whose vowel space is to be
  plotted.

- vowel_colours:

  a named list of vowel = colour entries to indicate which colour to
  plot each vowel.

- label_size:

  It is often convenient to adjust the size of the labels (in pts).
  Default is 4.

- means_only:

  whether to plot means only or all data points. Default: TRUE.

- ellipses:

  whether to 95% confidence ellipses. Only works if means_only is FALSE.
  Default is FALSE.

- point_alpha:

  alpha value for data points if means_only is FALSE.

- facet:

  whether to plot distinct speakers in distinct facets. Default is TRUE.

## Value

`ggplot` object.

## Examples

``` r
# Plot mean vowel space across
plot_vowel_space(
  onze_vowels,
  speakers = NULL,
  vowel_colours = NULL,
  label_size = 4,
  means_only = TRUE,
  ellipses = FALSE,
  point_alpha = 0.1,
  facet = FALSE
 )
```
