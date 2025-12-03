# Apply Lobanov 2.0 normalisation

`lobanov_2()` takes a data frame where the first four columns are:

1.  speaker identifiers,

2.  vowel identifiers,

3.  first formant values in Hertz,

4.  second formant values in Hertz.

It returns a dataframe with two additional columns, `F1_lob2` and
`F2_lob2`, containing normalised formant values.

## Usage

``` r
lobanov_2(vowel_data)
```

## Arguments

- vowel_data:

  a dataframe whose first four columns are speaker ids, vowel ids, F1
  values, and F2 values.

## Value

a dataframe matching the input dataframe with additional columns
`F1_lob2` and `F2_lob2`, containing the lobanov normalised F1 and F2
values respectively.

## Details

This functions applies Lobanov 2.0 normalisation presented in Brand et
al. (2021). This variant of Lobanov normalisation is designed to work
for datasets whether the vowel types have different token counts from
one another. The Lobanov 2.0 value for a vowel is given by
\$\$F\_{lobanov2.0_i} = \frac{F\_{raw_i} - \mu(\mu\_{vowel_1}, \ldots,
\mu\_{vowel_n})}{\sigma(\mu\_{vowel_1}, \ldots, \mu\_{vowel_n})}\$\$
where, for ease of notation, we assume all values are from a single
speaker. We signify the n vowel types as vowel_1, ..., vowel_2, while i
indicates the formant number. We implement the function for F1 and F2.

## References

Brand, James, Jen Hay, Lynn Clark, Kevin Watson & Márton Sóskuthy
(2021): Systematic co-variation of monophthongs across speakers of New
Zealand English. Journal of Phonetics. Elsevier. 88. 101096.
doi:10.1016/j.wocn.2021.101096

## Examples

``` r
normed_vowels <- lobanov_2(onze_vowels)
head(normed_vowels)
#>    speaker   vowel F1_50 F2_50 speech_rate gender  yob       word     F1_lob2
#> 1 IA_f_065 THOUGHT   514   868      4.3131      F 1891 word_09539 -0.72128947
#> 2 IA_f_065  FLEECE   395  2716      4.3131      F 1891 word_22664 -1.66034667
#> 3 IA_f_065     KIT   653  2413      4.3131      F 1891 word_02705  0.37559246
#> 4 IA_f_065   DRESS   612  2372      4.3131      F 1891 word_23651  0.05205175
#> 5 IA_f_065   GOOSE   445  2037      4.3131      F 1891 word_06222 -1.26578482
#> 6 IA_f_065   GOOSE   443  2258      4.3131      F 1891 word_06222 -1.28156729
#>      F2_lob2
#> 1 -1.9212428
#> 2  1.4915434
#> 3  0.9319794
#> 4  0.8562628
#> 5  0.2376030
#> 6  0.6457338
```
