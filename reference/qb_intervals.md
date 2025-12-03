# Formant and amplitude for intervals of QuakeBox monologues

QuakeBox monologues are divided into intervals of fixed length within
mean values are calcualted for formants, amplitude, and articulation
rate. Data from 77 speakers is provide (the same sample as `qb_vowels`).

## Usage

``` r
qb_intervals
```

## Format

A data frame with 53940 rows and 10 variables:

- interval_length:

  Length of interval in seconds.

- speaker:

  Anonymised speaker code (char).

- interval:

  Time in seconds at which interval ends.

- articulation_rate:

  Mean articulation rate within interval.

- amplitude:

  Mean maximum amplitude within interval.

- DRESS_F1:

  Speaker intercept from GAMM model of DRESS F1.

- DRESS_F2:

  Speaker intercept from GAMM model of DRESS F2.

- FLEECE_F1:

  Speaker intercept from GAMM model of FLEECE F1.

- FLEECE_F2:

  Speaker intercept from GAMM model of FLEECE F2.

- GOOSE_F1:

  Speaker intercept from GAMM model of GOOSE F1.

- GOOSE_F2:

  Speaker intercept from GAMM model of GOOSE F2.

- KIT_F1:

  Speaker intercept from GAMM model of KIT F1.

- KIT_F2:

  Speaker intercept from GAMM model of KIT F2.

- LOT_F1:

  Speaker intercept from GAMM model of LOT F1.

- LOT_F2:

  Speaker intercept from GAMM model of LOT F2.

- NURSE_F1:

  Speaker intercept from GAMM model of NURSE F1.

- NURSE_F2:

  Speaker intercept from GAMM model of NURSE F2.

- START_F1:

  Speaker intercept from GAMM model of START F1.

- START_F2:

  Speaker intercept from GAMM model of START F2.

- STRUT_F1:

  Speaker intercept from GAMM model of STRUT F1.

- STRUT_F2:

  Speaker intercept from GAMM model of STRUT F2.

- THOUGHT_F1:

  Speaker intercept from GAMM model of THOUGHT F1.

- THOUGHT_F2:

  Speaker intercept from GAMM model of THOUGHT F2.

- TRAP_F1:

  Speaker intercept from GAMM model of TRAP F1.

- TRAP_F2:

  Speaker intercept from GAMM model of TRAP F2.

## Source

<https://osf.io/m8nkh/>

## Details

Two interval lengths are given: 60 seconds and 240 seconds.

Formant data is z-scored by speaker and vowel, while the amplitude and
articulation rate are z-scored by speaker.

Original data was generated for Wilson Black et al. (2023).

## References

Wilson Black, Joshua, Jennifer Hay, Lynn Clark & James Brand (2023): The
overlooked effect of amplitude on within-speaker vowel variation.
Linguistics Vanguard. Walter de Gruyter GmbH. 9(1). 173–189.
doi:10.1515/lingvan-2022-0086
