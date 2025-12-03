# Formants from QuakeBox 1

A dataset containing formant values, amplitude, articulation rate, and
following segment data for 10 New Zealand English monophthongs, along
with participant demographics.

## Usage

``` r
qb_vowels
```

## Format

A data frame with 26331 rows and 14 variables:

- speaker:

  Anonymised speaker code (char).

- vowel:

  Wells lexical sets for 10 NZE monophthongs. Levels: DRESS, FLEECE,
  GOOSE, KIT, LOT, NURSE, START, STRUT, THOUGHT, TRAP, FOOT (char).

- F1_50:

  First formant in Hz, extracted from vowel mid-point using LaBB-CAT
  interface with Praat.

- F2_50:

  Second formant in Hz, extracted from vowel mid-point using LaBB-CAT
  interface with Praat.

- participant_age_category:

  Age category of speaker. Values: 18-25, 26-35, 36-45, ..., 76-85
  (char).

- participant_gender:

  Gender of participant. Values: M, F (char).

- participant_nz_ethnic:

  New Zealand ethnic category of participant. Values: NZ mixed
  ethnicity, NZ European, Other (char).

- word_freq:

  Frequency of word from which vowel token is taken in CELEX.

- word:

  Anonymised word id (char).

- time:

  Time in seconds at which vowel segment starts.

- vowel_duration:

  Length of vowel in seconds.

- articulation_rate:

  Articulation rate of utterance from which token is taken.

- following_segment_category:

  Category of following segment. NB: liquids have already been removed.
  Levels: labial, velar, other (factor).

- amplitude:

  Maximum amplitude of word from which vowel token is taken, generated
  by LaBB-CAT interface with Praat.

## Source

<https://osf.io/m8nkh/>

## Details

Original data was generated for Wilson Black et al. (2023).

## References

Wilson Black, Joshua, Jennifer Hay, Lynn Clark & James Brand (2023): The
overlooked effect of amplitude on within-speaker vowel variation.
Linguistics Vanguard. Walter de Gruyter GmbH. 9(1). 173–189.
doi:10.1515/lingvan-2022-0086
