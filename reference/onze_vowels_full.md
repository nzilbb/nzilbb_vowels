# Monophthong data for speakers from the ONZE corpus

A dataset containing the the first and second formants, speech rate,
gender, and year of birth for 481 speakers from the ONZE corpus. 50
speakers are sampled with birth years before 1900 and 50 sampled with
birth years on or after 1900 to ensure a full span of the time period.
Data is present for the following NZE monophthongs, represented by Wells
lexical sets: DRESS, FLEECE, GOOSE, KIT, LOT, NURSE, START, STRUT,
THOUGHT, TRAP. Data for FOOT is excluded due to low token counts.

## Usage

``` r
onze_vowels_full
```

## Format

A data frame with 414679 rows and 8 variables:

- speaker:

  Anonymised speaker code (factor).

- vowel:

  Variable with Wells lexical sets for 10 NZE monophthongs. Levels:
  DRESS, FLEECE, GOOSE, KIT, LOT, NURSE, START, STRUT, THOUGHT, TRAP
  (factor).

- F1_50:

  First formant, extracted from vowel mid-point using LaBB-CAT interface
  with Praat.

- F2_50:

  Second formant, extracted from vowel mid-point using LaBB-CAT
  interface with Praat.

- speech_rate:

  Average speaker speech rate for whole recording.

- gender:

  Gender of speaker, two levels: "M", "F" (factor).

- yob:

  Year of birth of speaker.

- word:

  Anonymised word code (factor).

## Source

<https://osf.io/q4j29/>

## Details

This dataset is derived from the data made available in the
supplementary materials of Brand et al. (2021).

## References

Brand, James, Jen Hay, Lynn Clark, Kevin Watson & Márton Sóskuthy
(2021): Systematic co-variation of monophthongs across speakers of New
Zealand English. Journal of Phonetics. Elsevier. 88. 101096.
doi:10.1016/j.wocn.2021.101096
