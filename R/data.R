#' Monophthong data for random sample of speakers from the ONZE corpus.
#'
#' A dataset containing the the first and second formants, speech rate,
#' gender, and year of birth for 100 random speakers from the ONZE corpus. Data
#' is present for the following NZE monophthongs, represented by Wells lexical
#' sets: DRESS, FLEECE, GOOSE, KIT, LOT, NURSE, START, STRUT, THOUGHT, TRAP. Data
#' for FOOT is excluded (as in Brand et al. 2021).
#'
#' Dataset is derived from the data made available in the supplementary materials
#' for Brand et al. (2021).
#'
#' @format A data frame with 101572 rows and 8 variables:
#' \describe{
#'   \item{speaker}{Anonymised speaker code.}
#'   \item{vowel}{Factor variable with Wells lexical sets for 10 NZE monophthongs. Levels:  DRESS, FLEECE, GOOSE, KIT, LOT, NURSE, START, STRUT, THOUGHT, TRAP.}
#'   \item{F1_50}{First formant, extracted from vowel mid-point using LaBB-CAT interface with Praat.}
#'   \item{F2_50}{Second formant, extracted from vowel mid-point using LaBB-CAT interface with Praat.}
#'   \item{speech_rate}{Average speaker speech rate for whole recording.}
#'   \item{gender}{Gender of speaker, two levels: "M", "F".}
#'   \item{yob}{Year of birth of speaker.}
#'   \item{word}{Anonymised word code.}
#' }
#' @source \url{https://osf.io/q4j29/}
"onze_vowels"

#' Speaker random intercepts for a sample of 100 speakers, as given in
#' supplementary materials for Brand et al. 2021.
#'
#' A dataset containing the speaker intercepts extracted from GAMM models fit in
#' Brand et al. 2021. Full details are available in analysis file following the
#' url given below.
#'
#' @format A data frame with 100 rows and 21 variables: \describe{
#'   \item{speaker}{Anonymised speaker code.}
#'   \item{F1_DRESS}{Speaker intercept from GAMM model of DRESS F1.}
#'   \item{F2_DRESS}{Speaker intercept from GAMM model of DRESS F2.}
#'   \item{F1_FLEECE}{Speaker intercept from GAMM model of FLEECE F1.}
#'   \item{F2_FLEECE}{Speaker intercept from GAMM model of FLEECE F2.}
#'   \item{F1_GOOSE}{Speaker intercept from GAMM model of GOOSE F1.}
#'   \item{F2_GOOSE}{Speaker intercept from GAMM model of GOOSE F2.}
#'   \item{F1_KIT}{Speaker intercept from GAMM model of KIT F1.}
#'   \item{F2_KIT}{Speaker intercept from GAMM model of KIT F2.}
#'   \item{F1_LOT}{Speaker intercept from GAMM model of LOT F1.}
#'   \item{F2_LOT}{Speaker intercept from GAMM model of LOT F2.}
#'   \item{F1_NURSE}{Speaker intercept from GAMM model of NURSE F1.}
#'   \item{F2_NURSE}{Speaker intercept from GAMM model of NURSE F2.}
#'   \item{F1_START}{Speaker intercept from GAMM model of START F1.}
#'   \item{F2_START}{Speaker intercept from GAMM model of START F2.}
#'   \item{F1_STRUT}{Speaker intercept from GAMM model of STRUT F1.}
#'   \item{F2_STRUT}{Speaker intercept from GAMM model of STRUT F2.}
#'   \item{F1_THOUGHT}{Speaker intercept from GAMM model of THOUGHT F1.}
#'   \item{F2_THOUGHT}{Speaker intercept from GAMM model of THOUGHT F2.}
#'   \item{F1_TRAP}{Speaker intercept from GAMM model of TRAP F1.}
#'   \item{F2_TRAP}{Speaker intercept from GAMM model of TRAP F2.}
#' }
#' @source \url{https://osf.io/q4j29/}
"onze_intercepts"

#' Vowel data for 77 speakers from the QuakeBox corpus, with 11 in each age category.
#'
#' A dataset containing formant values, amplitude, articulation rate, and
#' following segment data for 10 New Zealand English monophthongs, along with
#' participant demographics.
#'
#' Original data was generated for Wilson Black et al. 2022.
#'
#' @format A data frame with 26331 rows and 14 variables:
#' \describe{
#'   \item{speaker}{Anonymised speaker code.}
#'   \item{vowel}{Factor variable with Wells lexical sets for 10 NZE monophthongs. Levels:  DRESS, FLEECE, GOOSE, KIT, LOT, NURSE, START, STRUT, THOUGHT, TRAP, FOOT.}
#'   \item{F1_50}{First formant in Hz, extracted from vowel mid-point using LaBB-CAT interface with Praat.}
#'   \item{F2_50}{Second formant in Hz, extracted from vowel mid-point using LaBB-CAT interface with Praat.}
#'   \item{participant_age_category}{Age category of speaker. Levels: 18-25, 26-35, 36-45, ..., 76-85.}
#'   \item{participant_gender}{Gender of participant. Levels: M, F.}
#'   \item{participant_nz_ethnic}{New Zealand ethnic category of participant. Levels: NZ mixed ethnicity, NZ European, Other.}
#'   \item{word_freq}{Frequency of word from which vowel token is taken in CELEX.}
#'   \item{word}{Anonymised word id. Allows for fitting models with word as random effect without revealing content of transcript.}
#'   \item{time}{Time in seconds at which vowel segment starts.}
#'   \item{vowel_duration}{Length of vowel in seconds.}
#'   \item{articulation_rate}{Articulation rate of utterance from which token is taken.}
#'   \item{following_segment_category}{Category of following segment. NB: liquids have already been removed. Levels: labial, velar, other.}
#'   \item{amplitude}{Maximum amplitude of word from which vowel token is taken, generated by LaBB-CAT interface with Praat.}
#'}
#'
#' @source \url{https://github.com/JoshuaWilsonBlack/Amplitude_F1/}
"qb_vowels"

#' Mean formant, articulation rate, and amplitude values for intervals generated
#' for QuakeBox monologues. The same 77 speakers as in `qb_vowels` are used.
#'
#' QuakeBox monologues are divided into intervals of fixed length within mean
#' values are calcualted for formants, amplitude, and articulation rate.
#'
#' Two interval lengths are given: 60 seconds and 240 seconds.
#'
#' Formant data is z-scored by speaker and vowel, while the amplitude and
#' articulation rate are z-scored by speaker.
#'
#' Original data was generated for Wilson Black et al. 2022.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{interval_length}{Length of interval in seconds.}
#'   \item{speaker}{Anonymised speaker code.}
#'   \item{interval}{Time in seconds at which interval ends.}
#'   \item{articulation_rate}{Mean articulation rate within interval.}
#'   \item{amplitude}{Mean maximum amplitude within interval.}
#'   \item{DRESS_F1}{Speaker intercept from GAMM model of DRESS F1.}
#'   \item{DRESS_F2}{Speaker intercept from GAMM model of DRESS F2.}
#'   \item{FLEECE_F1}{Speaker intercept from GAMM model of FLEECE F1.}
#'   \item{FLEECE_F2}{Speaker intercept from GAMM model of FLEECE F2.}
#'   \item{GOOSE_F1}{Speaker intercept from GAMM model of GOOSE F1.}
#'   \item{GOOSE_F2}{Speaker intercept from GAMM model of GOOSE F2.}
#'   \item{KIT_F1}{Speaker intercept from GAMM model of KIT F1.}
#'   \item{KIT_F2}{Speaker intercept from GAMM model of KIT F2.}
#'   \item{LOT_F1}{Speaker intercept from GAMM model of LOT F1.}
#'   \item{LOT_F2}{Speaker intercept from GAMM model of LOT F2.}
#'   \item{NURSE_F1}{Speaker intercept from GAMM model of NURSE F1.}
#'   \item{NURSE_F2}{Speaker intercept from GAMM model of NURSE F2.}
#'   \item{START_F1}{Speaker intercept from GAMM model of START F1.}
#'   \item{START_F2}{Speaker intercept from GAMM model of START F2.}
#'   \item{STRUT_F1}{Speaker intercept from GAMM model of STRUT F1.}
#'   \item{STRUT_F2}{Speaker intercept from GAMM model of STRUT F2.}
#'   \item{THOUGHT_F1}{Speaker intercept from GAMM model of THOUGHT F1.}
#'   \item{THOUGHT_F2}{Speaker intercept from GAMM model of THOUGHT F2.}
#'   \item{TRAP_F1}{Speaker intercept from GAMM model of TRAP F1.}
#'   \item{TRAP_F2}{Speaker intercept from GAMM model of TRAP F2.}
#' }
#' @source \url{http://www.diamondse.info/}
"qb_intervals"
