#' Apply Lobanov 2.0 normalisation to first and second formant data.
#'
#' We assume the same structure as the normalisation functions in  the `vowels`
#' package. The first four columns should be:
#'
#' 1. speaker identifiers,
#' 2. vowel identifiers,
#' 3. first formant values in Hertz,
#' 4. second formant values in Hertz.
#'
#' @param vowel_data a data frame with speaker, vowel, F1, and F2 columns.
#' @return input dataframe with additional columns `F1_lob2` and `F2_lob2`,
#'   containing the lobanov normalised F1 and F2 values respectively.
#' @export
#' @examples
#' lobanov_2(vowel_data)
lobanov_2 <- function(vowel_data) {

  # Assume speaker is first column, vowel is second, F1 is third, and F2 is
  # fourth.
  speaker_col_name <- names(vowel_data)[[1]]
  vowel_col_name <- names(vowel_data)[[2]]
  F1_col_name <- names(vowel_data)[[3]]
  F2_col_name <- names(vowel_data)[[4]]

  vowel_data %>%
    group_by(.data[[speaker_col_name]], .data[[vowel_col_name]]) %>%
    mutate(
      vowel_mean_F1 = mean(.data[[F1_col_name]]),
      vowel_mean_F2 = mean(.data[[F2_col_name]]),
      vowel_sd_F1 = sd(.data[[F1_col_name]]),
      vowel_sd_F2 = sd(.data[[F2_col_name]]),
    ) %>%
    group_by(speaker) %>%
    mutate(
      mean_of_means_F1 = mean(vowel_mean_F1),
      mean_of_means_F2 = mean(vowel_mean_F2),
      sd_of_means_F1 = sd(vowel_mean_F1),
      sd_of_means_F2 = sd(vowel_mean_F2)
    ) %>%
    ungroup() %>%
    mutate(
      F1_lob2 = (.data[[F1_col_name]] - mean_of_means_F1)/sd_of_means_F1,
      F2_lob2 = (.data[[F2_col_name]] - mean_of_means_F2)/sd_of_means_F2
    ) %>%
    # Remove working variables.
    select(
      -(vowel_mean_F1:sd_of_means_F2)
    )

}

