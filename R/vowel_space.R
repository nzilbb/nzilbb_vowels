#' Plot vowel space for speaker or speakers.
#'
#' Given vowel data with the first column identifying speakers, the second
#' identifying vowels, the third containing F1 and the fourth containing F2
#' values, plot a vowel space using the speaker's mean values for each vowel.
#'
#' @param vowel_data data frame of vowel tokens as described above.
#' @param speakers list of speaker identifiers for speaker whose vowel space
#' is to be plotted.
#' @param vowel_colours a named list of vowel = colour entries to indicate
#' which colour to plot each vowel.
#' @return `ggplot` object.
#' @importFrom dplyr mutate filter summarise group_by vars
#' @importFrom ggplot2 ggplot geom_label facet_wrap scale_colour_manual aes labs
#'   geom_point scale_x_reverse scale_y_reverse expansion geom_point
#' @importFrom ggrepel geom_label_repel
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{pca_contrib_plot(pca_object, pc_no=1, cutoff=50)}
#' \dontrun{pca_contrib_plot(pca_object, pc_no=2, cutoff=70)}
#' @export
plot_vowel_space <- function(vowel_data, speakers = NULL, vowel_colours = NULL) {

  base::stopifnot(
    "Column one must be a factor or character vector of speaker ids." =
      base::is.character(vowel_data[[1]]) | base::is.factor(vowel_data[[1]]),
    "Column two must be a factor or character vector of vowel ids." =
      base::is.character(vowel_data[[2]]) | base::is.factor(vowel_data[[2]]),
    "Column three must be a numeric vector of F1 values." =
      base::is.numeric(vowel_data[[3]]),
    "Column four must be a numeric vector of F2 values." =
      base::is.numeric(vowel_data[[4]])
  )

  # Assume speaker is first column, vowel is second, F1 is third, and F2 is
  # fourth.
  speaker_col_name <- base::names(vowel_data)[[1]]
  vowel_col_name <- base::names(vowel_data)[[2]]
  F1_col_name <- base::names(vowel_data)[[3]]
  F2_col_name <- base::names(vowel_data)[[4]]

  # Determine if more than one speaker is being plotted. If so, include facets.

  if (base::is.null(speakers)) {
    speakers <- vowel_data[[speaker_col_name]] %>% base::unique()
  }

  if (length(speakers) > 1) {
    facet_element <- facet_wrap(vars(.data[[speaker_col_name]]))
  } else {
    facet_element <- NULL
  }

  # Add colours if provided.
  if (base::is.null(vowel_colours)) {
    colour_element <- NULL
  } else {
    colour_element <- scale_colour_manual(values = vowel_colours)
  }

  # Calculate mean F1 and F2 values for each speaker.
  speaker_means <- vowel_data %>%
    filter(
      .data[[speaker_col_name]] %in% speakers
    ) %>%
    group_by(
      .data[[speaker_col_name]],
      .data[[vowel_col_name]]
    ) %>%
    summarise(
      F1 = base::mean(.data[[F1_col_name]]),
      F2 = base::mean(.data[[F2_col_name]])
    )

  # Determine 1/4 of x axis range. Adding this seems to be sufficient to
  # include all labels.
  quarter_range <- base::max(speaker_means$F2) - base::min(speaker_means$F2) / 4

  speaker_means %>%
    ggplot(
      aes(
        x = .data$F2,
        y = .data$F1,
        colour = .data[[vowel_col_name]],
        label = .data[[vowel_col_name]]
      )
    ) +
    geom_label_repel(
      show.legend = FALSE,
      alpha = 0.7,
      min.segment.length = 0
    ) +
    geom_point(show.legend = FALSE) +
    scale_x_reverse(expand = expansion(mult = 0.1)) +
    scale_y_reverse(expand = expansion(mult = 0.1)) +
    colour_element +
    labs(
      x = "F2",
      y = "F1"
    ) +
    facet_element

}


# ADD PLOT VOWEL CHANGE HERE
