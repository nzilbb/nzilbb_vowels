#' Create plot of results of permutation test.
#'
#' Plots results of a permutation test carried out with the `permutation_test`
#' function.
#'
#' @param permutation_results object of class `permutation_results`.
#' @return `ggplot` object.
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_point geom_violin scale_alpha_manual
#'     aes labs
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect contains
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{plot_permutation_test(permutation_results)}
#' @export
plot_permutation_test <- function(permutation_results) {

  variance_explained <- permutation_results$permuted_variances %>%
    as_tibble(.name_repair = 'minimal', rownames = "permutation") %>%
    pivot_longer(
      cols = contains('PC'),
      names_to = "PC",
      values_to = "variance_explained"
    ) %>%
    mutate(
      PC = factor(.data$PC, levels = unique(.data$PC))
    )

  variance_plot <- variance_explained %>%
    ggplot(
      aes(
        x = .data$PC,
        y = variance_explained * 100 # Convert to percentage
      )
    ) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    geom_point(
      data=permutation_results$actual_variances,
      color="red",
      show.legend = TRUE
    ) +
    labs(
      title = paste0(
        "Variance Explained by First ",
        base::nrow(permutation_results$actual_variances),
        " PCs"
      ),
      y = "Variance Explained"
    )

  correlation_plot <- permutation_results$permuted_correlations %>%
    as_tibble() %>%
    ggplot(
      aes(
        x = "",
        y = .data$value
      )
    ) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    geom_point(
      aes(
        x= "",
        y = .data$value
      ),
      data = as_tibble(permutation_results$actual_correlations),
      color = "red"
    ) +
    labs(
      title = "Significant Pairwise Correlations",
      x = NULL,
      y = "Count of Significant Pairwise Correlations"
    )

  correlation_plot + variance_plot + patchwork::plot_annotation(
    title = "Permutation Test Results",
    subtitle = "Comparison of Permuted and Original Data",
    caption = paste0(
      "Violin plots indicate distribution of results from ",
      nrow(permutation_results$permuted_variances),
      " permutations. Red dots indicate values obtained from original data."
    ),
    tag_levels = "A"
  )
}
