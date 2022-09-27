#' Generate bootstrapped confidence intervals and permutation based null
#' distribution for PCA analysis.
#'
#' Permute and bootstrap data fed to PCA a given number of times. Bootstrapped
#' data is used to estimate confidence bands for variance explained by each PC
#' and for each loading. Squared loadings are multiplied by the squared
#' eigenvalue of the relevant PC. This ranks the loadings of PCs which explain
#' a lot of variance higher than those from PCs which explain less. This
#' approach to PCA testing follows Carmago (2022) and Viera (2012). This
#' approach differs from Carmago's PCAtest package by splitting up the data and
#' plotting functions and adopting more of the 'tidyverse' dialect.
#'
#' @param pca_data data fed to the `prcomp` function.
#' @param n the number of times to permute and bootstrap that data. **Warning:** high values
#'   will take a long time to compute.
#' @param scale whether the PCA variables should be scaled (default = TRUE).
#' @param variance_confint size of confidence intervals for variance explained.
#' @param loadings_confint size of confidence intervals for index loadings.
#' @returns object of class `pca_test`
#' * `$variance_explained` a tibble
#' * `$index_loadings` list of length n of significant pairwise
#' correlations in n permutations of the data (<= 0.05).
#' @importFrom dplyr mutate filter across bind_rows first if_else
#' @importFrom magrittr %>%
#' @importFrom purrr map map_lgl
#' @importFrom tibble tibble as_tibble
#' @importFrom rsample bootstraps
#' @importFrom tidyselect everything
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_reorder
#' @importFrom stringr str_sub str_c
#' @importFrom stats na.omit
#' @importFrom glue glue
#' @examples
#' \dontrun{pca_test(
#'     pca_data,
#'     n = 100,
#'     scale = TRUE,
#'     variance_confint = 0.95,
#'     loadings_confint = 0.9)
#' }
#' @export
pca_test <- function(pca_data, n = 100, scale = TRUE,
                     variance_confint = 0.95, loadings_confint = 0.9) {

  # Check all columns in pca_data are numeric.

  base::stopifnot(
    "All columns of pca_data must be numeric." =
      all(map_lgl(pca_data, ~ base::class(.x) == "numeric"))
  )

  # Run all required PCAs

  ## Original data

  original_pca <- stats::prcomp(pca_data, scale = scale)


  ## Bootrapped data

  data_boots <- bootstraps(pca_data, times = n)

  bootstrapped_pca <- map(
    data_boots$splits,
    ~ stats::prcomp(
      as_tibble(.x),
      scale = scale
    )
  )

  ## Permuted data

  permed_pca <- map(
    1:n,
    ~ stats::prcomp(
      pca_data %>%
        mutate(
          across(
            .cols = everything(),
            .fns = ~ base::sample(.x, length(.x), replace = FALSE)
          )
        ),
      scale = scale
    )
  )

  # Collect loadings and eigenvlaues.

  ## From original data

  original_values <- as_tibble(
    t(original_pca$rotation),
    rownames = "PC"
  ) %>%
    mutate(
      eigenvalue = original_pca$sdev^2,
      variance_explained = .data$eigenvalue / sum(original_pca$sdev^2)
    )

  ## From bootstrapped data

  bootstrapped_values <- map(
    bootstrapped_pca,
    extract_pca_values
  ) %>%
    bind_rows(.id = "iteration")

  ## From permuted data

  permuted_values <- map(
    permed_pca,
    extract_pca_values
  ) %>%
    bind_rows(.id = "iteration")

  # Merge all data and order PC variable numerically
  pca_values <- bind_rows(
    "bootstrapped" = bootstrapped_values,
    "permuted" = permuted_values,
    "original" = original_values,
    .id = "source"
  ) %>%
    mutate(
      PC = fct_reorder(
        .data$PC,
        base::as.numeric(str_sub(.data$PC, start = 3))
      )
    )

  # Pivot longer to generate index loadings
  pca_values <- pca_values %>%
    pivot_longer(
      cols = names(pca_data),
      names_to = "variable",
      values_to = "loading"
    ) %>%
    mutate(
      index_loading = .data$loading^2 * .data$eigenvalue^2
    )

  # Calculate confints for eigenvalues

  variance_half_tail <- (1 - variance_confint) / 2

  pca_values <- pca_values %>%
    group_by(.data$source, .data$PC) %>%
    mutate(
      low_eigenvalue = stats::quantile(.data$eigenvalue, variance_half_tail),
      low_variance_explained = stats::quantile(
        .data$variance_explained,
        variance_half_tail
      ),
      high_eigenvalue = stats::quantile(.data$eigenvalue, 1 - variance_half_tail),
      high_variance_explained = stats::quantile(
        .data$variance_explained,
        1 - variance_half_tail
      ),
    ) %>%
    ungroup()

  variance_summary <- pca_values %>%
    group_by(.data$PC) %>%
    summarise(
      low_null = first(
        na.omit(if_else(source == "permuted", .data$low_eigenvalue, NULL))
      ),
      low_null_var = first(
        na.omit(if_else(source == "permuted", .data$low_variance_explained, NULL))
      ),
      high_null = first(
        na.omit(if_else(source == "permuted", .data$high_eigenvalue, NULL))
      ),
      high_null_var = first(
        na.omit(if_else(source == "permuted", .data$high_variance_explained, NULL))
      ),
      low_confint = first(
        na.omit(if_else(source == "bootstrapped", .data$low_eigenvalue, NULL))
      ),
      low_confint_var = first(
        na.omit(if_else(source == "bootstrapped", .data$low_variance_explained, NULL))
      ),
      high_confint = first(
        na.omit(if_else(source == "bootstrapped", .data$high_eigenvalue, NULL))
      ),
      high_confint_var = first(
        na.omit(if_else(source == "bootstrapped", .data$high_variance_explained, NULL))
      ),
      eigenvalue = first(
        na.omit(if_else(source == "original", .data$eigenvalue, NULL))
      ),
      variance_explained = first(
        na.omit(if_else(source == "original", .data$variance_explained, NULL))
      ),
    ) %>%
    mutate(
      sig_PC = .data$eigenvalue > .data$high_null
    )

  # Calculate confints for loadings

  loadings_half_tail <- (1 - loadings_confint) / 2

  pca_values <- pca_values %>%
    group_by(.data$source, .data$PC, .data$variable) %>%
    mutate(
      low_index = stats::quantile(.data$index_loading, variance_half_tail),
      high_index = stats::quantile(.data$index_loading, 1 - variance_half_tail)
    ) %>%
    ungroup()

  loadings_summary <- pca_values %>%
    group_by(.data$PC, .data$variable) %>%
    summarise(
      low_null = first(
        na.omit(if_else(source == "permuted", .data$low_index, NULL))
      ),
      high_null = first(
        na.omit(if_else(source == "permuted", .data$high_index, NULL))
      ),
      low_confint = first(
        na.omit(if_else(source == "bootstrapped", .data$low_index, NULL))
      ),
      high_confint = first(
        na.omit(if_else(source == "bootstrapped", .data$high_index, NULL))
      ),
      index_loading = first(
        na.omit(if_else(source == "original", .data$index_loading, NULL))
      ),
      loading = first(
        na.omit(if_else(source == "original", .data$loading, NULL))
      )
    ) %>%
    mutate(
      sig_loading = .data$index_loading > .data$high_null
    )

  pca_test_results <- list(
    "variance" = variance_summary,
    "loadings" = loadings_summary,
    "raw_data" = pca_values,
    "variance_confint" = variance_confint,
    "loadings_confint" = loadings_confint,
    "n" = n
  )

  class(pca_test_results) <- "pca_test_results"

  pca_test_results
}


#' @export
summary.pca_test_results <- function(object, ...) {
  significant_pcs <- object$variance %>%
    filter(.data$sig_PC) %>%
    pull(.data$PC)

  significant_loadings <- object$loadings %>%
    filter(.data$sig_loading)

  glue(
    "PCA Permutation and Bootstrapping Test\n\n",
    "Iterations: {object$n}\n\n",
    "",
    "Significant PCs at {1 - object$variance_confint} level: ",
    "{str_c(significant_pcs, collapse = ', ')}.\n\n",
    "Significant loadings at {1 - object$loadings_confint} level: \n\t",
    "{str_c(significant_loadings$PC, significant_loadings$variable, sep = ': ', collapse = '\n\t')}"
  )
}


# Helper function.
extract_pca_values <- function(pca_object) {
  as_tibble(t(pca_object$rotation), rownames = "PC") %>%
    mutate(
      eigenvalue = pca_object$sdev^2,
      variance_explained = .data$eigenvalue / sum(pca_object$sdev^2)
    )
}
