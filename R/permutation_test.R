#' Run permutation test on PCA analysis.
#'
#' Permute data fed to PCA a given number of times, collecting the number of
#' significant pairwise correlations in the permuted data and the variances
#' explained for a given number of PCs.
#'
#' @param pca_data data fed to the `prcomp` function.
#' @param pc_n the number of PCs to collect variance explained from.
#' @param n the number of times to permute that data. **Warning:** high values
#'   will take a long time to compute.
#' @param scale whether the PCA variables should be scaled (default = TRUE).
#' @param cor.method method to use for correlations (default = "pearson").
#'   Alternative is "spearman".
#' @returns object of class `permutation_test`
#' * `$permuted_variances` n x pc_no matrix of variances explained by first
#' pc_no PCs in n permutations of original data.
#' * `$permuted_correlations` list of length n of significant pairwise
#' correlations in n permutations of the data (<= 0.05).
#' * `$actual_variances` pc_n x 2 tibble of variances explained by first pc_n
#' PCs with original data.
#' * `$actual_correlations` the number of significant pairwise correlations (<=
#' 0.05) in the original data.
#' @importFrom tidyr expand
#' @export
#' @examples
#' permutation_test(pca_data, pc_n = 5, n = 100, scale = TRUE, cor.method = 'pearson')
#' permutation_test(pca_data, pc_n = 10, n = 500, scale = FALSE, cor.method = 'spearman')
permutation_test <- function(
  pca_data, pc_n = 5, n=100, scale = TRUE, cor.method = 'pearson'
) {

  # Assume data of form | speaker | variable_1 | variable_2 | ...

  # Do this as a for loop to avoid memory issues with massively duplicating
  # the dataset.

  # Make it so the speaker column doesn't have to be _called_ speaker.

  # Collect real info
  actual_pca <- prcomp(pca_data %>% select(-speaker), scale = scale)
  actual_explained <- tibble(
    PC = c(glue("PC{seq(1:pc_n)}")),
    variance_explained = (actual_pca$sdev^2/sum(actual_pca$sdev^2))[1:pc_n]
  )


  # Actual corrs. (TODO: REFACTOR THIS)
  pairwise_correlations <- as_tibble(names(pca_data)) %>%
    filter(value != "speaker") %>%
    expand(value, value1 = value) %>%
    filter(value < value1)

  pairwise_correlations <- pairwise_correlations %>%
    mutate(
      cor_p = map2(
        value,
        value1,
        ~ cor.test(
          pca_data %>%
            pull(.x),
          pca_data %>%
            pull(.y),
          method=cor.method,
          exact=FALSE # Only active is spearman chosen.
        )[["p.value"]]
      )
    )

  actual_sig_cors <- pairwise_correlations %>%
    filter(
      cor_p <= 0.05
    ) %>%
    nrow()

  # Loop goes here (Accumulator)
  variances_explained <- matrix(
    ncol=pc_n,
    nrow=n,
    dimnames = list(
      seq(1:n),
      c(glue("PC{seq(1:pc_n)}"))
    )
  )
  sig_correlations <- rep(0, n)
  for (i in 1:n) {
    permuted <- pca_data %>%
      mutate(
        across(
          !speaker,
          ~ sample(.x, length(.x))
        )
      )

    # Correlations (REFACTOR - SEE ABOVE!)
    pairwise_correlations <- as_tibble(names(permuted)) %>%
      filter(value != "speaker") %>%
      expand(value, value1 = value) %>%
      filter(value < value1)

    pairwise_correlations <- pairwise_correlations %>%
      mutate(
        cor_p = map2(
          value,
          value1,
          ~ cor.test(
            permuted %>%
              pull(.x),
            permuted %>%
              pull(.y),
            method=cor.method,
            exact=FALSE # Only active is spearman chosen.
          )[["p.value"]]
        )
      )

    sig_correlations[i] <- pairwise_correlations %>%
      filter(
        cor_p <= 0.05
      ) %>%
      nrow()

    permuted_pca <- prcomp(permuted %>% select(-speaker), scale = scale)

    variances_explained[i, ] <- (
      permuted_pca$sdev^2 / sum(permuted_pca$sdev^2)
    )[1:pc_n]

  }

  permutation_results <- list(
    permuted_variances = variances_explained,
    permuted_correlations = sig_correlations,
    actual_variances = actual_explained,
    actual_correlations = actual_sig_cors
  )

  class(permutation_results) <- "permutation_results"

  permutation_results

}
