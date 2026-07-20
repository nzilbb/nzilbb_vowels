#' Simulation-based Null Distribution for `pca_test`
#'
#' This function generates a simulation-based null distribution for use with a
#' \code{pca_test} object via the function \code{update_null}, simulating
#' speaker data, computing principal component analysis (PCA) for each
#' simulation replicate, and summarizing the resulting eigenvalues and loadings
#' to provide empirical confidence intervals. Almost all arguments are passed
#' directly to \code{simulate_speakers}.
#'
#' @param grouped_means A data frame or tibble containing per-group means for
#'   each vowel to simulate.
#' @param ri_function A function to which goes from formant data and grouping
#'   variables to per-speaker random intercepts. See vignettes.
#' @param reps Integer. Number of simulation replicates to run. Default value:
#'   100.
#' @param n_speakers Integer. Number of speakers to simulate per replicate. It
#'   is important for this to match the number of speakers in your actual data
#'   as PCA is affected by n. See \code{simulate_speakers}. (default: 100).
#' @param probs Optional probability vector for grouping variables. See
#'   \code{simulate_speakers}. Default is \code{NULL}.
#' @param n_tokens_per_vowel Integer. Number of simulated tokens per vowel per
#'   speaker. See \code{simulate_speakers}. (default: 30).
#' @param min_tokens_per_vowel Integer or \code{NULL}. Minimum number of tokens
#'   per vowel; if \code{NULL}, no minimum is enforced. See
#'   \code{simulate_speakers}. (default: \code{NULL}).
#' @param patterns A named list specifying patterns of covariation to inject
#'   into simulation. This enables further specification of a project-specific
#'   null distribution. E.g. by including a pattern associated with jaw opening
#'   or some other feature of vocalic data you wish to be included in the null.
#'   See \code{simulate_speakers}. (default: empty list).
#' @param pattern_sds Numeric vector. Standard deviations for each patterni. See
#'   \code{simulate_speakers}. (default: empty numeric vector).
#' @param speaker_noise_factor_sd Numeric. Standard deviation for random
#'   between-speaker noise factor. See \code{simulate_speakers}. (default:
#'   0.25).
#' @param scaling_factor_sd Numeric. Standard deviation for per-speaker scaling
#'   factor applied to vowels. See \code{simulate_speakers} (default: 0.1).
#' @param pca_scale Logical. Whether to scale variables in PCA (\code{prcomp}).
#'   See \code{pca_test} (default: \code{TRUE}).
#' @param variance_confint Numeric (0-1). Confidence interval width for variance
#'   (eigenvalue) estimates. See \code{pca_test} (default: 0.95).
#' @param loadings_confint Numeric (0-1). Confidence interval width for
#'   component loadings. See \code{pca_test} (default: 0.9).
#'
#' @importFrom tibble tibble as_tibble
#' @importFrom stringr str_c str_sub
#' @importFrom dplyr mutate select ungroup group_by summarise arrange across
#' @importFrom dplyr any_of
#' @importFrom dplyr unnest
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map map2 map_lgl safely keep
#' @importFrom furrr future_map furrr_options
#' @importFrom stats prcomp quantile
#' @importFrom forcats fct_reorder
#'
#' @return A list with the following components:
#'   \item{var}{A tibble summarising
#'     null-distribution confidence intervals for each principal component's
#'   eigenvalue and proportion of variance explained.}
#'   \item{loadings}{A tibble
#'     summarising null-distribution confidence intervals for squared component
#'     loadings (per variable and component).}
#'   \item{var_confint}{Numeric value of \code{variance_confint} used.}
#'   \item{loadings_confint}{Numeric value of \code{loadings_confint} used.}
#'   \item{n_errors}{Number of simulations where \code{ri_function} or PCA
#'     failed and was skipped.}
#'
#'#' @examples
#' # TODO
#' @export
simulate_null <- function(
  grouped_means,
  ri_function,
  reps = 100,
  n_speakers = 100,
  probs = NULL,
  n_tokens_per_vowel = 30,
  min_tokens_per_vowel = NULL,
  patterns = list(),
  pattern_sds = numeric(0),
  speaker_noise_factor_sd = 0.25,
  scaling_factor_sd = 0.1,
  pca_scale = TRUE,
  variance_confint = 0.95,
  loadings_confint = 0.9
) {
  safe_pca <- safely(
    \(x) {
      sim_dat <- simulate_speakers(
        grouped_means, n_speakers, probs, n_tokens_per_vowel, min_tokens_per_vowel,
        patterns, pattern_sds, speaker_noise_factor_sd, scaling_factor_sd
      )
      prcomp(
        ri_function(sim_dat) |> select(-.data[['speaker']]),
        scale. = pca_scale
      )
    }
  )

  pca_results <- future_map(
    str_c("Repetition_", 1:reps),
    safe_pca,
    .options = furrr_options(seed = TRUE)
  )

  n_errors <- sum(map_lgl(pca_results, \(r) !is.null(r[['error']])))

  pca_list <-
    keep(
      pca_results,
      \(r) is.null(r[['error']])
    ) |>
    map(\(r) r[['result']])

  pca_res <-
    tibble(
      rep = str_c("Repetition_", seq_along(pca_list)),
      pca = pca_list
    ) |>
    mutate(
      eigen = map(
        .data[['pca']],
        \(pca_out) pca_out$sdev^2
      ),
      varexp = map(
        .data[['eigen']],
        \(x) x / sum(x)
      ),
      index_loadings = map2(
        .data[['pca']],
        .data[['eigen']],
        \(pca_obj, e) {
          as_tibble(t(pca_obj$rotation), rownames = "PC") |>
            mutate(across(-.data[['PC']], \(x) x^2 * e))
        }
      )
    )

  new_var_null <- pca_res |>
    select(.data[['rep']], .data[['eigen']], .data[['varexp']]) |>
    mutate(
      PC = future_map(
        .data[['eigen']],
        \(x) str_c("PC", seq_along(x)),
        .options = furrr_options(seed = TRUE)
      )
    ) |>
    unnest(c(.data[['eigen']], .data[['varexp']], .data[['PC']])) |>
    group_by(.data[['PC']]) |>
    summarise(
      low_null = quantile(.data[['eigen']], (1 - variance_confint) / 2),
      high_null = quantile(.data[['eigen']], 1 - (1 - variance_confint) / 2),
      low_null_var = quantile(.data[['varexp']], (1 - variance_confint) / 2),
      high_null_var = quantile(.data[['varexp']], 1 - (1 - variance_confint) / 2),
      .groups = "drop"
    ) |>
    ungroup() |>
    mutate(
      PC = fct_reorder(
        factor(.data[['PC']]),
        as.numeric(str_sub(.data[['PC']], start = 3))
      )
    ) |>
    arrange(.data[['PC']])


  new_loading_null <- pca_res |>
    select(.data[['rep']], .data[['index_loadings']]) |>
    unnest(.data[['index_loadings']]) |>
    pivot_longer(
      cols = -any_of(c("rep", "PC")),
      names_to = "variable",
      values_to = "i_loading"
    ) |>
    group_by(.data[['PC']], .data[['variable']]) |>
    summarise(
      low_null = quantile(.data[['i_loading']], (1 - loadings_confint) / 2),
      high_null = quantile(.data[['i_loading']], 1 - (1 - loadings_confint) / 2),
      .groups = "drop"
    ) |>
    mutate(
      PC = fct_reorder(
        factor(.data[['PC']]),
        as.numeric(str_sub(.data[['PC']], start = 3))
      )
    ) |>
    arrange(.data[['PC']])

  list(
    var = new_var_null,
    loadings = new_loading_null,
    var_confint = variance_confint,
    loadings_confint = loadings_confint,
    n_errors = n_errors
  )
}

#' Update Null Distributions in PCA Test Results
#'
#' Replaces the null distributions (generated via permutation, for
#' interpretation of significance) in an object of class \code{pca_test_results}
#' with new null values provided by the user. The function updates both the
#' variance explained and loadings sections, and recalculates which principal
#' components (PCs) and loadings are considered significant given the new null
#' distributions.
#'
#' @param pca_test_res An object of class \code{pca_test_results}, typically the
#'   output of [pca_test()].
#' @param new_null A named list containing new null data frames/tibbles for
#'   \code{var} (variance), \code{loadings} (index loadings), and their
#'   confidence intervals (\code{var_confint}, \code{loadings_confint}). The
#'   format for these should match the corresponding summary tables in the
#'   original \code{pca_test_res} object.
#' @return An updated \code{pca_test_results} object, with new null
#'   columns/values, recalculated significance flags, and a note in the
#'   \code{$note} field indicating the null distribution was updated. Update
#'   Null Distributions in PCA Test Results
#' @importFrom dplyr select left_join relocate mutate contains all_of
#' @importFrom dplyr everything
#' @importFrom tibble as_tibble
#' @export
update_null <- function(pca_test_res, new_null) {

  # handle variance explained. Remove current variance explained null columns,
  # replace with new ones, relocate to ensure same column order as before.
  pca_test_res[['variance']] <- pca_test_res[['variance']] |>
    select(-contains("null")) |>
    left_join(new_null[['var']], by="PC") |>
    relocate(
      all_of(c("PC", "low_null", "low_null_var", "high_null", "high_null_var"))
    ) |>
    mutate(
      sig_PC = .data[['eigenvalue']] > .data[['high_null']]
    )

  pca_test_res[['variance_confint']] <- new_null[['var_confint']]

  # handle loadings (same pattern as above).
  pca_test_res[['loadings']] <- pca_test_res[['loadings']] |>
    select(-contains('null')) |>
    left_join(new_null[['loadings']], by=c('PC', 'variable')) |>
    relocate(
      all_of(c('PC', 'variable')), contains('null')
    ) |>
    mutate(
      sig_loading = .data[['index_loading']] > .data[['high_null']]
    )

  pca_test_res[['loadings_confint']] <- new_null[['loadings_confint']]

  pca_test_res[['note']] <- "Null distribution updated by simulation."

  pca_test_res
}
