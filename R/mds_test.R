#' Test optimal number of MDS dimensions.
#'
#' `r lifecycle::badge('experimental')` Generate bootstrapped confidence intervals and permutation based null
#' distribution for MDS analysis. Output shows how much stress is reduced by
#' adding an additional dimension to the MDS analysis of `similarity_matrix`,
#' and bootstrapped iterations of `similarity_matrix`,
#' compared with the stress reduction expected from a matrix with no meaningful
#' structure. This function is inspired by [pca_test()], but is less connected
#' with statistical literature than that function. We currently reject
#' additional dimensions is they reduce less stress than we would expect by
#' chance. That is, when the distribution from the boostrapped analyses sits
#' notably lower than the permuted distribution when plotted by [plot_mds_test()]
#'
#' @param similarity_matrix Square matrix of speaker similarity scores.
#' @param n_boots Number of bootstrapping iterations (default: 25).
#' @param n_perms Number of permutations (default: 25).
#' @param test_dimensions Number of MDS dimensions to test for stress reduction (default: 5).
#' @param principal Whether to apply principal axis transform to MDS (default: TRUE)
#' @param mds_type What kind of MDS to apply, see [smacof::smacofSym()] (default: 'ordinal')
#' @param spline_degree How many spline degrees when `type` is 'mspline' (default: 2)
#' @param spline_int_knots How many internal knots when `type` is 'mspline' (default: 2)
#'
#' @importFrom smacof sim2diss smacofSym
#' @importFrom rsample bootstraps
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows mutate group_by lag
#' @importFrom tidyr unnest
#'
#' @returns object of class `mds_test_results`, containing:
#' * `$stress_reduction` a tibble containing
#' * `$n_boots` Number of bootstrapping iterations.
#' * `$n_perms` Number of permutation iterations
#' * `$mds_type` Type of MDS analysis (`type` argument passed to
#' [smacof::smacofSym()])
#' * `$principal` Whether principal axis transformation is applied (passed to
#' [smacof::smacofSym()])
#' @export
#'
#' @examples
#' # Apply interval MDS to `sim_matrix`, with 5 permutations and bootstraps
#' # testing up to 3 dimensions. In real usage, increase `n_boots` and `n_perms`
#' # to at least 50.
#' mds_test(
#'  sim_matrix,
#'  n_boots = 5,
#'  n_perms = 5,
#'  test_dimensions = 3,
#'  mds_type = 'interval'
#' )
#'
mds_test <- function(
  similarity_matrix,
  n_boots = 50,
  n_perms = 50,
  test_dimensions = 5,
  principal = TRUE,
  mds_type = 'ordinal',
  spline_degree = 2,
  spline_int_knots = 2
) {

  # Switch from similarity matrix to dissimilarity matrix
  df <- sim2diss(similarity_matrix, method = "reverse")

  # Set up boostrapping splits
  bootstrap_indices <- bootstraps(
    data.frame(1:nrow(similarity_matrix)),
    times = n_boots
  )

  # set up table to track how much stress ea
  stress_dists <- tibble(
    dims = 1:test_dimensions
  )

  # Determine stress for bootstrapped samples from similarity data.
  stress_dists <- stress_dists |>
    mutate(
      stress_dist = map(
        .data$dims,
        ~ stress_distribution(
          df,
          n_boots,
          .x,
          bootstrap_indices,
          mds_type,
          principal,
          spline_degree,
          spline_int_knots
        )
      )
    )

  stress_dists <- stress_dists |>
    unnest("stress_dist") |>
    mutate(
      stress_dist = as.numeric(.data$stress_dist)
    )

  # Now generate distribution of stress values for permuted version of the
  # dissimilarity matrix.
  perm_dists <- tibble(
    dims = 1:test_dimensions
  )

  perm_dists <- perm_dists |>
    mutate(
      stress_dist = map(
        .data$dims,
        ~ permutation_distribution(
          df,
          n_perms,
          .x,
          mds_type,
          principal,
          spline_degree,
          spline_int_knots
        )
      )
    )

  perm_dists <- perm_dists |>
    unnest("stress_dist") |>
    mutate(
      stress_dist = as.numeric(.data$stress_dist)
    )

  # Get stress with full dataset.
  orig_stress <- tibble(
    dims = 1:test_dimensions,
    stress_dist = map_dbl(
      1:test_dimensions,
      ~ (smacofSym(
        df,
        type = mds_type,
        principal = principal,
        ndim = .x,
        spline.degree = spline_degree,
        spline.intKnots = spline_int_knots,
        weightmat = NULL,
        init = "torgerson",
        ties = "primary",
        verbose = FALSE,
        relax = FALSE,
        modulus = 1,
        itmax = 1000,
        eps = 1e-06
      ))$stress
    )
  )

  # join both permed, bootstrapped, and full analysis
  merged <- bind_rows(
    "permuted" = perm_dists,
    "boot" = stress_dists,
    "original" = orig_stress,
    .id = "source"
  )

  # Calculate stress reduction
  merged <- merged |>
    mutate(
      cumulative = 1
    ) |>
    group_by(.data$source, .data$dims) |>
    mutate(
      cumulative = cumsum(.data$cumulative)
    )

  merged <- merged |>
    group_by(.data$source, .data$cumulative) |>
    mutate(
      lag_stress = lag(.data$stress_dist, default = 1L),
      diff = .data$lag_stress - .data$stress_dist
    ) |>
    ungroup()

  # Output all information
  mds_test_results <- list(
    "stress_reduction" = merged,
    "n_boots" = n_boots,
    "n_perms" = n_perms,
    "mds_type" = mds_type,
    "principal" = principal
  )

  class(mds_test_results) <- "mds_test_results"

  mds_test_results

}

stress_distribution <- function(
    dis_matrix,
    n_bootstraps,
    n_dim,
    bs_indices,
    mds_type,
    principal,
    spline_degree,
    spline_int_knots
) {

  bootstrap_dist <- map(
    1:n_bootstraps,
    ~ (
      bs_indices$splits[[.x]]$in_id |>
        generate_bootstrap(dis_matrix = dis_matrix) |>
        # Specifying arguments to avoid relying on defaults.
        smacofSym(
          type = mds_type,
          principal = principal,
          ndim = n_dim,
          spline.degree = spline_degree,
          spline.intKnots = spline_int_knots,
          weightmat = NULL,
          init = "torgerson",
          ties = "primary",
          verbose = FALSE,
          relax = FALSE,
          modulus = 1,
          itmax = 1000,
          eps = 1e-06
        )
    )$stress
  )
}

generate_bootstrap <- function(splits, dis_matrix) {

  bs_dis_matrix <- matrix(nrow = length(splits), ncol = length(splits))

  for (i in seq_along(splits)) {
    for (j in seq_along(splits)) {
      bs_dis_matrix[i, j] = dis_matrix[splits[i], splits[j]]
    }
  }

  bs_dis_matrix
}

generate_perm <- function(dis_matrix) {
  perm_matrix <- matrix(nrow = nrow(dis_matrix), ncol = ncol(dis_matrix))

  for (i in 1:nrow(dis_matrix)) {
    perm_matrix[i,] = sample(
      dis_matrix[i, ],
      size = ncol(dis_matrix),
      replace = FALSE
    )
    # 0 is self-dissimilarity.
    perm_matrix[i, i] <- 0
  }
  perm_matrix
}

permutation_distribution <- function(
    dis_matrix,
    n_perms,
    n_dim,
    mds_type,
    principal,
    spline_degree,
    spline_int_knots
)
{
  perm_dist <- map(
    1:n_perms,
    ~ (
      generate_perm(dis_matrix) |>
        smacofSym(
          type = mds_type,
          principal = principal,
          ndim = n_dim,
          spline.degree = spline_degree,
          spline.intKnots = spline_int_knots,
          weightmat = NULL,
          init = "torgerson",
          ties = "primary",
          verbose = FALSE,
          relax = FALSE,
          modulus = 1,
          itmax = 1000,
          eps = 1e-06
        )
    )$stress
  )
}



















