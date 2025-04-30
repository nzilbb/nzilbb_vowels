#' Apply procrustes rotation to PCA loadings and scores.
#'
#' @param to_rotate
#' @param target
#' @param pcs
#'
#' @importFrom vegan procrustes
#'
#' @returns
#' @export
#'
#' @examples
pca_rotate_procrustes <- function(
    to_rotate, target, max_pcs, rotate="loadings", rotation_variables = "all"
) {
  # Rotate the 'to rotate' PCA object (either `prcomp` or `princomp`)
  # to maximally match the 'target' matrix.

  stopifnot(
      "Target and PCA to rotate must be produced by the same PCA,
      function,i.e. both by `prcomp` or both by `princomp()`." =
        class(to_rotate) == class(target),
      "rotate must be either 'loadings' or 'scores'." =
        rotate %in% c("loadings", "scores")
  )

  # Determine if pca is result of prcomp() or princomp().
  if (inherits(to_rotate, "prcomp")) {
    scores_var <- "x"
    loadings_var <- "rotation"
  } else if (inherits(to_rotate, "princomp")) {
    scores_var <- "scores"
    loadings_var <- "loadings"
  }

  # check landmarks are the same.
  if (
    any(
      rownames(target[[loadings_var]]) != rownames(to_rotate[[loadings_var]])
    )
  ) {
    warning(
      paste(
        "Variable names or variable order differs between target",
        "and matrix to rotate."
      )
    )
  }

  # Work out how many PCs to include, if max PCs is greater than the number
  # of cols in the matrix to be rotated or the target matrices, pick the smallest
  # of these.
  max_pc = min(
    ncol(to_rotate[[loadings_var]]),
    ncol(target[[loadings_var]]),
    max_pcs
  )

  if (all(rotation_variables == "all")) {
    rotvar_target <- rownames(target[[loadings_var]])
    rotvar_rotate <- rownames(to_rotate[[loadings_var]])
  } else {
    rotvar_target <- rotation_variables
    rotvar_rotate <- rotation_variables
  }

  if (rotate == "loadings") {
    proc <- procrustes(
      target[[loadings_var]][rotvar_target, 1:max_pc],
      to_rotate[[loadings_var]][rotvar_rotate, 1:max_pc],
      scale = FALSE, scores = "sites"
    )
    rot_loadings <- to_rotate[[loadings_var]][, 1:max_pc] %*%
      proc$rotation
    rot_scores <- to_rotate[[scores_var]][, 1:max_pc] %*%
      proc$rotation
  } else if (rotate == "scores") {
    proc <- procrustes(
      target[[scores_var]][, 1:max_pc], to_rotate[[scores_var]][, 1:max_pc],
      scale = FALSE, scores = "sites"
    )
    scores_transform <- matrix(
      rep(proc$xmean, times = nrow(to_rotate[[scores_var]])),
      nrow = nrow(to_rotate[[scores_var]]), byrow=TRUE
    )
    rot_scores <- proc$Yrot + scores_transform
    rot_loadings <- to_rotate[[loadings_var]][, 1:max_pc] %*%
      proc$rotation
  }

  # update scores
  to_rotate[[loadings_var]][, 1:max_pc] <- rot_loadings
  to_rotate[[scores_var]][, 1:max_pc] <- rot_scores

  # Change variance explained
  to_rotate[['sdev']] <- map_dbl(
    1:ncol(to_rotate[[scores_var]]),
    \(x) stats::sd(to_rotate[[scores_var]][,x])
  )

  if (inherits(to_rotate, "princomp")) {
    names(to_rotate[['sdev']]) = paste0("Comp.", 1:ncol(to_rotate[[scores_var]]))
  }

  # Add note
  # Add a note to indicate rotation has happened.
  if ("note" %in% names(to_rotate)) {
    to_rotate[["note"]] <- append(
      to_rotate[["note"]],
      paste0(
        "Procrustes rotation applied to PCs 1 through ", max_pc, " with ",
        deparse(substitute(target)), " as target."
      )
    )
  } else {
    to_rotate[["note"]] <- list(
      paste0(
        "Procrustes rotation applied to PCs 1 through ", max_pc, " with ",
        deparse(substitute(target)), " as target."
      )
    )
  }

  to_rotate
}
