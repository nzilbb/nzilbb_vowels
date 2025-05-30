% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/procrustes_loadings.R
\name{procrustes_loadings}
\alias{procrustes_loadings}
\title{Generate distribution of (index) loadings using the bootstrap and Procrustes rotation.}
\usage{
procrustes_loadings(pca_data, max_pcs, index = TRUE, n = 500, scale = TRUE)
}
\arguments{
\item{pca_data}{data fed to the \code{prcomp} function.}

\item{max_pcs}{maximum number of PCs to rotate.}

\item{index}{whether to use signed index loadings rather than loadings (default: TRUE)}

\item{n}{the number of bootstrapped and permuted samples.}

\item{scale}{whether the variables in \code{pca_data} should be scaled before PCA (default: TRUE)}
}
\value{
a tibble, with columns:
\itemize{
\item \code{source} either "Sampling", "Null" or "Original", identifying where the
loadings comes from. "Original" identifies loadings from the full dataset,
"Sampling" identifies loadings from the bootstrapped samples, "Null" identifes
loadings from permuted versions of the data.
\item \code{id} identifies which iteration of either permutation or bootstrapping the
loading comes from.
\item \code{variable} indicates the variable corresponding to the loading.
\item a column containing the loading for each \code{PC} up to \code{max_pcs}.
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}} Generate distribution of loadings or
signed index loadings for Principal Components. These are used in order to
estimate confidence intervals for loadings and, if signed index loadings
are used, also a null distribution for tests of statistical significance.
Plot the results using \code{\link[=plot_procrustes_loadings]{plot_procrustes_loadings()}}.
}
\examples{
  proc_loadings <- procrustes_loadings(
    pca_data = onze_intercepts |> dplyr::select(-speaker),
    max_pcs = 3,
    index = TRUE,
    n = 10, # set this to at least 100 in actual use.
    scale = TRUE
   )

}
