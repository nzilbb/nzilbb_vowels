test_that(
  "princomp output unchanged.",
  {
    onze_pca <- princomp(onze_intercepts |> select(-speaker))
    vdiffr::expect_doppelganger(
      "princomp output", plot_pc_vs(onze_vowels, onze_pca)
    )
  }
)

test_that(
  "prcomp output unchanged.",
  {
    onze_pca <- prcomp(onze_intercepts |> select(-speaker), scale=TRUE)
    vdiffr::expect_doppelganger(
      "prcomp output",
      plot_pc_vs(onze_vowels, onze_pca)
    )
  }
)

test_that(
  "pca_test output unchanged.",
  {
    onze_pca <- pca_test(onze_intercepts |> select(-speaker))
    vdiffr::expect_doppelganger(
      "pca_test output",
      plot_pc_vs(onze_vowels, onze_pca)
    )
  }
)

test_that(
  "`is_sig` functions.",
  {
    onze_pca <- pca_test(onze_intercepts |> select(-speaker))
    vdiffr::expect_doppelganger(
      "pca_test `is_sig` output",
      plot_pc_vs(onze_vowels, onze_pca, is_sig = TRUE)
    )
  }
)
