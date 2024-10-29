test_that("plot_correlation_counts snapshot", {
  vdiffr::expect_doppelganger(
    title = "plot_correlation_counts",
    fig = {
      set.seed(10)
      onze_cor <- correlation_test(onze_intercepts |> select(-speaker), n=10)
      plot_correlation_counts(onze_cor)
    }
  )
})

test_that("plot_correlation_magnitudes snapshot", {
  vdiffr::expect_doppelganger(
    title = "plot_correlation_mag",
    fig = {
      set.seed(10)
      onze_cor <- correlation_test(onze_intercepts |> select(-speaker), n=10)
      plot_correlation_magnitudes(onze_cor)
    }
  )
})
