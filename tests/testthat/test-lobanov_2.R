library(magrittr)
library(dplyr)

test_that("lobanov_2 adds two columns", {
  expect_equal(length(lobanov_2(onze_vowels)), length(onze_vowels) + 2)
})

test_that("Error if speaker column is numeric", {

  qb_vowels <- qb_vowels %>%
    mutate(
      speaker = 1
    )

  expect_error(qb_vowels %>% lobanov_2(), "factor")
})

test_that("Error if formant columns not numeric", {
  qb_vowels <- qb_vowels |>
    mutate(
      F1_50 = "dog"
    )

  expect_error(qb_vowels %>% lobanov_2(), "numeric")
})

test_that("Vowel exclusion results in different norm values", {
  qb_vowels_1 <- qb_vowels |>
    lobanov_2()

  qb_vowels_2 <- qb_vowels |>
    lobanov_2(exclude = "THOUGHT")

  expect_false(all(qb_vowels_1$F1_lob2 == qb_vowels_2$F1_lob2))
})

test_that("Normalised values haven't changed in qb_vowels.", {
  expect_snapshot(
    lobanov_2(qb_vowels),
    cran = FALSE,
    error = FALSE,
    transform = NULL,
    variant = NULL,
    cnd_class = FALSE
  )
})
