library(magrittr)
library(dplyr)

test_that("lobanov_2 adds two columns", {
  expect_equal(length(lobanov_2(onze_vowels)), length(onze_vowels) + 2)
})

test_that("Error returned is speaker column is numeric", {

  qb_vowels <- qb_vowels %>%
    mutate(
      speaker = 1
    )

  expect_error(qb_vowels %>% lobanov_2(), "Column one must")
})
