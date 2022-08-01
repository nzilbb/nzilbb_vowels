test_that("lobanov_2 adds two columns", {
  expect_equal(length(lobanov_2(onze_vowels)), length(onze_vowels) + 2)
})
