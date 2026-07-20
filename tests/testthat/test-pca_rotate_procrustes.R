test_that("Procrustes rotation flips `prcomp` loadings", {
  target <- prcomp(onze_intercepts |> select(-speaker), scale=TRUE)
  to_rotate <- target
  # flip first PC
  to_rotate$rotation[, 1] <- -to_rotate$rotation[, 1]
  proc <- pca_rotate_procrustes(to_rotate, target, max_pcs=1)
  expect_true(
    # Precise values can be different, but we will check signs.
    all(sign(target$rotation[,1]) == sign(proc$rotation[,1]))
  )
})
