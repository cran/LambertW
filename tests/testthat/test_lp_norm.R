context("Testing Lp norm\n")

test_that("lp norm is correct", {
  # Pythagoras
  expect_equal(lp_norm(c(3, 4), p = 2), 5)
  expect_equal(lp_norm(c(3, 4), p = 1), 7)
  for (pp in c(0, 1, 2, 10)) {
    expect_equal(lp_norm(rep(0, 10), p = pp), 0)
  }
  
  kComplexVec <- exp(1i * runif(20, -pi, pi))
  expect_equal(sapply(kComplexVec, lp_norm), rep(1, length(kComplexVec)))
  
  expect_error(lp_norm(10, -1))
})

