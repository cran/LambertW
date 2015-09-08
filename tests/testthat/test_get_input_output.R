context("Testing Gaussianize \n")

test_that("get_input and get_output are inverse of each other", {
  tau.tmp <- c(mu_x = 0, sigma_x = 1, delta = 0.2)
  xx <- rnorm(100)
  yy <- get_output(xx, tau.tmp)
  xx.hat <- get_input(yy, tau.tmp)
  # they must be equal (except for numerical issues)
  expect_equal(lp_norm(xx - xx.hat, 1), 0) 
})
