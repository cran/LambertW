context("Testing support of distribution\n")

test_that("support is correct for corner cases", {
  for (gg in c(0, 0.1)) {
    expect_equal(get_support(tau = c(mu_x = 0, sigma_x = 1, gamma = gg), TRUE),
                 c(0, Inf),
                 check.names = FALSE)
  }
  
  for (dd in c(0, 0.2, 1)) {
    expect_equal(get_support(tau = c(mu_x = 0, sigma_x = 1, delta = dd), TRUE),
                 c(0, Inf),
                 check.names = FALSE)
  }
  
  for (dd in c(0, 0.2, 1)) {
    expect_equal(get_support(tau = c(mu_x = 0, sigma_x = 1, delta = dd), FALSE),
                 c(-Inf, Inf),
                 check.names = FALSE)
  }
})