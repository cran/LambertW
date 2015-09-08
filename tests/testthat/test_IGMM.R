context("Testing MLE_LambertW \n")
set.seed(20)
nobs <- 1e3
yy <- rnorm(n = nobs, mean = 3, sd = 0.2)

test_that("IGMM estimates c(mu, sigma) are approx correct for a Normal distribution", {
  for (tt in c("s", "h", "hh")) {
    mod <- IGMM(yy, type = tt)
    # mean is approx equal
    expect_more_than(mod$tau["mu_x"], 3 - 0.2 * 2 / sqrt(nobs))
    expect_less_than(mod$tau["sigma_x"], 3 + 0.2 * 2 / sqrt(nobs))
    
    # TODO: replace with actual CI for sigma
    expect_more_than(mod$tau["sigma_x"], 0.2 - 2 / sqrt(nobs))
    expect_less_than(mod$tau["sigma_x"], 0.2 + 2 / sqrt(nobs))  
    
    other.params <- mod$tau[!grepl("mu_x|sigma_x", names(mod$tau))]
    expect_equal(lp_norm(other.params, 1), 0, tol = 1e-2)
  }
})
