context("bad_ash")

test_that("bad_ash runs", {
    betahat   <- stats::rnorm(100)
    sebetahat <- stats::rgamma(100, 1, 1)

    bout <- bad_ash(betahat = betahat, sebetahat = sebetahat)
}
)
