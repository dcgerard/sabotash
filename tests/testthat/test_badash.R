context("bad_ash")

test_that("bad_ash runs", {
    betahat   <- stats::rnorm(100)
    sebetahat <- stats::rgamma(100, 1, 1)

    bout1 <- bad_ash(betahat = betahat, sebetahat = sebetahat)
    bout2 <- bad_ash(betahat = betahat, sebetahat = sebetahat, df = 2)
    bout3 <- bad_ash(betahat = betahat, sebetahat = sebetahat, df = 2, mixcompdist = "halfuniform")
    bout4 <- bad_ash(betahat = betahat, sebetahat = sebetahat, df = 2, mixcompdist = "normal")
    bout5 <- bad_ash(betahat = betahat, sebetahat = sebetahat, df = 2, mixcompdist = "+uniform")
    bout6 <- bad_ash(betahat = betahat, sebetahat = sebetahat, df = 2, mixcompdist = "-uniform")
}
)
