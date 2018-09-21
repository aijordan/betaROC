# context("Verification methods for S3 object obsforc")
#
# set.seed(12532453)
#
# obs  <- c(rep(1, 6), rep(0, 4))
# forc <- round(runif(10, 0, 10) + obs * 3, 2)
#
# df   <- data.frame(obs = obs, forc = forc)
# attr(df, "dataset")      <- "unknown"
# attr(df, "forecastname") <- "unknown"
#
# test_that("as.data.frame.obsforc works",{
#   expect_equal(as.data.frame(observationforecast(obs, forc)), df)
# })
#
# p0  <- 4/10
# p1  <- 6/10
# sF0 <- summary(forc[7:10])
# sF1 <- summary(forc[1:6])
#
# sob <- round(rbind(c(p0, p1), cbind(sF0, sF1)), 2)
# dimnames(sob) <- list(c("Prob", dimnames(sob)[[1]][-1]), c("Y=0", "Y=1"))
#
# test_that("summary.obsforc",{
#   expect_equal(summary(observationforecast(obs, forc)), sob)
# })
#
#
#
#
#
#
#
#
#
#
#
#
#
#
