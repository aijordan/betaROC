# context("Verification input acceptance and rejection for S3 object obsforc")
#
# of     <- list(
#   obs  = c(1, 0, 1),
#   forc = c(0, 1, 2)
# )
# class(of) <- "obsforc"
# attr(of, "dataset") <- "unknown"
# attr(of, "forecastname") <- "unknown"
#
# obs11 <- c(1, 0, 1)
# obs21 <- as.logical(obs11)
# obs31 <- as.character(obs11)
# obs41 <- as.integer(obs11)
# obs12 <- as.factor(obs11)
# obs22 <- as.factor(obs21)
# obs32 <- as.factor(obs31)
# obs42 <- as.factor(obs41)
#
# forc11 <- c(0, 1, 2)
# forc21 <- as.character(forc11)
# forc31 <- as.integer(forc11)
# forc12 <- as.factor(forc11)
# forc22 <- as.factor(forc21)
# forc32 <- as.factor(forc31)
#
# test_that("permissible standard input is accepted",{
#
#   expect_equal(observationforecast(obs11, forc11),of)
#
#   for(i in 1:4){
#     for(ii in 1:2){
#       for(j in 1:3){
#         for(jj in 1:2){
#           obs  <- get(paste0("obs", i, ii))
#           forc <- get(paste0("forc", j, jj))
#           expect_equal(observationforecast(obs, forc), of)
#         }
#       }
#     }
#   }
#
# })
#
#
# obs1  <- c(0, 1, 0, 1)
# forc1 <- c(7, 0, 1, 2)
#
# test_that("permissible standard input with missing values is accepted",{
#
#   for(i in 1:5){
#
#     if(i %% 2 == 1){
#       obs  <- obs1
#       if(i == 1) forc <- c(NA, forc1[-1])
#       if(i == 3) forc <- c(NaN, forc1[-1])
#       if(i == 5) forc <- c(Inf, forc1[-1])
#     }
#     if(i %% 2 == 0){
#       forc <- forc1
#       if(i == 2) obs <- c(NA, obs1[-1])
#       if(i == 4) obs <- c(NaN, obs1[-1])
#     }
#
#     expect_equal(observationforecast(obs, forc), of)
#   }
# })
#
#
# obs1  <- c(1, 0, 1)
# iobs1 <- c("1", "2", "a")
# iobs2 <- c(1, 0, 1, 1)
# iobs3 <- c(0,1,-1)
# iobs4 <- c(0, 2, 1)
# iobs5 <- c(NA, 1, 0)
# iobs6 <- c(0, 1, NaN)
#
# forc1   <- c(0, 1, 2)
# iforc1  <- c("1", "2", "a")
# iforc2  <- 1:4
#
# mess1 <- "observation and forecast have different lengths"
# mess2 <- "forecast can not be transformed into numeric vector"
# mess3 <- "observation can not be transformed into numeric vector"
# mess4 <- "observation is not binary"
#
# test_that("incorrectly specified input throws an error",{
#
#   forc <- forc1
#
#   for(i in 1:4){
#     obs  <- get(paste0("iobs", i))
#     mess <- ifelse(i == 1, mess3, ifelse(i == 2, mess1, mess4))
#
#     expect_that(
#       observationforecast(obs, forc),
#       throws_error(regexp = mess)
#     )
#   }
#
#   obs <- obs1
#
#   for(i in 1:2){
#     forc  <- get(paste0("iforc", i))
#     mess  <- ifelse(i == 1, mess2, mess1)
#
#     expect_that(
#       observationforecast(obs, forc),
#       throws_error(regexp = mess)
#     )
#   }
# })
#
# attr(of, "dataset") <- "TEST_DATA"
# attr(of, "forecastname") <- "TEST_FORECAST"
#
# test_that("attributes are set correctly",{
#
#   expect_equal(
#     observationforecast(obs11, forc11, "TEST_DATA", "TEST_FORECAST"),
#     of
#   )
#
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
