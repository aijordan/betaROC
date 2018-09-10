context("Construction of obsforc object")

obs11 <- c(1, 0, 1)
obs21 <- c(T, F, T)
obs31 <- c("1", "0", "1")
obs12 <- as.factor(obs11)
obs22 <- as.factor(obs21)
obs32 <- as.factor(obs31)

forc11 <- c(0, 1, 2)
forc21 <- c("0", "1", "2")
forc12 <- as.factor(forc11)
forc22 <- as.factor(forc21)

of     <- list(
  x = c(0, 1, 2),
  y = c(1, 0, 1)
)
class(of) <- "obsforc"

test_that("Correct computation of bare obsforc object",{

  expect_that(observationforecast(obs11, forc11), equals(of))

  for(i in 1:3){
    for(ii in 1:2){
      for(j in 1:2){
        for(jj in 1:2){
          observation <- get(paste0("obs", i, ii))
          forecast <- get(paste0("forc", j, jj))
          expect_that(observationforecast(observation, forecast), equals(of))
        }
      }
    }
  }
})

# test_that("Length differences to observationforecast",{
#   throws_error(observationforecast(observation = c("1", "2", "a"), forecast = c(1,2,3)))
#   expect
# })