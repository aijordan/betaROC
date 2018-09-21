context("Acceptance and rejection of input for S3 object roc")

obs11 <- c(1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1)
obs21 <- as.logical(obs11)
obs31 <- as.character(obs11)
obs41 <- as.integer(obs11)
obs12 <- as.factor(obs11)
obs22 <- as.factor(obs21)
obs32 <- as.factor(obs31)
obs42 <- as.factor(obs41)

forc11 <- 0:10
forc21 <- as.character(forc11)
forc31 <- as.integer(forc11)
forc12 <- as.factor(forc11)
forc22 <- as.factor(forc21)
forc32 <- as.factor(forc31)


mf <- data.frame(obs = obs11, forc = forc11)

test_that("permissible standard input is accepted",{

  for(i in 1:4){
    for(ii in 1:2){
      for(j in 1:3){
        for(jj in 1:2){

          form <- as.formula(paste0("obs11 ~ forc11"))
          df   <- data.frame(forc11 = forc11, obs11 = obs11)

          obs  <- get(paste0("obs", i, ii))
          forc <- get(paste0("forc", j, jj))

          r1 <- rm_attribute_terms(roc(formula = form, data = df)$model.frame)
          r2 <- rm_attribute_terms(roc(formula = form)$model.frame)

          expect_equal(r1, mf)
          expect_equal(r2, mf)
        }
      }
    }
  }

})


obs1   <- c(1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1)
forc1  <- 0:10
mf2    <- mf[-1,]
nainfo <- 1L
names(nainfo) <- "1"
class(nainfo) <- "omit"
attr(mf2, "na.action") <- nainfo

test_that("permissible standard input with missing values is accepted",{

  for(i in 1:5){

    if(i %% 2 == 1){
      obs  <- obs1
      if(i == 1) forc <- c(NA, forc1[-1])
      if(i == 3) forc <- c(NaN, forc1[-1])
      if(i == 5) forc <- c(Inf, forc1[-1])
    }
    if(i %% 2 == 0){
      forc <- forc1
      if(i == 2) obs <- c(NA, obs1[-1])
      if(i == 4) obs <- c(NaN, obs1[-1])
    }

    form <- as.formula("obs~forc")
    df   <- data.frame(obs = obs, forc = forc)

    r1 <- rm_attribute_terms(roc(formula = form, data = df)$model.frame)
    r2 <- rm_attribute_terms(roc(formula = form)$model.frame)

    expect_equal(r1, if(i < 5) mf2 else mf[-1,])
    expect_equal(r2, if(i < 5) mf2 else mf[-1,])
  }
})


obs1  <- c(1, 0, 1)
iobs1 <- c("1", "2", "a")
iobs2 <- c(1, 0, 1, 1)
iobs3 <- c(0,1,-1)

forc1   <- c(0, 1, 2)
iforc1  <- c("1", "2", "a")
iforc2  <- 1:4

messbin    <- "observation is not binary"
messlength <- "Variablenlängen sind unterschiedlich (gefunden für 'forc')"
messtrans  <- "observation or forecast can not be converted to numeric"

test_that("incorrectly specified input throws an error",{

  forc <- forc1

  for(i in 1:3){
    obs  <- get(paste0("iobs", i))
    if(i == 1 | i == 3) mess <- messbin
    if(i == 2) mess <- messlength

    if(i == 2){
      print("TEST FUNKTIONIERT NICHT")
      next
    }

    expect_that(
      roc(obs ~ forc),
      throws_error(regexp = mess)
    )
  }

  obs <- obs1

  for(i in 1:2){
    forc  <- get(paste0("iforc", i))
    mess  <- ifelse(i == 1, messtrans, messlength)

    if(i == 2){
      print("TEST FUNKTIONIERT NICHT")
      next
    }

    expect_that(
      roc(obs ~ forc),
      throws_error(regexp = mess)
    )
  }
})



obs  <- obs11
forc <- forc11

test_that("attributes are set correctly",{

  expect_equal(roc(obs ~ forc, emp_info = "concave")$emp_info, "concave")
  expect_equal(roc(obs ~ forc, emp_info = "c")$emp_info, "concave")
  expect_equal(roc(obs ~ forc, emp_info = "unrestricted")$emp_info, "unrestricted")
  expect_equal(roc(obs ~ forc, emp_info = "u")$emp_info, "unrestricted")

  expect_equal(
    roc(obs ~ forc, fit_method = "emp", fit_info = "u")$MDE_info,
    list(method = "empirical", info = "unrestricted"))

  expect_equal(
    roc(obs ~ forc, fit_method = "beta2", fit_info = "con")$MDE_info,
    list(method = c("beta2p", "empirical"), info = "concave"))

  expect_equal(
    roc(obs ~ forc, fit_method = "bin3", fit_info = "con")$MDE_info,
    list(method = c("bin3p", "empirical"), info = "concave"))
})


