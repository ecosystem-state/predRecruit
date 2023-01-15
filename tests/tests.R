test_that("1D linear model works", {
  set.seed(123)

  response <- data.frame(time = 1:40, dev = rnorm(40))

  predictors <- matrix(rnorm(400), ncol = 10)
  #colnames(predictors) = paste0("X",1:ncol(predictors))
  predictors <- as.data.frame(predictors)
  predictors$time <- 1:40
  lm_example <- univariate_forecast(response,
       predictors,
       model_type = "lm",
       n_forecast = 1,
       n_years_ahead = 0,
       max_vars = 3)
  expect_equal("pred", names(lm_example)[1])
  expect_equal("vars", names(lm_example)[2])
  expect_equal("coefs", names(lm_example)[3])

  pred = predict(lm(response$dev ~ -1 + predictors[,1] + predictors[,2] + predictors[,3]))
  expect_equal(lm_example$pred$est[40], pred[40][[1]])

})

test_that("1D gam model works", {
  set.seed(123)

  response <- data.frame(time = 1:40, dev = rnorm(40))

  predictors <- matrix(rnorm(400), ncol = 10)
  #colnames(predictors) = paste0("X",1:ncol(predictors))
  predictors <- as.data.frame(predictors)
  predictors$time <- 1:40
  gam_example <- univariate_forecast(response,
                                    predictors,
                                    model_type = "gam",
                                    n_forecast = 1,
                                    n_years_ahead = 0,
                                    max_vars = 3)
  expect_equal(gam_example$pred$est[40], -0.08842347)

  set.seed(123)
  gam_example <- univariate_forecast(response,
                                     predictors,
                                     model_type = "gam",
                                     n_forecast = 4,
                                     n_years_ahead = 0,
                                     max_vars = 3)
  expect_equal(gam_example$pred$est[37], -0.0003308037)
  expect_equal(gam_example$pred$est[38], 0.2696979393)
  expect_equal(gam_example$pred$est[39], -0.0094620398)
  expect_equal(gam_example$pred$est[40], -0.0884234683)
})


test_that("1D lasso model works", {
  set.seed(123)

  response <- data.frame(time = 1:40, dev = rnorm(40))

  predictors <- matrix(rnorm(400), ncol = 10)
  #colnames(predictors) = paste0("X",1:ncol(predictors))
  predictors <- as.data.frame(predictors)
  predictors$time <- 1:40
  lm_example <- univariate_forecast_ml(response,
       predictors,
       model_type = "glmnet",
       n_forecast = 1,
       n_years_ahead = 1)
  expect_equal(lm_example$pred$est[40], -0.3595902)

  set.seed(123)
  lm_example <- univariate_forecast_ml(response,
                                       predictors,
                                       model_type = "glmnet",
                                       n_forecast = 4,
                                       n_years_ahead = 1)
  expect_equal(lm_example$pred$est[37], -0.2298422)
  expect_equal(lm_example$pred$est[38], -0.3201412)
  expect_equal(lm_example$pred$est[39], 1.3507670)
  expect_equal(lm_example$pred$est[40], -0.3595902)
})


test_that("1D random forest model works", {
  set.seed(123)

  response <- data.frame(time = 1:40, dev = rnorm(40))

  predictors <- matrix(rnorm(400), ncol = 10)
  #colnames(predictors) = paste0("X",1:ncol(predictors))
  predictors <- as.data.frame(predictors)
  predictors$time <- 1:40
  lm_example <- univariate_forecast_ml(response,
                                       predictors,
                                       model_type = "randomForest",
                                       n_forecast = 1,
                                       n_years_ahead = 1)
  expect_equal(lm_example$pred$est[40], -0.02658667)

  set.seed(123)
  lm_example <- univariate_forecast_ml(response,
                                       predictors,
                                       model_type = "randomForest",
                                       n_forecast = 4,
                                       n_years_ahead = 1)
  expect_equal(lm_example$pred$est[37], -0.2298422)
  expect_equal(lm_example$pred$est[38], -0.3201412)
  expect_equal(lm_example$pred$est[39], 1.3507670)
  expect_equal(lm_example$pred$est[40], -0.3595902)
})
