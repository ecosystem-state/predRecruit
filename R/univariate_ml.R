#' Forecast recruitment deviations as a function of covariates using random forest and lasso models
#'
#' \code{univariate_forecast_ml} vector of responses and dataframe of predictors and automates
#'
#' @param response A data frame of responses for the modeling (values to be forecast) containing
#' a "time" column and "dev" column
#' @param predictors A data frame of predictors used for forecasting recruitment
#' @param model_type The type of model used to link predictors to forecasted recruitment. Can
#' be "randomForest", "glmnet"
#' @param n_forecast How many years to use as a holdout / test set
#' @param n_years_ahead How many years ahead to forecast (defaults to 1)
#' 1:n_vars variables, and then results are combined and sorted to remove duplicates
#' @param control a list of the range and steps of tuning parameters to perform a grid
#' search over. This includes alpha (defaults from 0.1 to 0.9, by steps of 0.05) and
#' lambda (defaults from 0 to 2, by steps of 0.05) for glmnet; for randomForest this
#' includes ntree (defaults from 300 to 2000, by steps of 100) and
#' mtry (defaults from 2 to 10, by steps of 1)
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr left_join
#' @importFrom broom tidy
#' @importFrom randomForest randomForest
#' @importFrom glmnet glmnet
#' @importFrom stats complete.cases predict
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @export
#'
#' @return a list containing predictions, with elements
#'
#' * `pred`: the predictions
#' * `vars`: the variable values used to fit the models
#' * `coefs`: tidy summaries from each year:iteration. Zeros not included for glmnet
#'
#'#' @examples
#' response <- data.frame(time = 1:40, dev = rnorm(40))
#'
#' predictors <- matrix(rnorm(400), ncol = 10)
#' #colnames(predictors) = paste0("X",1:ncol(predictors))
#' predictors <- as.data.frame(predictors)
#' predictors$time <- 1:40
#' lm_example <- univariate_forecast_ml(response,
#'      predictors,
#'      model_type = "glmnet",
#'      n_forecast = 10,
#'      n_years_ahead = 1)
#'
univariate_forecast_ml = function(response,
                               predictors,
                               model_type,
                               n_forecast = 10,
                               n_years_ahead = 1,
                               control = list(alpha=seq(0.1,0.9,by=0.05),
                                              lambda = seq(0,2,by=0.05),
                                              ntree=seq(300,2000,by=100),
                                              mtry = seq(2,10))) {

  if(any(class(predictors) == "data.frame")) {
  } else {
    stop("Error: predictors object must be a dataframe")
  }
  # create a dataframe of predictors
  pred_names = names(predictors)
  time_col = which(names(predictors)=="time")
  # grid search for tuning parameters
  if(model_type=="glmnet") {
    tuning = expand.grid(alpha = control$alpha,
                         lambda = control$lambda)
  }
  if(model_type=="randomForest") {
    tuning = expand.grid(ntree = control$ntree,
                         mtry = control$mtry)
  }

  coef_list <- list() # empty list for storing coefficients

  # add progress bar
  progress_bar <- txtProgressBar(min = 0, max = nrow(tuning), style = 3, char = "=")

  for(i in 1:nrow(tuning)) {
    setTxtProgressBar(progress_bar, value = i)

    # keep time and
    sub = dplyr::left_join(as.data.frame(response[,c("time","dev")]), predictors, by="time")
    # filter out NAs in predictor / responses
    sub = sub[which(complete.cases(sub)==TRUE),]

    sub$est <- NA
    sub$se <- NA
    min_yr <- max(sub$time)-n_forecast+1
    max_yr <- max(sub$time)

    for(yr in min_yr:max_yr) {

      # create X and Y. X are all covariates w/out time
      train_x = sub[which(sub$time < yr - n_years_ahead + 1),]
      train_x = train_x[,-which(names(train_x) %in% c("dev","time","est","se"))]
      test_x = sub[which(sub$time == yr),]
      test_x = test_x[,-which(names(test_x) %in% c("dev","time","est","se"))]
      train_y = sub[which(sub$time < yr - n_years_ahead + 1),]
      train_y = train_y[,c("dev")]

      if(model_type=="glmnet") try(fit <- glmnet(x=train_x, y = as.numeric(unlist(train_y)),
                                        lambda = tuning$lambda[i], alpha=tuning$alpha[i]), silent=TRUE)
      if(model_type=="randomForest") try(fit <- randomForest(x = train_x, y = as.numeric(unlist(train_y)),
                                                    ntree = tuning$ntree[i], mtry = tuning$mtry[i]), silent=TRUE)

      #fit <- lm(z ~ -1 + cov_1:species, data=sub[which(sub$time<yr),])
      # note -- some of these will be negatively correlated. Sign isn't important and a
      # all coefficient signs can be flipped
      if(model_type=="glmnet") {
        if(nrow(test_x) > 0) {
          if(class(fit)[1] != "try-error") {
            pred = try(
              predict(fit, newx = as.matrix(test_x)), silent = TRUE)
            sub$est[which(sub$time==yr)] <- pred
          }
        }
        # save coefficients
        if(yr == min_yr) {
          coefs <- broom::tidy(fit)
          coefs$yr <- yr
        } else {
          tmp_coefs <- broom::tidy(fit)
          tmp_coefs$yr <- yr
          coefs <- rbind(coefs, tmp_coefs)
        }
      }
      if(model_type=="randomForest") {
        if(nrow(test_x) > 0) {
          if(class(fit)[1] != "try-error") {
            pred = try(
              predict(fit, newdata = test_x), silent = TRUE)
            sub$est[which(sub$time==yr)] <- pred
          }
        }
        coefs = NA
        #sub$se[which(sub$time==yr)] <- pred$se.fit
      }
    }

    coef_list[[i]] <- coefs

    sub$id = i
    if(i==1) {
      out_df <- sub
    } else {
      out_df <- rbind(out_df, sub)
    }
  }

  tuning$id <- seq(1,nrow(tuning))
  out <- list("pred" = out_df, "tuning"=tuning)
  return(out)
}

