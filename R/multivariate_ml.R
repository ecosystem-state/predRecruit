#' Forecast multivariate recruitment deviations as a function of covariates using random forest and lasso models
#'
#' \code{multivariate_forecast_ml} Takes matrix or data frame of responses and data frame of predictors and automates forecasts
#'
#' @param response A data frame of responses for the modeling (values to be forecast) containing
#' a "time" column, "species" column, and "dev" column
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
#' @importFrom randomForest randomForest
#' @importFrom glmnet glmnet
#' @importFrom stats complete.cases predict
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @export
#'
#' @return a list containing predictions and the variables used in making predictions
#'
multivariate_forecast_ml = function(response,
                               predictors,
                               model_type,
                               n_forecast = 10,
                               n_years_ahead = 1,
                               control = list(alpha=seq(0.1,0.9,by=0.05),
                                              lambda = seq(0,2,by=0.05),
                                              ntree=seq(300,2000,by=100),
                                              mtry = seq(2,10))) {

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

  # add progress bar
  progress_bar <- txtProgressBar(min = 0, max = nrow(tuning), style = 3, char = "=")

  for(i in 1:nrow(tuning)) {
    setTxtProgressBar(progress_bar, value = i)
    # keep time and
    sub = dplyr::left_join(as.data.frame(response[,c("time","dev","species")]), predictors, by="time")
    # filter out NAs in predictor / responses
    sub = sub[which(complete.cases(sub)==TRUE),]

    sub$est <- NA
    sub$se <- NA
    sub_stats <- data.frame(train_r2 = rep(NA, nrow(sub)),
                            train_rmse = rep(NA, nrow(sub)))

    for(yr in (max(sub$time)-n_forecast+1):max(sub$time)) {

      # create X and Y. X are all covariates w/out time
      train_x = sub[which(sub$time < yr - n_years_ahead + 1),]
      train_x = train_x[,-which(names(train_x) %in% c("dev","time","est","se"))]

      test_x = sub[which(sub$time == yr),]
      test_x = test_x[,-which(names(test_x) %in% c("dev","time","est","se"))]

      train_y = sub[which(sub$time < yr - n_years_ahead + 1),]
      train_y = train_y[,c("dev")]
      if(model_type == "glmnet") {
        # glmnet can't take data frames or factors -- everything in matrix form
        mm_train <- model.matrix(~.-1, train_x, na.action=na.pass)
        mm_test <- model.matrix(~.-1, test_x, na.action=na.pass)
        if(ncol(mm_test) != ncol(mm_train)) {
          mm_train <- mm_train[,which(colnames(mm_train) %in% colnames(mm_test))]
        }
      }
      if(model_type=="glmnet") try(fit <- glmnet(x=mm_train, y = unlist(train_y),
                                        lambda = tuning$lambda[i], alpha=tuning$alpha[i]), silent=TRUE)
      if(model_type=="randomForest") try(fit <- randomForest(x = train_x, y = unlist(train_y),
                                                    ntree = tuning$ntree[i], mtry = tuning$mtry[i]), silent=TRUE)

      #fit <- lm(z ~ -1 + cov_1:species, data=sub[which(sub$time<yr),])
      # note -- some of these will be negatively correlated. Sign isn't important and a
      # all coefficient signs can be flipped
      if(model_type=="glmnet") {
        if(nrow(test_x) > 0) {
          if(class(fit)[1] != "try-error") {
            pred = try(
              predict(fit, newx = mm_test), silent = TRUE)
            sub$est[which(sub$time==yr)] <- pred
          }

          # add training r2 and training rmse
          pred = try(predict(fit, newx = as.matrix(train_x)), silent = TRUE)
          if(class(pred)[1] != "try-error") {
            sub_stats$train_r2[which(sub$time==yr)] <- cor(as.numeric(unlist(train_y)), pred, use = "pairwise.complete.obs") ^ 2
            sub_stats$train_rmse[which(sub$time==yr)] <- sqrt(mean((as.numeric(unlist(train_y)) - pred)^2, na.rm=T))
          }
        }
      }
      if(model_type=="randomForest") {
        if(nrow(test_x) > 0) {
          if(class(fit)[1] != "try-error") {
            pred = try(
              predict(fit, newdata = test_x), silent = TRUE)
            sub$est[which(sub$time==yr)] <- pred

            # add training r2 and training rmse
            pred = try(predict(fit, newdata = train_x), silent = TRUE)
            if(class(pred)[1] != "try-error") {
              sub_stats$train_r2[which(sub$time==yr)] <- cor(as.numeric(unlist(train_y)), pred, use = "pairwise.complete.obs") ^ 2
              sub_stats$train_rmse[which(sub$time==yr)] <- sqrt(mean((as.numeric(unlist(train_y)) - pred)^2, na.rm=T))
            }
          }
        }
        #sub$se[which(sub$time==yr)] <- pred$se.fit
      }
    }

    sub$id = i
    sub = cbind(sub, sub_stats) # can't have these in the df
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

