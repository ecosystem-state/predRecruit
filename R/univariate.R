#' Forecast recruitment deviations as a function of covariates using GLMs and GAMs
#'
#' \code{univariate_forecast} vector of responses and dataframe of predictors and automates
#'
#' @param response A data frame of responses for the modeling (values to be forecast) containing
#' a "time" column and "dev" column
#' @param predictors A data frame of predictors used for forecasting recruitment
#' @param model_type The type of model used to link predictors to forecasted recruitment. Can
#' be "lm", "gam",
#' @param n_forecast How many years to use as a holdout / test set
#' @param n_years_ahead How many years ahead to forecast (defaults to 1)
#' 1:n_vars variables, and then results are combined and sorted to remove duplicates
#' @param max_vars The maximum number of variables to include as predictors; defaults to 3
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr left_join
#' @importFrom broom tidy
#' @importFrom ggeffects ggpredict
#' @importFrom stats lm as.formula predict cor model.matrix na.pass
#' @importFrom mgcv gam
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @export
#'
#' @return a list containing predictions, with elements
#'
#' * `pred`: the predictions
#' * `vars`: the variable values used to fit the models
#' * `coefs`: coefficient estimates from each year:iteration
#' @examples
#' response <- data.frame(time = 1:40, dev = rnorm(40))
#'
#' predictors <- matrix(rnorm(400), ncol = 10)
#' #colnames(predictors) = paste0("X",1:ncol(predictors))
#' predictors <- as.data.frame(predictors)
#' predictors$time <- 1:40
#' lm_example <- univariate_forecast(response,
#'      predictors,
#'      model_type = "lm",
#'      n_forecast = 10,
#'      n_years_ahead = 1,
#'      max_vars = 3)
#'
univariate_forecast = function(response,
                               predictors,
                               model_type,
                               n_forecast = 10,
                               n_years_ahead = 1,
                               max_vars = 3) {

  if(any(class(predictors) == "data.frame")) {
  } else {
    stop("Error: predictors object must be a dataframe")
  }
  # create a dataframe of predictors
  pred_names = names(predictors)
  time_col = which(names(predictors)=="time")

  combos = create_df_predictors(names = pred_names[which(pred_names!="time")],
                                n_vars = max_vars)
  if(class(combos)=="character") {
    # catch the case where a single character is returned
    combos = data.frame(cov1 = pred_names[which(pred_names!="time")])
  }

  coef_list <- list() # empty list for storing coefficients
  marginal_pred <- list() # empty list for storing marginal predictions

  # add progress bar
  progress_bar <- txtProgressBar(min = 0, max = nrow(combos), style = 3, char = "=")

  for(i in 1:nrow(combos)) {
    setTxtProgressBar(progress_bar, value = i)

    # keep time and
    tmp = predictors[,c(which(pred_names %in% c(combos[i,],"time")))]
    sub = dplyr::left_join(as.data.frame(response[,c("time","dev")]), tmp, by="time")
    # remove spaces if they exist to help with formula parsing
    names(sub)[3:ncol(sub)] = paste0("cov", seq(1,length(3:ncol(sub))))
    covar_names = names(sub)[3:ncol(sub)]

    # Add formulas -- no intercept because rec devs are already standardized, as are predictors
    if(model_type=="lm") {
      f <- as.formula(paste("dev",
                            paste(c("-1",covar_names), collapse = " + "),
                            sep = " ~ "))
    }
    if(model_type=="gam") {
      covar_names_str = paste0("s(",covar_names,",k=4,bs='ps')")
      f <- as.formula(paste("dev",
                            paste(c("-1",covar_names_str), collapse = " + "),
                            sep = " ~ "))
    }
    sub$est <- NA
    sub$se <- NA
    sub$train_r2 <- NA
    sub$train_rmse <- NA
    sub$id <- i
    min_yr <- max(sub$time)-n_forecast+1
    max_yr <- max(sub$time)

    for(yr in min_yr:max_yr) {
      sub_dat <- sub[which(sub$time< yr - n_years_ahead + 1),]
      if(model_type=="lm") fit <- try(lm(f, data = sub_dat), silent=TRUE)
      if(model_type=="gam") fit <- try(gam(f, data = sub_dat), silent=TRUE)
      #fit <- lm(z ~ -1 + cov_1:species, data=sub[which(sub$time<yr),])
      # note -- some of these will be negatively correlated. Sign isn't important and a
      # all coefficient signs can be flipped
      if(class(fit)[1] != "try-error") {
        pred = try(
          predict(fit, newdata = sub[which(sub$time==yr),], se.fit=TRUE), silent = TRUE)
        sub$est[which(sub$time==yr)] <- pred$fit
        sub$se[which(sub$time==yr)] <- pred$se.fit

        # add training r2 and training rmse
        pred = try(predict(fit, sub_dat), silent = TRUE)
        if(class(pred)[1] != "try-error") {
          sub$train_r2[which(sub$time==yr)] <- cor(c(sub_dat$dev), pred, use = "pairwise.complete.obs") ^ 2
          sub$train_rmse[which(sub$time==yr)] <- sqrt(mean((c(sub_dat$dev) - pred)^2, na.rm=T))
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

        # save marginal predictions
        for(ii in 1:length(covar_names)) {
          marg <- ggpredict(fit,covar_names[ii])
          marg$year <- yr
          marg$cov <- covar_names[ii]
          if(ii == 1) {
            marg_pred <- marg
          } else {
            marg_pred <- rbind(marg_pred, marg)
          }
        }
        if(yr == min_yr) {
          marg_all <- marg_pred
        } else {
          marg_all <- rbind(marg_all, marg_pred)
        }
      }
    }
    marginal_pred[[i]] <- marg_all
    coef_list[[i]] <- coefs

    if(i==1) {
      out_df <- sub
    } else {
      # find missing covariates if applicable
      missing_covars = names(out_df)[which(names(out_df) %in% names(sub) == FALSE)]
      if(length(missing_covars) > 0) {
        for(mm in 1:length(missing_covars)) {
          sub[[missing_covars[mm]]] = NA
        }
      }
      out_df <- rbind(out_df, sub)
    }
  }

  combos$id <- seq(1,nrow(combos))
  out <- list("pred" = out_df,
              "vars"=combos,
              "coefs" = coef_list,
              "marginal" = marginal_pred)
  return(out)
}

