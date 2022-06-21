#' Forecast recruitment deviations as a function of covariates using GLMs and GAMs
#'
#' \code{multivariate_forecast} Takes data frame or matrix of responses and dataframe of predictors and automates predictions
#'
#' @param response A matrix or data frame of responses for the modeling (values to be forecast) containing
#' a "time" column, "species" column, and "dev" column
#' @param predictors A data frame of predictors used for forecasting recruitment
#' @param model_type The type of model used to link predictors to forecasted recruitment. Can
#' be "lm", "gam",
#' @param n_forecast How many years to use as a holdout / test set
#' @param n_years_ahead How many years ahead to forecast (defaults to 1)
#' 1:n_vars variables, and then results are combined and sorted to remove duplicates
#' @param max_vars The maximum number of variables to include as predictors; defaults to 3
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr left_join
#' @importFrom stats lm as.formula predict.lm
#' @importFrom glmmTMB glmmTMB
#' @importFrom mgcv gam predict.gam
#'
#' @export
#'
#' @return a list containing predictions and the variables used in making predictions
#'
multivariate_forecast = function(response,
                               predictors,
                               model_type,
                               n_forecast = 10,
                               n_years_ahead = 1,
                               max_vars = 3) {

  # create a dataframe of predictors
  pred_names = names(predictors)
  time_col = which(names(predictors)=="time")

  combos = create_df_predictors(names = pred_names[which(pred_names!="time")],
                                n_vars = max_vars)
  if(class(combos)=="character") {
    # catch the case where a single character is returned
    combos = data.frame(cov1 = pred_names[which(pred_names!="time")])
  }

  for(i in 1:nrow(combos)) {
    # keep time and
    tmp = predictors[,c(1,which(pred_names %in% c(combos[i,])))]
    sub = dplyr::left_join(as.data.frame(response[,c("time","dev","species")]), tmp, by="time")
    # remove spaces if they exist to help with formula parsing
    names(sub)[4:ncol(sub)] = paste0("cov", seq(1,length(4:ncol(sub))))
    covar_names = names(sub)[4:ncol(sub)]

    # Add formulas -- no intercept because rec devs are already standardized, as are predictors
    if(model_type=="lm") {
      f <- as.formula(paste("dev",
                            paste(c("-1",paste0("species:",covar_names)), collapse = " + "),
                            sep = " ~ "))
    }
    if(model_type=="gam") {
      sub$species = as.factor(sub$species)
      covar_names = paste0("s(",covar_names,", species,k=4,bs='fs')")
      f <- as.formula(paste("dev",
                            paste(c("-1",covar_names), collapse = " + "),
                            sep = " ~ "))
    }
    if(model_type=="glmm") {
      sub$species = as.factor(sub$species)
      covar_names = paste0("(-1+",covar_names," | species)")
      f <- as.formula(paste("dev",
                            paste(c("-1",covar_names), collapse = " + "),
                            sep = " ~ "))
    }
    sub$est <- NA
    sub$se <- NA
    sub$id <- i
    for(yr in (max(sub$time)-n_forecast+1):max(sub$time)) {

      if(model_type=="lm") try(fit <- lm(f, data=sub[which(sub$time<yr - n_years_ahead + 1),]), silent=TRUE)
      if(model_type=="gam") try(fit <- gam(f, data=sub[which(sub$time<yr - n_years_ahead + 1),]), silent=TRUE)
      if(model_type=="glmm") try(fit <- glmmTMB(f, data=sub[which(sub$time<yr - n_years_ahead + 1),]), silent=TRUE)

      #fit <- lm(z ~ -1 + cov_1:species, data=sub[which(sub$time<yr),])
      # note -- some of these will be negatively correlated. Sign isn't important and a
      # all coefficient signs can be flipped
      converge = TRUE
      if(model_type == "glmm" & class(fit)[1] != "try-error") {
        if(any(is.na(sqrt(diag(fit$sdr$cov.fixed))))) converge = FALSE
      }
      pred = try(
        predict(fit, newdata = sub[which(sub$time==yr),], se.fit=TRUE), silent = TRUE)
      if(class(pred)[1] != "try-error" & converge==TRUE) {
        sub$est[which(sub$time==yr)] <- pred$fit
        sub$se[which(sub$time==yr)] <- pred$se.fit
      }
    }

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
  out <- list("pred" = out_df, "vars"=combos)
  return(out)
}


# if(model=="lm") fit_full <- lm(f, data=sub)
# if(model=="gam") fit_full <- gam(f, data=sub)
# spp_combo$aic[i] = AIC(fit_full)
#
# # calculate R2
# r2 = dplyr::filter(sub, !is.na(est)) %>%
#   dplyr::summarize(r2 = cor(est,z)^2)
#
# spp_combo$mean_r2[i] = mean(r2$r2)
# spp_combo$rmse[i] = sqrt(mean((sub$est - sub$z)^2,na.rm=T))
# if(spp_combo$rmse[i] < best_rmse) {
#   best_rmse = spp_combo$rmse[i]
#   best_dat = sub
# }
