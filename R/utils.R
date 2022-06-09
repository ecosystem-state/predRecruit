#' Expand covariates in multiple permuations. Like expand.grid, but with 1, 2, ..., n variables chosen
#'
#' \code{create_df_predictors} takes a vector of variable names and creates a dataframe of
#'
#' @param names The names of the dataframe to be expanded
#' @param n_vars Number of variables to be selected, e.g. expand.grid is called with
#' 1:n_vars variables, and then results are combined and sorted to remove duplicates
#' @importFrom dplyr bind_rows distinct
#'
#' @export
#'
#' @return an expanded data frame
#'
create_df_predictors = function(names, n_vars = 3) {

  # generate largest set
  combo = as.data.frame(matrix(rep(names,n_vars), ncol=n_vars),
                        stringsAsFactors = FALSE)
  names(combo) = paste0("cov",1:n_vars)
  # expand data frame
  df = expand.grid(var=combo,stringsAsFactors = FALSE)

  names(df) = rep("",ncol(df))  # strip names for sorting
  sorted = as.data.frame(t(apply(df, 1, sort)))
  df =  distinct(sorted) # remove duplicates

  if(n_vars > 1) {
    for(i in (n_vars-1):1) {
      new_combo = as.data.frame(matrix(rep(names,i), ncol=i),
                                stringsAsFactors = FALSE)
      names(new_combo) = paste0("cov",1:i)
      # expand data frame
      newdf = expand.grid(var=new_combo,stringsAsFactors = FALSE)

      if(i > 1) {
        names(newdf) = rep("",ncol(newdf))  # strip names for sorting
        sorted = as.data.frame(t(apply(newdf, 1, sort)))
        newdf = distinct(sorted) # remove duplicates
      } else {names(newdf) = "V1"}
      df = bind_rows(df,newdf)
    }
  }

  # this block is identifying cases where the same species is
  # used multiple times
  not_na = function(x) {return(length(which(!is.na(x))))}
  not_nas = apply(df, 1, not_na)
  n_unique = function(x) {
    x = x[which(!is.na(x))]
    return(length(unique(x)))
  }
  n_uniq = apply(df, 1, n_unique)
  df = df[which(n_uniq==not_nas),]
  names(df) = paste0("cov",1:n_vars)
  return(df)
}
