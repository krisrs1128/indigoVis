
# glmnet -----------------------------------------------------------------------
#' @title Fit a glmnet of a subset of columns of X onto one other column of X
#' @param dat All the data (X and y)
#' @param groups The names of groups to subset from dat
#' @param response_id The name of the response to extract from dat
fit_glmnet <- function(dat, groups, response_id) {
  X <- dat[, setdiff(colnames(dat), response_id)]
  all_groups <- groups_from_names(colnames(X))
  X <- X[, all_groups %in% groups]
  y <- dat[, response_id]
  cv.glmnet(x = as.matrix(X), y = y)
}

get_nonzero_coef <- function(dat, groups, response_id, lambda) {
  X <- dat[, setdiff(colnames(dat), response_id)]
  all_groups <- groups_from_names(colnames(X))
  X <- X[, all_groups %in% groups]
  y <- dat[, response_id]
  beta <- coef(glmnet(x = as.matrix(X), y = y, lambda = lambda))
  beta <- beta[-1, ] # remove intercept
  names(beta[which(beta != 0)])
}
