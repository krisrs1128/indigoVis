
# lm -----------------------------------------------------------------------
#' @title Fit a lm of a subset of columns of X onto one other column of X
#' @param dat All the data (X and y)
#' @param groups The names of groups to subset from dat
#' @param response_id The name of the response to extract from dat
fit_lm <- function(dat, cur_vars, response_id) {
  X <- dat[, setdiff(colnames(dat), response_id)]
  X <- X[, colnames(X) %in% cur_vars, drop = F]
  y <- dat[, response_id]
  cur_model <- lm(y ~ ., data = data.frame(X, y))
  summary(cur_model)
}
