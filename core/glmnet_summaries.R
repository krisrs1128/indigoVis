# plot-glmnet -------------------------------------------------------------
#' @title Alternative visualization of glmnet coefficients
#' @description Visualize significant coefficients using a heatmap,
#' instead of a trace of the lasso paths.
#' @param beta The coefficient matrix of a glmnet object.
#' @return res_plot A heatmap of the lasso paths.
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_tile aes scale_fill_gradient2 element_text
#' element_blank geom_vline scale_color_manual
#' theme
#' @export
plot_lasso_coef <- function(beta) {
  # reshape betas
  beta <- as.matrix(beta)
  beta <- beta[rev(rownames(beta)), ]
  mbeta <- melt(beta)
  colnames(mbeta) <- c("feature", "lambda", "value")
  mbeta <- mbeta[order(mbeta$lambda), ]

  ggplot(mbeta) +
    geom_tile(aes(x = lambda, y = feature, fill = value), col = "grey") +
    scale_fill_gradient2(midpoint = 0) +
    theme(axis.text.x = element_text(angle = -90, size = 5, hjust = 0, vjust = 0),
          axis.text.y = element_text(size = 10),
          axis.title = element_blank())
}

# summarize-multiclass ----------------------------------------------------

#' Helper function to compute vector norms in summary
norm_fun <- function(x, type) {
  switch(type,
         "0norm" = sum(x != 0),
         "1norm" = sum(abs(x)),
         "2norm" = sqrt(sum(x ^ 2)))
}

#' @title Summarize the Multiclass Lasso Coefficients
#' @description Given a list of beta coefficients (one for each response),
#' summarize the importance of different variables.
#' @param betas A list of lasso coefficients, whose r^th element is an
#' n_lambda x p matrix of the lasso paths for response r.
#' @param type The kind of norm to use when summarizing a coefficient across
#' dimensions. Options are "0norm", "1norm", and "2norm", the "2norm" is the
#' default.
#' @export
summarize_multiclass_coef <- function(betas, type = "2norm") {
  type <- match.arg(type, c("2norm", "1norm", "0norm"))
  betas <- lapply(betas, as.matrix)
  betas <- simplify2array(betas)
  betas_pool <- apply(betas, 1:2, function(x) norm_fun(x, type))
  return (betas_pool)
}

# all-lasso-coef ----------------------------------------------------------
#' @title Plot a Summary of the Multiclass Lasso Coefficients
#' @description Given a list of beta coefficients (one for each response),
#' plot the importance of different variables.
#' @param betas A list of lasso coefficients, whose r^th element is an
#' n_lambda x p matrix of the lasso paths for response r.
#' @param type The kind of norm to use when summarizing a coefficient across
#' dimensions. Options are "0norm", "1norm", and "2norm", the "2norm" is the
#' default.
#' @return The ggplot2 object containing a plot of either the number of times
#' the coefficient was nonzero, or the sum of its absolute value across
#' responses.
#' @export
plot_multiclass_coef <- function(betas, type = "2norm") {
  betas_pool <- summarize_multiclass_coef(betas, type)
  plot_lasso_coef(betas_pool)
}

# wrappers ----------------------------------------------------------------
#' @title Wrap plot_lasso
#' @description Automatically extract betas and lambda opts from cv.glmnet
#' output.
#' @param glmnet_fit The output of the distance lasso (more generally, the
#' output of a call to cv.glmnet, using family = "mgaussian").
#' @param lambda_opt The value of the optimal lambda, so we can plot a bar
#' at this point in the figure.
#' @param plot_type The way to visualize lasso paths in the case there are
#' multiple response dimensions. For "all", then all the coefficients paths for
#' each response dimension will be plotted, this is the default. The other
#' options are norms we can apply to each coefficient across dimensions, that
#' summarize the importance of that coefficient across dimensions. The options
#' are "0norm", "1norm", and "2norm".
#' @return res_plot A heatmap of the lasso paths.
#' @export
plot_glmnet <- function(glmnet_fit, plot_type = "all") {
  plot_type <- match.arg(plot_type, c("all", "0norm", "1norm", "2norm"))
  if(plot_type == "all") {
    coef_names <- names(glmnet_fit$beta)
    p <- vector(length = length(coef_names), mode = "list")
    names(p) <- coef_names
    for(r in seq_along(glmnet_fit$beta)) {
      p[[coef_names[r]]] <- plot_lasso_coef(glmnet_fit$beta[[r]]) +
        ggtitle(coef_names[r])
    }
  } else {
    p <- plot_multiclass_coef(glmnet_fit$beta, plot_type)
  }
  return (p)
}
