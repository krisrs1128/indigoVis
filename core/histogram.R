histogram <- function(X, var1, var2, row_groups, split_groups) {
  X <- as.data.frame(X)
  var1_ix <- which(colnames(X) == var1)
  var2_ix <- which(colnames(X) == var2)
  X_sub <- cbind(id = 1:nrow(X), X[, c(var1_ix, var2_ix), drop = F])

  if(length(row_groups) > 0 & split_groups) {
    X_sub <- cbind(X_sub, group = row_groups)
    mX <- melt(X_sub, id.vars = c("id", "group"))
    mX$group <- as.factor(mX$group)
    ggplot(mX, aes(x = value, fill = variable)) +
      geom_histogram(alpha = 0.8, position = "identity") +
      scale_fill_brewer(palette = "Set2") +
      theme_bw() +
      facet_grid(variable ~ group) +
      theme(legend.text = element_text(size = 13))
  } else {
    mX <- melt(X_sub, id.vars = "id")
    ggplot(mX, aes(x = value, fill = variable)) +
      geom_histogram(alpha = 0.8, position = "identity") +
      scale_fill_brewer(palette = "Set2") +
      facet_grid(variable ~ .) +
      theme_bw() +
      theme(legend.text = element_text(size = 13))
  }
}








