plot_cor_groups <- function(X) {
  #x_groups <- groups_from_names(colnames(X))
  #print(x_groups)
  #print(groups)
  #keep_ix <- which(x_groups %in% groups)
  #print(keep_ix)
  #corX <- cor(X[, keep_ix])
  corX <- cor(X)
  return (plot_cor(corX))
}

plot_cor <- function(corX) {
  require("ggplot2")
  require("reshape2")
  cor_plot <- ggplot(melt(corX)) +
    geom_tile(aes(x = Var1, y = Var2, fill = value), color = "white") +
    scale_fill_gradient2(midpoint = 0, limits = c(-1, 1)) +
    theme_bw() +
    theme(axis.text.x=element_text(angle = -90, size = 8),
          axis.text.y=element_text(size = 8)) +
    coord_fixed()
  return (cor_plot)
}

print_cors <- function(corX) {
  colnames(corX) <- substr(colnames(corX), 1, 25)
  rownames(corX) <- substr(rownames(corX), 1, 25)
  diag(corX) <- NA
  corX[upper.tri(corX)] <- NA
  mcorX <- na.omit(melt(corX))
  mcorX$value <- round(mcorX$value, 2)
  print(arrange(mcorX, desc(abs(value))))
}
