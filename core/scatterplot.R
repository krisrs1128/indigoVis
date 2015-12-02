scatterplot <- function(X, var1, var2, row_groups, split_groups) {
  X <- as.data.frame(X)
  var1_ix <- which(colnames(X) == var1)
  var2_ix <- which(colnames(X) == var2)
  X_sub <- cbind(id = rownames(X), X[, c(var1_ix, var2_ix), drop = F])

  # case when we have row groups
  if(length(row_groups) > 0 & split_groups) {
    X_sub <- cbind(X_sub, group = row_groups)
    n_groups <- length(unique(row_groups))
    p <- X_sub %>%
      ggvis(x = as.name(var1), y = as.name(var2)) %>%
      layer_points(size := 25, size.hover := 75, fill = ~group, key := ~id) %>%
      scale_nominal("fill", range = brewer_pal("seq", "Set2")(n_groups)) %>%
      add_tooltip(function(x) x$id, "hover") %>%
      group_by(group)
    if(var1 != var2) {
      p <- p %>%
        layer_model_predictions(model = "lm", stroke = ~group) %>%
        scale_nominal("stroke", range = brewer_pal("seq", "Set2")(n_groups))
    }
    # case we don't have row groups
  } else {
    p <- X_sub %>%
      ggvis(x = as.name(var1), y = as.name(var2)) %>%
      layer_points(size := 25, size.hover := 75, key := ~id) %>%
      add_tooltip(function(x) x$id, "hover")
      if(var1 != var2) {
        p <- p %>% layer_model_predictions(model = "lm")
      }
  }
  return (p)
}
