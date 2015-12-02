
plot_hclust <- function(X, groups) {
  all_groups <- groups_from_names(colnames(X))
  X <- X[, all_groups %in% groups]
  hclustX <- hclust(dist(t(X)))

  #convert cluster object to use with ggplot
  dendr <- dendro_data(hclustX, type="rectangle")

  # label groups
  dendr$labels$label <- as.character(dendr$labels$label)
  dendr$labels$group <- groups_from_names(dendr$labels$label)
  dendr$labels$label <- sapply(dendr$labels$label, function(x) {
    x <- strsplit(x, "_")
    paste0("   ", paste(x[[1]][-1], collapse = "_"))
  })

  # generate plot
  ggplot() +
    geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) +
    geom_text(data=label(dendr),
              aes(x=x, y=y, label=label, hjust=0, col = group), size = 3) +
    coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
    theme(axis.line.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_rect(fill="white"),
          panel.grid=element_blank())
}
