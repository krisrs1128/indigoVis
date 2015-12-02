# groups-from-names ------------------------------------------------------------
#' @title Get groups from vector of names
#' @param x A vector of names whose elements have the form
#' {group label}_{question specific info}.
groups_from_names <- function(x) {
  sapply(strsplit(x, "_"), function(z) z[1])
}
