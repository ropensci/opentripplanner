#' Convert list to data frame
#'
#' A faster alternative to as.data.frame(list), similar to base::list2DF() added
#' in R 4.0.0 but back compatible
#'
#' @param l named list of equal length
#' @family internal
#' @noRd
list2df <- function(l) {
  class(l) <- "data.frame"
  attr(l, "row.names") <- .set_row_names(length(l[[1]]))
  l
}


