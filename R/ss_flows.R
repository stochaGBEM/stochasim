#' Get peak flows and channel widths from stochasim
#' @param stochasim Stochasim object.
#' @export
#' @rdname ss
ss_flows <- function(stochasim) {
  hx <- stochasim$x
  x <- vapply(hx, gbem::peak, FUN.VALUE = numeric(1L))
  hy <- stochasim$y
  if (!is.null(hy)) {
    y <- vapply(hy, gbem::peak, FUN.VALUE = numeric(1L))
    list(x = x, y = y)
  } else {
    x
  }
}
