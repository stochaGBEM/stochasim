#' @export
#' @rdname ss
ss_widths <- function(stochasim) {
  cs <- stochasim$cross_sections
  vapply(cs, gbem::ch_width, FUN.VALUE = numeric(1L))
}
