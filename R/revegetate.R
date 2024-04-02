#' Revegetate a cross section
#'
#' Reduce the width of a channel cross section by a percentage
#' compared to a baseline channel width.
#' @param cross_section A `"cross_section"` object representing a stream's
#' cross section.
#' @param width0 "Stable" cross section width; positive numeric.
#' @param rate Revegetation percent; single number between 0 and 1.
#' @returns The original cross section with a reduced width. Width reduction
#' is calculated as the proportion `rate` of the difference between channel
#' width and `width0`.
#' @export
revegetate <- function(cross_section, width0, rate = 0.1) {
  w <- ch_width(cross_section)
  reveg <- rate * (w - Q_base)
  ch_width(width) <- w - reveg
  cross_section
}
