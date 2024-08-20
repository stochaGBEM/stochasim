#' @export
#' @rdname hydist_snowrain
hydist_snow <- function(dst_peak, baseflow, duration = 9 * 24) {
  if (is.numeric(dst_peak)) dst_peak <- distionary::dst_degenerate(dst_peak)
  dst_peak <- distplyr::slice_left(dst_peak, baseflow)
  realise <- function(n) {
    if (n == 0) return(list())
    x <- distionary::realise(dst_peak, n)
    if (n == 1) return(gbem::hyd_snow(x, baseflow, duration = duration))
    lapply(x, function(x_) gbem::hyd_snow(x_, baseflow, duration = duration))
  }
  hydist(realise)
}


