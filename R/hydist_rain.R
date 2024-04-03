#' Make a Rainfall or Snowmelt-Related Event Hydrograph Distribution
#'
#' @param dst_peak Distionary distribution of the peak. Left-truncated so as
#' to always exceed the baseflow.
#' @param baseflow Baseflow discharge; single numeric.
#' @param duration Length of the event. Defaults to 3 days for
#' rainfall-related events, and 9 days for snowmelt-related events (in hours).
#' @returns A hydrograph distribution for rainfall-related and snowmelt-related
#' events, with a constant baseflow and a stochastic peak discharge.
#' @examples
#' d <- distionary::dst_gev(100, 3, 0.1)
#' set.seed(4)
#' hs <- hydist_snow(d, 50)
#' plot(hs, n = 1000, col = "orange3")
#' hr <- hydist_rain(d, 50)
#' plot(hr, n = 1000, col = "blue3", add = TRUE)
#' @rdname hydist_snowrain
#' @export
hydist_rain <- function(dst_peak, baseflow, duration = 3 * 24) {
  if (is.numeric(dst_peak)) dst_peak <- distionary::dst_degenerate(dst_peak)
  dst_peak <- distplyr::slice_left(dst_peak, baseflow)
  realise <- function(n) {
    if (n == 0) return(list())
    x <- distionary::realise(dst_peak, n)
    if (n == 1) return(gbem::hyd_rain(x, baseflow, duration = duration))
    lapply(x, function(x_) gbem::hyd_rain(x_, baseflow, duration = duration))
  }
  hydist(realise)
}


