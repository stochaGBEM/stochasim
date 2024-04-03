#' Plot random realization of a hydrograph distribution
#'
#' @param x Hydrograph distribution
#' @param ... Other arguments to pass to the curve() function.
#' @param xlab,ylab Y-axis and x-axis labels for the plot.
#' @returns Base R plot of randomly drawn hydrograph.
#' @export
plot.hydist <- function(x, ..., ylab = "Discharge", xlab = "Time") {
  plot(distionary::realise(x), ..., ylab = ylab, xlab = xlab)
}
