#' Constructor Function for "hydst" Objects
#'
#' @param l List containing the components of a distribution object.
#' @param ... Attributes to add to the list.
#' @param class If making a subclass, specify its name here.
#' @returns `"hydist"` object.
new_hydist <- function(l, ..., class = character()) {
  distionary::new_distribution(l, variable = "function", class = "hydist")
}


#' Create a new hydrograph distribution
#'
#' @param realise Function that accepts a single non-negative integer and
#' returns that many `gbem::hydrograph()` realizations.
#' hydrograph.
#' @returns A hydrograph distribution.
hydist <- function(realise) {
  res <- list(
    realise = realise
  )
  new_hydist(res)
}

#' Generate Random Hydrographs
#'
#' @param distribution Hydrograph distribution.
#' @param n Number of hydrographs to generate; single positive integer.
#' @exportS3Method distionary::realise
realise.hydist <- function(distribution, n = 1) {
  distribution$realise(n)
}

#' Plot random realization of a hydrograph distribution
#'
#' @param x Hydrograph distribution
#' @param ... Other arguments to pass to the curve() function.
#' @param xlab,ylab Y-axis and x-axis labels for the plot.
#' @returns Base R plot of randomly drawn hydrograph.
#' @export
plot.hydist <- function(x, ..., ylab = "Discharge", xlab = "Time") {
  plot(realise(x), ..., ylab = ylab, xlab = xlab)
}
