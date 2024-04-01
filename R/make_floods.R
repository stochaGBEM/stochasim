#' Generate a sequence of floods to run the simulations based on the
#' specified GEV distributions
#'
#' @param gev Specify a GEV
#' @param n_floods description
#'
#' @export
make_floods <- function(gev, n_floods){

  floods <- extRemes::revd(
    n = n_floods,
    loc = gev[1],
    scale = gev[2],
    shape = gev[3],
    type = "GEV"
  )
  # get rid of any negative numbers
  floods[floods < 0] <- mean(floods)
  return(floods)
}
