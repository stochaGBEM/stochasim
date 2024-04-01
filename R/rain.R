#' @rdname stochasim_rainsnow
stochasim_rain <- function(floods_rain, Q_base, n, d84, d50, width_0, slope, H) {
  rv_rate <- 0.1
  #calculate some constants
  nsim <- length(floods_rain)
  width_base <- 3 * sqrt(Q_base)  #estimate the low flow width based on base flow using std hydraulic geometry eq

  results <- matrix(data = NA, nrow = nsim, ncol = 4)

  #initialize the simulation by making calculations for the first year
  i = 1
  #analyse the effects of the first snowmelt flood
  r_rain <- rain_hydrograph(floods_rain[i], Q_base, n, d84, d50, width_0, slope, H)
  width_r <- width_0 + r_rain[2] #widen the channel

  #account for revegetation and net widening during hydrographs
  if(r_rain[2] > 0) {
    reveg <- 0  #no revegetation if bank erosion occurs
  } else {
    reveg <- rv_rate * (width_r - width_base)
  }
  width_v <- width_r - reveg

  #write the results Q_rain, Vb_rain, E_rain, new_width
  results[i,] <- c(floods_rain[i], r_rain[3], r_rain[2], width_v)

  # loop through all the floods generated above
  for (i in seq(2, nsim)) {
    #use the final width from the previous run to start the next one
    width_0 <- results[(i-1), 4]

    #analyse the effects of the next snowmelt flood
    r_rain <- rain_hydrograph(floods_rain[i], Q_base, n, d84, d50, width_0, slope, H)
    width_r <- width_0 + r_rain[2] #widen the channel

    #account for revegetation and net widening during hydrographs
    if(r_rain[2] > 0) {
      reveg <- 0  #no revegetation if bank erosion during fall/storm
    } else {
      reveg <- rv_rate * (width_r - width_base)
    }
    width_v <- width_r - reveg

    #write the results Q_rain, Vb_rain, E_rain, new_width
    results[i,] <- c(floods_rain[i], r_rain[3], r_rain[2], width_v)

  }

  #once the simulation is complete, transform to a data frame
  results <- as.data.frame(results)

  #give the columns meaningful names
  colnames(results) <- c('Q_rain',
                         'Vb_rain',
                         'E_rain',
                         'new_width')
  return(results)
}
