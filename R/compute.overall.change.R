compute.overall.change <- function(cVeg,dt = 1){

  N.timestep = round(5/dt)

  overall.change = mean(cVeg[(length(cVeg)-(N.timestep - 1)):length(cVeg)]) - mean(cVeg[1:(N.timestep)])
  return(overall.change)
}
