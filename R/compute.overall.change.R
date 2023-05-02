compute.overall.change <- function(cVeg,dt = 1,Nyears = 5){

  N.timestep = round(Nyears/dt)

  overall.change = mean(cVeg[max(1,(length(cVeg)-(N.timestep - 1))):length(cVeg)]) - mean(cVeg[1:(min(length(cVeg),N.timestep))])
  return(overall.change)
}
