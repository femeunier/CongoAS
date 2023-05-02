compute.change.rate <- function(diff.cVeg,dt = 1){

  Ntimestep = round(15/dt)

  change.rate.m = abs(diff.cVeg)/c(seq(0,(Ntimestep - 2)),rep(Ntimestep,(length(diff.cVeg) - (Ntimestep - 1))))
  return(change.rate.m)
}
