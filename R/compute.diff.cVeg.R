compute.diff.cVeg <- function(cVeg,dt = 1){

    N.timestep = round(15/dt)

    diff.cVeg = c(cVeg[1:(N.timestep-1)] - cVeg[1], cVeg[N.timestep:length(cVeg)] - cVeg[1:(length(cVeg) - (N.timestep-1))])
    return(diff.cVeg)

}
