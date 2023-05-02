compute.diff.cVeg <- function(cVeg,dt = 1,Nyears = 1){

    N.timestep = round(Nyears/dt)

    diff.cVeg = c(cVeg[1:(N.timestep)] - cVeg[1], cVeg[(N.timestep + 1):length(cVeg)] - cVeg[1:(length(cVeg) - (N.timestep))])
    return(diff.cVeg)

}
