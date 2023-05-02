compute.rolling.means <- function(values,years,Nyears = 1,dt = 1){

  rel.yr <- years - years[1]
  init.values <- mean(values[rel.yr < Nyears])

  k = round(Nyears/dt)

  rolling.mean <- rollmean(values,k,align = "center",fill = NA) - init.values

  return(rolling.mean)
}


