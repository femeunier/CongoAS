yr2CO2 <- function(yrs,scenario = "1pctCO2"){

  if (scenario == "1pctCO2"){
    CO2 <- 280*(1.01**(yrs - yrs[1]))
  } else{
    stop()
  }

  return(CO2)
}
