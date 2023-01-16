attributes2CMIP6name <- function(cdf = data.frame(var="cVeg",
                                                  timestep="Lmon",
                                                  model="CanESM5",
                                                  scenario="1pctCO2",
                                                  variant="r2i1p1f1",
                                                  grid="gn",
                                                  init.year=1850,
                                                  init.month=1,
                                                  end.year=2000,
                                                  end.month=12)){

  var=cdf[["var"]]
  timestep=cdf[["timestep"]]
  model=cdf[["model"]]
  scenario=cdf[["scenario"]]
  variant=cdf[["variant"]]
  grid=cdf[["grid"]]
  init.year=cdf[["init.year"]]
  init.month=cdf[["init.month"]]
  end.year=cdf[["end.year"]]
  end.month=cdf[["end.month"]]

  CMIP6name <- c()
  for (i in (1:length(var))){
    CMIP6name[i] <- paste0(paste(var[i],timestep[i],model[i],
                              scenario[i],variant[i],grid[i],
                              paste0(as.character(init.year[i]),sprintf("%02d",init.month[i]),"-",as.character(end.year[i]),sprintf("%02d",end.month[i])),
                              sep = "_"),".nc")
  }




  return(CMIP6name)

}
