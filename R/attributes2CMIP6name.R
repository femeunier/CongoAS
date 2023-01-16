attributes2CMIP6name <- function(var="cVeg",
                                 timestep="Lmon",
                                 model="CanESM5",
                                 scenario="1pctCO2",
                                 variant="r2i1p1f1",
                                 grid="gn",
                                 init.year=1850,
                                 init.month=1,
                                 end.year=2000,
                                 end.month=12){

  CMIP6name <- paste0(paste(var,timestep,model,
                            scenario,variant,grid,
                            paste0(as.character(init.year),sprintf("%02d",init.month),"-",s.character(end.year),sprintf("%02d",end.month)),
                            sep = "_"),".nc")


  return(CMIP6name)

}
