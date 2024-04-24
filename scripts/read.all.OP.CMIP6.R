rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)

variables <- c("pr")
scenarios <- c("land-cCO2","land-hist","land-cClim")

# models <- c("CMCC-ESM2","CNRM-ESM2-1","IPSL-CM6A-LR","MPI-ESM1-2-LR")
# models <- c("CNRM-ESM2-1","IPSL-CM6A-LR")

outputs <- list()

df.all <- data.frame()
for (cscenario in scenarios){
  for (cmodel in models){
    for (cvariable in variables){

      if (cvariable %in% c("tas","pr")){
        nc.file <- file.path("/data/gent/vo/000/gvo00074/CMIP6/results/files_1985_2014_mean/",
                             cscenario,cvariable,
                             paste(cvariable,cmodel,"scenar",cscenario,"nc",sep = "."))
      } else {
        nc.file <- file.path("/data/gent/vo/000/gvo00074/CMIP6/results/files_1985_2014_mean/",
                             cscenario,
                             paste(cvariable,cmodel,"scenar",cscenario,"nc",sep = "."))
      }


      if (!file.exists(nc.file)) stop()

      nc <- nc_open(nc.file)

      lats <- ncvar_get(nc,"lat")
      lons <- ncvar_get(nc,"lon")

      outputs <- ncvar_get(nc,cvariable)

      cdf <- melt(outputs) %>%
        rename(lon = Var1,
               lat = Var2) %>%
        mutate(lat = lats[lat],
               lon = lons[lon])


      df.all <- bind_rows(list(df.all,
                               cdf %>% mutate(scenario = cscenario,
                                              model = cmodel,
                                              variable = cvariable)))


    }
  }
}


saveRDS(df.all,
        "./outputs/CMIP6.OP.RDS")
