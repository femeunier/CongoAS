rm(list = ls())

library(ncdf4)
library(stringr)
library(CongoAS)
library(RNetCDF)
library(ncdf4.helpers)
library(reshape2)
library(lubridate)
library(dplyr)

files <- readRDS("./outputs/CMIP5.2.download.RDS") %>%
  dplyr::select(filename,model,scenario,variant,year_init,year_end) %>%
  distinct()

models <- sort(unique(files$model)) ; scenarios <- sort(unique(files$scenario))

var <- "nbp"

overwrite <- TRUE
for (cmodel in models){
  for (cscenario in scenarios){

    print(paste0(cmodel," - ",cscenario))

    cdir <- file.path("/data/gent/vo/000/gvo00074/felicien/CMIP5",cscenario,var)
    cfiles <- files %>%
      filter(scenario == cscenario,
             model == cmodel)

    if (nrow(cfiles) == 0) {
      next()
    }

    cOP.file <- file.path("./outputs/",paste0("CMIP5.monthly.",var,".global.",cscenario,".",cmodel,".r1i1p1f1.RDS"))

    if (file.exists(cOP.file) & !overwrite){
      next()
    }

    cfiles2read <- file.path(cdir,cfiles$filename)
    cfiles2read <- cfiles2read[file.exists(cfiles2read)]

    tmp <- tryCatch(read.and.filter.ncfiles(ncfiles = cfiles2read,
                                            coord.analysis = continent2coord("World"),
                                            var = "nbp",
                                            aggr = FALSE) %>%
                      filter(year %in% 1900:2100) %>%
                      dplyr::select(-time0) %>%
                      ungroup() %>%
                      distinct(),
                    error = function(e) NULL)

    if (is.null(tmp)){
      warning(paste0("Failed for ",cmodel,"-",scenario))
      next()
    }

    saveRDS(tmp,cOP.file)

  }
}

# scp /home/femeunier/Documents/projects/CongoAS/scripts/read.all.CMIP5.R hpc:/data/gent/vo/000/gvo00074/felicien/R/



