rm(list = ls())

library(epwshiftr)
library(dplyr)
library(CongoAS)
library(tidyr)
library(RNetCDF)
library(stringr)
library(lubridate)
library(reshape2)
library(raster)
library(TrENDY.analyses)
library(ggplot2)
library(ncdf4)
library(RCMIP5)
library(Hmisc)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

experiment <-  c("1pctCO2")
variable <- c("tas")
dest.dir <- file.path("/data/gent/vo/000/gvo00074/felicien/R/outputs")

continent <- "Tropics"

files.downloaded <- list.files(file.path(dest.dir,experiment,variable),
                               pattern = "*.nc",full.names = TRUE)

df.downloaded <- data.frame(id = 1:length(files.downloaded),
                            model = sapply(1:length(files.downloaded),function(ifile) CongoAS::CMIP6name2attributes(files.downloaded[ifile])[["model"]]),
                            variant = sapply(1:length(files.downloaded),function(ifile) CongoAS::CMIP6name2attributes(files.downloaded[ifile])[["variant"]]),
                            experiment = sapply(1:length(files.downloaded),function(ifile) CongoAS::CMIP6name2attributes(files.downloaded[ifile])[["scenario"]]),
                            init_year = sapply(1:length(files.downloaded),function(ifile) CongoAS::CMIP6name2attributes(files.downloaded[ifile])[["init.year"]]),
                            end_year = sapply(1:length(files.downloaded),function(ifile) CongoAS::CMIP6name2attributes(files.downloaded[ifile])[["end.year"]]))

df.filtered <- df.downloaded %>%
  mutate(model.variant = paste(model,variant,sep = "."))
models <- unique(df.downloaded$model)

overwrite = FALSE

model.selection <-
  c("EC-Earth3-Veg","SAM0-UNICON","TaiESM1","GFDL-ESM4","MPI-ESM1-2-LR","UKESM1-0-LL","NorCPM1")

for (cexperiment in experiment){
  for (cmodel in model.selection){

    cdf.files <- df.filtered %>% filter(model == cmodel,
                                        experiment == cexperiment)

    for (cvariant in as.character(unique(cdf.files$variant))){

      ccdf.files <- cdf.files %>% filter(variant == cvariant)

      ncfiles <- files.downloaded[ccdf.files %>% pull(id)]

      if (length(ncfiles) > 0){

        print(paste(cmodel,"-",cvariant))

        OP.file <- file.path("./outputs/",
                             paste0("global.temp.",cexperiment,".",cmodel,".",cvariant,".RDS"))

        if (file.exists(OP.file) & !overwrite){
          next()
        }

        cdf <- read.and.filter.ncfiles(ncfiles,
                                       continent2coord("World"),
                                       var = "tas",
                                       aggr = TRUE)


        global.temp <-
          cdf %>%
          group_by(year,lat,lon) %>%
          summarise(MAT = mean(tas),
                    .groups = "keep") %>%
          mutate(model = cmodel,
                 variant = cvariant,
                 experiment = cexperiment)

        saveRDS(global.temp,OP.file)

      }
    }
  }
}


# scp /home/femeunier/Documents/projects/CongoAS/scripts/compile.tas.CMIP6_v2.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R

