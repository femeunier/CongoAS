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
library(RcppRoll)
library(zoo)
library(YGB)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

experiment <-  c("1pctCO2")
variable <- c("pr")
dest.dir <- file.path("/data/gent/vo/000/gvo00074/felicien/R/outputs")
# dest.dir <- file.path("/home/femeunier/Documents/projects/CongoAS/outputs/example/")

continent <- "Tropics"

files.downloaded <- list.files(file.path(dest.dir,experiment,variable),
                 pattern = "*.nc",full.names = TRUE)

df.downloaded <- data.frame(id = 1:length(files.downloaded),
                 model = sapply(1:length(files.downloaded),function(ifile) CongoAS::CMIP6name2attributes(files.downloaded[ifile])[["model"]]),
                 variant = sapply(1:length(files.downloaded),function(ifile) CongoAS::CMIP6name2attributes(files.downloaded[ifile])[["variant"]]),
                 experiment = sapply(1:length(files.downloaded),function(ifile) CongoAS::CMIP6name2attributes(files.downloaded[ifile])[["scenario"]]),
                 init_year = sapply(1:length(files.downloaded),function(ifile) CongoAS::CMIP6name2attributes(files.downloaded[ifile])[["init.year"]]),
                 end_year = sapply(1:length(files.downloaded),function(ifile) CongoAS::CMIP6name2attributes(files.downloaded[ifile])[["end.year"]]))

model.selection <-
  c("EC-Earth3-Veg","SAM0-UNICON","TaiESM1","GFDL-ESM4","MPI-ESM1-2-LR","UKESM1-0-LL","NorCPM1")

df.filtered <- df.downloaded %>%
  mutate(model.variant = paste(model,variant,sep = ".")) %>%
  filter(model %in% model.selection)

models <- unique(df.downloaded$model)


overwrite = TRUE

for (cexperiment in experiment){
  for (cmodel in models){

    cdf.files <- df.filtered %>% filter(model == cmodel,
                                        experiment == cexperiment)

    for (cvariant in as.character(unique(cdf.files$variant))){

      ccdf.files <- cdf.files %>% filter(variant == cvariant)

      ncfiles <- files.downloaded[ccdf.files %>% pull(id)]

      if (length(ncfiles) > 0){

        print(paste(cmodel,"-",cvariant))

        OP.file <- file.path("./outputs/",
                             paste0("global.precip.",cexperiment,".",cmodel,".",cvariant,".RDS"))

        if (file.exists(OP.file) & !overwrite){
          next()
        }

        cdf <- read.and.filter.ncfiles(ncfiles,
                                       coord.analysis = list(coord.analysis = c(-180,180,
                                                                                -23,23)),
                                       var = "pr",
                                       # years = 1:25,
                                       aggr = FALSE)

        # coords <- cdf %>%
        #   dplyr::select(lat,lon) %>%
        #   distinct()
        # X <- 6000

        cdf.diff <- cdf %>%
          # ungroup() %>%
          # filter(lat == coords[X,1],
          #        lon == coords[X,2]) %>%
          group_by(year,lon,lat) %>%
          mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
                 E = 3.33,
                 Etot = E*Ndays) %>%
          mutate(Pmm = mean(pr)*Ndays*86400) %>%

          mutate(diff = Pmm - Etot) %>%
          mutate(wettest.month = which.max(diff)) %>%
          mutate(month.ord = 1 + (month - wettest.month)) %>%
          mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                                       TRUE ~ month.ord)) %>%
          arrange(year,month.ord) %>%
          mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                                 TRUE ~ NA_real_)) %>%
          mutate(CWD = calc.CWD(diff,CWD[1])) %>%
          arrange(year,month) %>%
          mutate(MCWD.year = min(CWD),
                 MAP.year = sum(Pmm)) %>%
          dplyr::select(year,month,lon,lat,MCWD.year,MAP.year,diff,Pmm) %>%
          distinct()

        global.precip <-
          cdf.diff %>%
          group_by(lat,lon) %>%
          mutate(MCWD = rollapply(diff, 12,
                                  function(x) calc.MCWD(x),
                                  fill = NA,
                                  align = c("right")),
                 MAP = roll_sum(Pmm, 12, align = "right", fill = NA)) %>%
          dplyr::select(year,month,lon,lat,MCWD,MAP,MCWD.year,MAP.year) %>%
          mutate(time = year + (month-1)/12) %>%
          group_by(year,lat,lon) %>%
          summarise(MCWD.year = unique(MCWD.year),
                    MAP.year = unique(MAP.year),
                    MAP.m = mean(MAP),
                    MCWD.m = mean(MCWD),
                    MAP.min = min(MAP),
                    MCWD.min = min(MCWD),

                    .groups = "keep") %>%
          ungroup() %>%
          mutate(model = cmodel,
                 variant = cvariant,
                 experiment = cexperiment,
                 MAP.m = case_when(is.na(MAP.m) ~ MAP.year,
                                   TRUE ~ MAP.m),
                 MAP.min = case_when(is.na(MAP.min) ~ MAP.year,
                                   TRUE ~ MAP.min),
                 MCWD.m = case_when(is.na(MCWD.m) ~ MCWD.year,
                                   TRUE ~ MCWD.m),
                 MCWD.min = case_when(is.na(MCWD.min) ~ MCWD.year,
                                     TRUE ~ MCWD.min))

        # plot(global.precip$year,global.precip$MAP.year, type = "l")
        # lines(global.precip$year,global.precip$MAP.min, col = "red")
        # lines(global.precip$year,global.precip$MAP.m, col = "darkgreen")
        #
        #
        # plot(global.precip$year,global.precip$MCWD.year, type = "l")
        # lines(global.precip$year,global.precip$MCWD.min, col = "red")
        # lines(global.precip$year,global.precip$MCWD.m, col = "darkgreen")


        saveRDS(global.precip,
                OP.file)

      }
    }
  }
}


# scp /home/femeunier/Documents/projects/CongoAS/scripts/compile.pr.CMIP6_v2.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R

