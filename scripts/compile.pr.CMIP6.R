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

experiments <-  c("1pctCO2")
variables <- c("pr")
dest.dir <- file.path("/data/gent/vo/000/gvo00074/felicien/R/outputs")

continent <- "Tropics"

all.cVeg <-
  init_cmip6_index(activity = "CMIP",
                   variable = variables,
                   frequency = 'mon',
                   experiment = experiments,
                   source = NULL,
                   variant = NULL,
                   replica = FALSE,
                   latest = TRUE,
                   resolution = NULL,
                   limit = 10000L,
                   data_node = NULL)

experiment_id <- all.cVeg$experiment_id
names_id <- basename(all.cVeg$file_url)

files <- file.path(dest.dir,experiment_id,variables,names_id)
files.downloaded <- files[file.exists(files)]
files.downloaded <- files.downloaded[grepl("_Amon_",files.downloaded)]


all.cVeg.existing <- all.cVeg %>%
  rename(model = source_id,
         experiment = experiment_id,
         variant = member_id) %>%
  group_by(experiment,model,variant) %>%
  summarise(N = length(frequency),.groups = "keep")

df <- data.frame(id = 1:length(files.downloaded),
                 model = sapply(1:length(files.downloaded),function(ifile) CongoAS::CMIP6name2attributes(files.downloaded[ifile])[["model"]]),
                 variant = sapply(1:length(files.downloaded),function(ifile) CongoAS::CMIP6name2attributes(files.downloaded[ifile])[["variant"]]),
                 experiment = sapply(1:length(files.downloaded),function(ifile) CongoAS::CMIP6name2attributes(files.downloaded[ifile])[["scenario"]]),
                 init_year = sapply(1:length(files.downloaded),function(ifile) CongoAS::CMIP6name2attributes(files.downloaded[ifile])[["init.year"]]),
                 end_year = sapply(1:length(files.downloaded),function(ifile) CongoAS::CMIP6name2attributes(files.downloaded[ifile])[["end.year"]]))

df.downloaded <- df %>%
  group_by(model,experiment,variant) %>%
  summarise(Ndown = length(experiment),
            init_year = min(init_year),
            end_year = max(end_year),
            .groups = "keep")


models2keep <- all.cVeg.existing %>%
  left_join(df.downloaded %>% dplyr::select(-c(init_year,end_year)),
            by = c("model","experiment","variant")) %>%
  filter(Ndown >= 1) %>%
  dplyr::select(-N) %>%
  pivot_wider(names_from = experiment,
              values_from = Ndown) %>%
  filter(`1pctCO2` >= 1) %>%
  mutate(model.variant = paste(model,variant,sep = "."))

df.filtered <- df %>%
  mutate(model.variant = paste(model,variant,sep = ".")) %>%
  dplyr::filter(model.variant %in% (models2keep[["model.variant"]]))

models <- unique(models2keep$model)

overwrite = FALSE

for (cexperiment in experiments){
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
                                       continent2coord("World"),
                                       var = "pr",
                                       aggr = TRUE)


        global.precip <-
          cdf %>%
          group_by(year,lat,lon) %>%
          summarise(MAP = mean(pr)*365*86400,
                    .groups = "keep") %>%
          mutate(model = cmodel,
                 variant = cvariant,
                 experiment = cexperiment)

        saveRDS(global.precip,OP.file)

      }
    }
  }
}


# scp /home/femeunier/Documents/projects/CongoAS/scripts/compile.pr.CMIP6.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R

