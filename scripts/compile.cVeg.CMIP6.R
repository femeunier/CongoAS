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

overwrite = FALSE

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

experiments <-  c("1pctCO2","piControl")
dest.dir <- file.path("/data/gent/vo/000/gvo00074/felicien/R/outputs")

continent <- "Tropics"
coord.list <- continent2coord(continent)
min.lon.plot <- coord.list[[2]][1]; max.lon.plot <- coord.list[[2]][2]; min.lat.plot <- coord.list[[2]][3]; max.lat.plot <- coord.list[[2]][4]
min.lon.analysis <- coord.list[[1]][1]; max.lon.analysis <- coord.list[[1]][2]; min.lat.analysis <- coord.list[[1]][3]; max.lat.analysis <- coord.list[[1]][4]


all.cVeg <-
  init_cmip6_index(activity = "CMIP",
                   variable = "cVeg",
                   frequency = 'mon',
                   experiment = experiments,
                   source = NULL,
                   variant = NULL,
                   replica = FALSE,
                   latest = TRUE,
                   resolution = NULL,
                   limit = 10000L,
                   data_node = NULL) %>%
  mutate(init_year = year(as.Date(datetime_start)),
         end_year = year(as.Date(datetime_end))) %>%
  group_by(experiment_id,member_id,source_id) %>%
  filter(experiment_id == "1pctCO2" |
           (experiment_id == "piControl" &
           (init_year >= (max(end_year) - 150)) |
           (init_year <= (max(end_year) - 150) & end_year >= (max(end_year) - 150)))
         ) %>%
  ungroup()

experiment_id <- all.cVeg$experiment_id
names_id <- basename(all.cVeg$file_url)

files <- file.path(dest.dir,experiment_id,"cVeg",names_id)
files.downloaded <- files[file.exists(files)]

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
  filter(`1pctCO2` >= 1,
         piControl >= 1) %>%
  mutate(model.variant =
           paste(model,variant,sep = "."))

df.filtered <- df %>%
  mutate(model.variant = paste(model,variant,sep = ".")) %>%
  dplyr::filter(model.variant %in% (models2keep[["model.variant"]]))



all.tas <-
  init_cmip6_index(activity = "CMIP",
                   variable = "tas",
                   frequency = 'mon',
                   experiment = experiments,
                   source = NULL,
                   variant = NULL,
                   replica = FALSE,
                   latest = TRUE,
                   resolution = NULL,
                   limit = 10000L,
                   data_node = NULL)

for (cmodel in unique(models2keep$model)){

  cdf.files <- df.filtered %>% filter(model == cmodel)

  for (cvariant in as.character(unique(cdf.files$variant))){

    ccdf.files <- cdf.files %>% filter(variant == cvariant)

    ncfiles <- files.downloaded[ccdf.files %>%
                                  filter(experiment == "1pctCO2") %>%
                                  pull(id)]

    ncfiles.cfiles <- files.downloaded[ccdf.files %>%
                                        filter(experiment == "piControl") %>%
                                        pull(id)]

    if (length(ncfiles) > 0 & length(ncfiles.cfiles) > 0){

      print(paste(cmodel,"-",cvariant))

      OP.file <- file.path("./outputs/",paste0("model.OP.",cmodel,".",cvariant,".RDS"))

      if (!file.exists(OP.file) | overwrite){
        model.OP <- detect.AS(location = "Tropics",
                              ncfiles,
                              ncfiles.cfiles)

        saveRDS(model.OP,OP.file)
      }


      OP.file.tmp <- file.path("./outputs/",paste0("model.OP.Tmp.",cmodel,".",cvariant,".RDS"))

      if (!file.exists(OP.file.tmp) | overwrite){

        tas.files <- all.tas %>%
          filter(source_id == cmodel,
                 member_id == cvariant,
                 experiment_id == "1pctCO2")

        if (nrow(tas.files) == 0) next()

        tas.files.path <- file.path(dest.dir,"1pctCO2","tas",basename(tas.files$file_url))
        if (all(file.exists(tas.files.path))){

          Tmp <- read.and.filter.ncfiles(ncfiles = tas.files.path,
                                         coord.analysis = continent2coord("World"),
                                         var = "tas",
                                         aggr = TRUE,
                                         progressbar = FALSE) %>%

            mutate(tas = tas -273.15) %>%
            group_by(lat, lon) %>%
            mutate(ID = cur_group_id())


          saveRDS(Tmp,OP.file.tmp)

      }

      }
    }
  }
}



# scp /home/femeunier/Documents/projects/CongoAS/scripts/compile.cVeg.CMIP6.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R

