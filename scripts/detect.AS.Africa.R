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

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

experiments <-  c("1pctCO2","piControl")
variables <- c("cVeg")
dest.dir <- file.path("./outputs")

continent <- "Tropics"
coord.list <- continent2coord(continent)
min.lon.plot <- coord.list[[2]][1]; max.lon.plot <- coord.list[[2]][2]; min.lat.plot <- coord.list[[2]][3]; max.lat.plot <- coord.list[[2]][4]
min.lon.analysis <- coord.list[[1]][1]; max.lon.analysis <- coord.list[[1]][2]; min.lat.analysis <- coord.list[[1]][3]; max.lat.analysis <- coord.list[[1]][4]

# To resample
biome <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/biome1961.1990.RDS")
biome.rst <- aggregate(rasterFromXYZ(biome[,c("lon","lat","tmp")]),2)

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
  summarise(Ndown = length(experiment),.groups = "keep")

models2keep <- all.cVeg.existing %>%
  left_join(df.downloaded,
            by = c("model","experiment","variant")) %>%
  filter(N == Ndown) %>%
  dplyr::select(-Ndown) %>%
  pivot_wider(names_from = experiment,
              values_from = N) %>%
  dplyr::filter(!is.na(piControl) & !is.na(`1pctCO2`)) %>%
  mutate(model.variant = paste(model,variant,sep = ".")) %>%
  filter(model %in% c("CESM2","NorCPM1"))

df.filtered <- df %>%
  mutate(model.variant = paste(model,variant,sep = ".")) %>%
  dplyr::filter(model.variant %in% (models2keep[["model.variant"]]))

df.outputs <- data.frame()

for (cexperiment in experiments){
  for (cmodel in unique(models2keep$model)){

    cdf.files <- df.filtered %>% filter(model == cmodel,
                                        experiment == cexperiment)

    for (cvariant in as.character(unique(cdf.files$variant))){
      print(paste(cexperiment,"-",cmodel,"-",cvariant))

      ccdf.files <- cdf.files %>% filter(variant == cvariant)
      cur.files <- files.downloaded[ccdf.files %>% pull(id)]

      if (cexperiment == "piControl"){

        final.year <- ccdf.files[length(cur.files),"end_year"]
        inits <- ccdf.files[,"init_year"]

        if (all((inits > (final.year - 150)))){
          select <- 1:length(inits)
        } else {
          select <- pmax(1,-1+min(which((inits > (final.year - 150))))):max(which((inits > (final.year - 150))))
        }


        cur.files <- cur.files[select]
        ccdf.files <- ccdf.files[select,]

        ccdf.files
      }




      coutput <-  read.and.filter.ncfiles(ncfiles = cur.files,
                                          coord.analysis = coord.list,
                                          progressbar = FALSE,
                                          start.year = NULL,
                                          var = variables,
                                          aggr = TRUE)



      if (cexperiment == "piControl") coutput <- coutput %>% dplyr::filter(year > (max(year) - 150))

      # coutput.rspld <- resample.df.all.col(bigdf = coutput,
      #                                      raster2resample = biome.rst,
      #                                      var.names = "cVeg")


      df.outputs <- bind_rows(list(df.outputs,
                                   coutput %>% mutate(model = cmodel,
                                                      experiment = cexperiment,
                                                      variant = cvariant)))
    }
  }
}

ggplot() +
  geom_raster(data = df.outputs %>%
                group_by(model,experiment) %>%
                filter(year == max(year)),
              aes(x = lon, y = lat,
                  fill = cVeg),na.rm = TRUE, alpha = 1) +
  geom_sf(data = world,
          fill = NA) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  coord_sf(xlim = c(min.lon.plot, max.lon.plot),
           ylim = c(min.lat.plot, max.lat.plot),
           expand = FALSE) +
  facet_grid(experiment ~ model) +
  labs(x = "",y = "") +
  theme_bw()

# Abrupt shifts?

df.AS <- df.outputs %>%
  filter(!is.na(cVeg)) %>%
  group_by(model,experiment,variant,lat,lon) %>%
  summarise(final.cVeg = cVeg[year == max(year)],
            .groups = "keep") %>%
  pivot_wider(names_from = experiment,
              values_from = final.cVeg) %>%
  filter(piControl > 2*`1pctCO2`) %>%
  mutate(model.variant.lat.lon = paste(model,variant,lat,lon,sep = "."))

df.outputs.loc.all <- df.outputs %>%
  mutate(model.variant.lat.lon = paste(model,variant,lat,lon,sep = ".")) %>%
  dplyr::filter(model.variant.lat.lon %in% (df.AS[["model.variant.lat.lon"]])) %>%
  group_by(model,experiment,variant) %>%
  mutate(yr.rel = case_when(experiment == "piControl" ~ 1+ (year - year[1]),
                            TRUE ~ 150 + (year - year[1])))

if (nrow(df.outputs.loc.all) > 0){
  ggplot(data = df.outputs.loc.all) +
    geom_line(aes(x = yr.rel,y = cVeg, color = model, group = interaction(experiment,model.variant.lat.lon))) +
    facet_wrap(~ model) +
    theme_bw()
}

df.AS.map <- df.outputs %>%
  group_by(model) %>%
  dplyr::filter(year == max(year),
                experiment == "piControl",
                cVeg > 0) %>%
  left_join(df.outputs.loc.all %>%
              dplyr::filter(yr.rel == 1,
                            experiment == "piControl") %>%
              ungroup() %>%
              dplyr::select(lat,lon,model) %>% mutate(AS = TRUE),
            by = c("lat","lon","model")) %>%
  mutate(AS = case_when(is.na(AS) ~ FALSE,
                        TRUE ~ TRUE))

ggplot() +
  geom_raster(data = df.AS.map %>%
                filter(AS),
              aes(x = lon, y = lat,
                  fill = AS),na.rm = TRUE, alpha = 1) +
  geom_sf(data = world,
          fill = NA) +
  coord_sf(xlim = c(min.lon.plot, max.lon.plot),
           ylim = c(min.lat.plot, max.lat.plot),
           expand = FALSE) +
  facet_wrap(~ model) +
  labs(x = "",y = "") +
  theme_bw()




