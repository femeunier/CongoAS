rm(list = ls())

# First install and load those libraries (only first time)
# install.packages(c("devtools","epwshiftr","dplyr","lubridate","RNetCDF","reshape2","stringr","ggplot2","sf","rnaturalearth","rnaturalearthdata"))
# library(devtools)
# devtools::install_github("femeunier/CongoAS")

# Load libraries
library(epwshiftr)
library(dplyr)
library(lubridate)
library(RNetCDF)
library(reshape2)
library(stringr)
library(CongoAS)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)

# User-defined options
main.op.dir <- "/data/gent/vo/000/gvo00074/CMIP6"   # where the files will be stored
aggr <- TRUE                                         # aggregate output per year

# Information about the simulations you want to download
scenarios = c("abrupt-4xCO2")  # vector of "scenarios" = experiment_id
variables = "nep"              # vector of outputs
variants = "r1i1p1f1"          # simulation variants --> let's not change it at the moment

########################################################################################################

# define activity_id, see https://wcrp-cmip.github.io/CMIP6_CVs/docs/CMIP6_experiment_id.html, to improve

activities <- rep("ScenarioMIP",length(scenarios))
activities[scenarios %in% c("historical","1pctCO2","piControl","abrupt-4xCO2")] <- "CMIP"

########################################################################################################
# Functions

# Define some useful function

# Check size of the file to download
download_size <- function(url) {
  as.numeric(httr::HEAD(url)$headers$`content-length`)
}

# Retry function, bc sometimes download fails for no good reason
retry.func <- function (expr, isError = function(x) inherits(x, "try-error"),
                        maxErrors = 5, sleep = 0) {
  attempts = 0
  retval = try(eval(expr))
  while (isError(retval)) {
    attempts = attempts + 1
    if (attempts >= maxErrors) {
      msg = sprintf("retry: too many retries [[%s]]",
                    utils::capture.output(utils::str(retval)))
      # warning(msg)
      return(0)
    }
    else {
      msg = sprintf("retry: error in attempt %i/%i [[%s]]",
                    attempts, maxErrors, utils::capture.output(utils::str(retval)))
      # warning(msg)
    }
    if (sleep > 0)
      Sys.sleep(sleep)
    retval = try(eval(expr))
  }
  return(1)
}

######################################################################################################################
# For large files, we increase the potential download duration before it gets killed
options(timeout=6000,
        bitmapType='cairo')

# Check what's the current status of the downloading nodes, only if "UP" they are available for download
data.nodes.status <- get_data_node()

# we create the output folder if it does not exist
dir.create(main.op.dir,showWarnings = FALSE)
dir.create(file.path(main.op.dir,"data"),
           showWarnings = FALSE)

######################################################################################################################
# First we download the CMIP6 output files

for (iscenario in seq(1,length(scenarios))){          # loop over the scenario
  for (ivar in seq(1,length(variables))){             # loop over the output variables

    # Create the output/scenario specific folder
    dest.dir.scenar <- file.path(main.op.dir,"data",
                                 scenarios[iscenario])
    dir.create(dest.dir.scenar,showWarnings = FALSE)

    dest.dir <- file.path(dest.dir.scenar,variables[ivar])
    dir.create(dest.dir,showWarnings = FALSE)

    # List the existing files for the current scenario/output selection

    files <- init_cmip6_index(activity = activities[iscenario],
                              variable = variables[ivar],
                              frequency = 'mon',                       # could be "yr" instead, see init_cmip6_index options
                              experiment = scenarios[iscenario],
                              source = NULL,
                              variant = variants,
                              years = NULL,                            # here we can subset the years that we are interested in
                              replica = FALSE,
                              latest = TRUE,
                              resolution = NULL,
                              limit = 10000L,
                              data_node = NULL)

    # For now, we will only download outputs from a few models (n = 3) for the nodes that are workin (UP) and we focus on the model with a limited number of files (arrange/slice)
    model2download <-
      files %>% filter(data_node %in% (data.nodes.status %>% filter(status == "UP") %>% pull(data_node))) %>%
      group_by(source_id) %>%
      summarise(N = length(source_id)) %>%
      arrange(N) %>%
      slice_head(n = 3)

    # Subset the files available for download
    files2download <- files %>% filter(data_node %in% (data.nodes.status %>% filter(status == "UP") %>% pull(data_node))) %>%
      filter(source_id %in% (model2download %>% pull(source_id)))  # for this test, we only download and extract two models!

    # Entertain the user
    print(paste("=== Downloading files for scenario ",scenarios[iscenario], " and variable ",variables[ivar]," ==="))

    for (i in seq(1,nrow(files2download))){

      # we define the destination file
      dest.file <- file.path(dest.dir,
                             basename(files2download$file_url[i]))

      # We only download if the file does not exist yet or if its size is null (previous failed download)
      if (!file.exists(dest.file) | file.info(dest.file)[["size"]] == 0){

        dumb <- retry.func(utils::download.file(url = files2download$file_url[i],
                                                destfile = dest.file),
                           maxErrors = 5,
                           sleep = 0)

      }
    }
  }
}


############################################################################################################
# Second, we extract the outputs from the netcdf files

models <- unique(files2download$source_id) # unique models

df.outputs <- data.frame()

dir.create(file.path(main.op.dir,"outputs"),
           showWarnings = FALSE)

for (i in seq(1,length(variables))){

  var.name <- variables[i]

  for (j in seq(1,length(scenarios))){

    scenario <- scenarios[j]
    activity <- activities[j]


    for (imodel in seq(1,length(models))){    # Loop over the model

      # Let's give some info to the user that should be bored at this point

      print(paste("=== Reading model output files ==="))
      print("==================================")
      print(paste("==",models[imodel],"==",sprintf("%.2f",100*imodel/length(models)),"% =="))

      # Files of this specific model
      cfiles <- files2download %>% filter(source_id == models[imodel])
      init.file <- cfiles[1]

      # Multiple initial files?
      if (sum(year(cfiles[["datetime_start"]]) == year(init.file[["datetime_start"]])) > 1){
        cfiles <- cfiles[1:(which(year(cfiles[["datetime_start"]]) == year(init.file[["datetime_start"]]))[2] -1),]
      }

      # No initial file? Skip
      if (!any(year(cfiles[["datetime_start"]]) == year(init.file[["datetime_start"]]))){
        next()
      }

      model.files <- file.path(main.op.dir,"data",scenario,var.name,basename(cfiles$file_url))

      if (all(file.exists(model.files))){  # only extract if they all exist

        tmp <- tryCatch(read.and.filter.ncfiles(model.files,
                                                coord.analysis = continent2coord("Tropics"),
                                                var = var.name,
                                                aggr = aggr,
                                                start.year = year(cfiles[["datetime_start"]])),
                        error = function(e) NULL)

        if(is.null(tmp)) next()

        if (aggr){
          cdf.outputs <- tmp %>%
            dplyr::select(lat,lon,yr,!!var.name)
        } else {
          cdf.outputs <- tmp %>%
            dplyr::select(lat,lon,yr,time,!!var.name)
        }


        df.outputs <- bind_rows(list(df.outputs,
                                     cdf.outputs %>% mutate(model = models[imodel])))
      } else {
        print(paste("Files",
                    model.files[!file.exists(model.files)],"do not exist"))
      }
    }

    saveRDS(df.outputs,file.path(main.op.dir,"outputs",paste(var.name,"CMIP6","scenar",scenario,"RDS",
                                                             sep = ".")))

  }
}

#######################################################################
# Finally, we re-load the created file and try to plot smt

CMIP6.OP <- readRDS(file.path(main.op.dir,"outputs",paste(var.name,"CMIP6","scenar",scenario,"RDS",
                                                          sep = ".")))

CMIP6.OP.sum <- CMIP6.OP %>%
  group_by(model,lon,lat) %>%
  summarise(nep.m = mean(nep,na.rm = TRUE),
            .groups = "keep")

# Country map
world <- ne_countries(scale = "medium", returnclass = "sf")

coord.list <- continent2coord("Tropics")
min.lon.analysis <- coord.list[[1]][1]; max.lon.analysis <- coord.list[[1]][2]; min.lat.analysis <- coord.list[[1]][3]; max.lat.analysis <- coord.list[[1]][4]

ggplot(data = world) +
  geom_raster(data = CMIP6.OP.sum,
            aes(x = lon, y = lat,fill = nep.m),na.rm = TRUE) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(min.lon.analysis, max.lon.analysis),
           ylim = c(min.lat.analysis, max.lat.analysis),
           expand = FALSE) +
  facet_wrap(~ model) +
  labs(x = "",y = "") +
  theme_bw()

dir.create(file.path(main.op.dir,"Figures"),
           showWarnings = FALSE)

ggsave(plot = last_plot(),
       filename = file.path(main.op.dir,"Figures",paste(var.name,"CMIP6","scenar",scenario,"Fig1","png",
                                                        sep = ".")),
       dpi = 300, height = 15, width = 25, units = "cm")

CMIP6.OP.ts <- CMIP6.OP %>%
  mutate(cont = coord2continent(lon)) %>%
  group_by(model,yr,cont) %>%
  summarise(nep.m = mean(nep,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(model) %>%
  mutate(yr.rel = yr - yr[1])

ggplot(data = CMIP6.OP.ts) +
  geom_line(aes(x = yr.rel, y = nep.m,color = cont),na.rm = TRUE) +
  facet_wrap(~ model) +
  # scale_y_log10() +
  theme_bw()

ggsave(plot = last_plot(),
       filename = file.path(main.op.dir,"Figures",paste(var.name,"CMIP6","scenar",scenario,"Fig2","png",
                                                        sep = ".")),
       dpi = 300, height = 15, width = 25, units = "cm")

system2("chmod",paste("777","-R",main.op.dir))

# scp /home/femeunier/Documents/projects/CongoAS/scripts/Example.download.and.process.CMIP6.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R
