rm(list = ls())

library(epwshiftr)
library(dplyr)

scenarios = c("historical","ssp126","ssp245","ssp370","ssp585")
variables = c("tas")
variants = "r1i1p1f1"

# scenarios <-c("land-cClim","land-cCO2")  #"land-hist")
# scenarios <-c("land-cClim","land-cCO2","land-noLu","land-hist")  #"land-hist")
# scenarios <- c("1pctCO2","piControl")
# variables = c("cVeg","cRoot")
# variables = c("tas","cVeg")

overwrite = FALSE

# scenarios = c("historical")
# variables = "tas"
# variants = "r1i1p1f1"

########################################################################################################

activities <- rep("ScenarioMIP",length(scenarios))
activities[scenarios %in% c("historical","1pctCO2")] <- "CMIP"
activities[grepl("land",scenarios)] <- "LUMIP"

########################################################################################################
# Functions

download_size <- function(url) {
  as.numeric(httr::HEAD(url)$headers$`content-length`)
}

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

options(timeout=6000)

# data.nodes.status <- get_data_node()

######################################################################################################################
# First download Vegetation


for (iscenario in seq(1,length(scenarios))){
  for (ivar in seq(1,length(variables))){

    dest.dir.scenar <- file.path("/data/gent/vo/000/gvo00074/felicien/CMIP6",
                                 scenarios[iscenario])
    dir.create(dest.dir.scenar,showWarnings = FALSE)

    dest.dir <- file.path(dest.dir.scenar,variables[ivar])
    dir.create(dest.dir,showWarnings = FALSE)


    pr <- init_cmip6_index(activity = activities[iscenario],
                           variable = variables[ivar],
                           frequency = 'mon',
                           experiment = scenarios[iscenario],
                           source = NULL,
                           variant = variants,
                           replica = FALSE,
                           latest = TRUE,
                           resolution = NULL,
                           limit = 10000L,
                           data_node = NULL)

    if (nrow(pr) == 0) next()

    files2download.prc <- pr %>%
      mutate(Id = 1:n())# %>%
      # filter(data_node %in% (data.nodes.status %>% filter(status == "UP") %>% pull(data_node)))

    print(paste("--- Downloading files for scenario ",scenarios[iscenario], " and variable ",variables[ivar]))
    print(unique(files2download.prc$source_id))

    if (nrow(files2download.prc) == 0) next()

    models <- unique(files2download.prc$source_id)

    for (imodel in seq(1, length(models))){

      cmodel <- models[imodel]
      print(cmodel)
      cdf <- files2download.prc %>%
        filter(source_id == cmodel)

      if (nrow(cdf) == 0) next()

      for (i in seq(1,nrow(cdf))){

        dest.file <- file.path(dest.dir,
                               basename(cdf$file_url[i]))

        if (overwrite | !file.exists(dest.file) | file.info(dest.file)[["size"]] == 0){

          dumb <- retry.func(utils::download.file(url = cdf$file_url[i],
                                                  destfile = dest.file),
                             maxErrors = 2,
                             sleep = 0)

          if (dumb == 0){
            break
          }


        }
      }
    }
  }
}

# scp /home/femeunier/Documents/projects/CongoAS/scripts/download.CMIP6.files.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R
