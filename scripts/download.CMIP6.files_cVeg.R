rm(list = ls())

library(epwshiftr)
library(dplyr)
library(lubridate)

# scenarios = c("historical","ssp126","ssp245","ssp370","ssp585","ssp370")
# variables = c("tasmin","tasmax")
# variants = "r1i1p1f1"

scenarios = c("piControl","1pctCO2")
variables = c("cVeg","tas")
variants = NULL
models = NULL

########################################################################################################

activities <- rep("ScenarioMIP",length(scenarios))
activities[scenarios %in% c("historical","1pctCO2","piControl","abrupt-4xCO2")] <- "CMIP"

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

data.nodes.status <- get_data_node()

######################################################################################################################
# First download Vegetation

overwrite = FALSE

for (iscenario in seq(1,length(scenarios))){
  for (ivar in seq(1,length(variables))){

    dest.dir.scenar <- file.path("./outputs/",
                                 scenarios[iscenario])
    dir.create(dest.dir.scenar,showWarnings = FALSE)

    dest.dir <- file.path(dest.dir.scenar,variables[ivar])
    dir.create(dest.dir,showWarnings = FALSE)


    pr <- init_cmip6_index(activity = activities[iscenario],
                           variable = variables[ivar],
                           frequency = "mon",
                           experiment = scenarios[iscenario],
                           source = NULL,
                           variant = variants,
                           replica = FALSE,
                           latest = TRUE,
                           resolution = NULL,
                           limit = 10000L,
                           data_node = NULL) %>%
      mutate(init_year = year(as.Date(datetime_start)),
             end_year = year(as.Date(datetime_end)))

    files2download.prc <- pr %>%
      filter(data_node %in% (data.nodes.status %>% filter(status == "UP") %>% pull(data_node)))

    models <- unique(files2download.prc$source_id)
    models2download <- files2download.prc %>%
      # filter(source_id == "GFDL-ESM4") %>%
      group_by(source_id) %>%
      group_by(experiment_id, member_id,source_id) %>%
      filter(experiment_id == "1pctCO2" |
               (experiment_id == "piControl" &
                  (init_year >= (max(end_year) - 150)) |
                  (init_year <= (max(end_year) - 150) & end_year >= (max(end_year) - 150)))) %>%
      ungroup()

    print(paste("--- Downloading files for scenario ",scenarios[iscenario], " and variable ",variables[ivar]))

    if (nrow(models2download) == 0) next()

    for (i in seq(1,nrow(models2download))){

      dest.file <- file.path(dest.dir,
                             basename(models2download$file_url[i]))

      if (!file.exists(dest.file) | file.info(dest.file)[["size"]] == 0 | overwrite){

        dumb <- retry.func(utils::download.file(url = models2download$file_url[i],
                                                destfile = dest.file),
                           maxErrors = 5,
                           sleep = 0)

      }
    }
  }
}

# scp /home/femeunier/Documents/projects/CongoAS/scripts/download.CMIP6.files_cVeg.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R
