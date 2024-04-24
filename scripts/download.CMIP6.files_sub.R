rm(list = ls())

library(epwshiftr)
library(dplyr)

# scenarios = c("historical","ssp370")
# variables = "pr"
# variants = "r1i1p1f1"

scenarios = c("historical")
variables = "tas"
variants = "r1i1p1f1"

########################################################################################################

activities <- rep("ScenarioMIP",length(scenarios))
activities[scenarios %in% c("historical","1pctCO2")] <- "CMIP"

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

options(timeout=600)

data.nodes.status <- get_data_node()

######################################################################################################################
# First download Vegetation

nep.files <- init_cmip6_index(activity = "CMIP",
                       variable = "nep",
                       frequency = 'mon',
                       experiment = "historical",
                       source = NULL,
                       variant = NULL,
                       replica = FALSE,
                       latest = TRUE,
                       resolution = NULL,
                       limit = 10000L,
                       data_node = NULL)

models <- unique(nep.files$source_id)


for (iscenario in seq(1,length(scenarios))){
  for (ivar in seq(1,length(variables))){

    dest.dir.scenar <- file.path("/data/gent/vo/000/gvo00074/felicien/CMIP6",
                                 scenarios[iscenario])
    dir.create(dest.dir.scenar,showWarnings = FALSE)

    dest.dir <- file.path(dest.dir.scenar,variables[ivar])
    dir.create(dest.dir,showWarnings = FALSE)


    pr <- init_cmip6_index(activity = activities[iscenario],
                           variable = variables[ivar],
                           frequency = "mon",
                           experiment = scenarios[iscenario],
                           source = NULL,
                           variant = NULL,
                           replica = FALSE,
                           latest = TRUE,
                           resolution = NULL,
                           limit = 10000L,
                           data_node = NULL) %>%
      filter(source_id %in% models)

    files2download.prc <- pr %>% filter(member_id %in% variants,
                                        data_node %in% (data.nodes.status %>% filter(status == "UP") %>% pull(data_node)))

    print(paste("--- Downloading files for scenario ",scenarios[iscenario], " and variable ",variables[ivar]))

    for (i in seq(1,nrow(files2download.prc))){

      dest.file <- file.path(dest.dir,
                             basename(files2download.prc$file_url[i]))

      if (!file.exists(dest.file) | file.info(dest.file)[["size"]] == 0){

        dumb <- retry.func(utils::download.file(url = files2download.prc$file_url[i],
                                                destfile = dest.file),
                           maxErrors = 5,
                           sleep = 0)

      }
    }
  }
}

# scp /home/femeunier/Documents/projects/CongoAS/scripts/download.CMIP6.files_sub.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R
