rm(list = ls())

library(dplyr)

system2("rsync",
        paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/global.warming.*",
              "./outputs/"))

cf <- list.files("./outputs/",
           pattern = "global.warming.*",full.names = TRUE)

global.warming <- data.frame()

for (i in seq(1,length(cf))){

  cdata <- readRDS(cf[i])
  if (! all(c("model","variant","experiment") %in% colnames(cdata))){
    next()
  }

  if (!grepl(pattern = "1pctCO2",basename(cf[i]))){
    next()
  }

  global.warming <- bind_rows(list(
    global.warming,
    cdata %>% ungroup() %>%
      mutate(year = year - min(year),
             tas.m = tas.m - 273.15)
  ))

}


saveRDS(global.warming,
        "./outputs/global.warming.RDS")

