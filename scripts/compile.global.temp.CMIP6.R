rm(list = ls())

library(dplyr)

system2("rsync",
        paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/global.temp.*",
              "./outputs/"))

cf <- list.files("./outputs/",
                 pattern = "global.temp.*",full.names = TRUE)

global.warming <- data.frame()

model.selection <-
  c("EC-Earth3-Veg","SAM0-UNICON","TaiESM1","GFDL-ESM4","MPI-ESM1-2-LR","UKESM1-0-LL","NorCPM1")

for (i in seq(1,length(cf))){

  print(i/length(cf))

  if (!grepl(paste(model.selection,collapse = "|"),
            cf[i])){
    next()
  }

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
             MAT = MAT - 273.15)
  ))

}

unique(global.warming$model)

saveRDS(global.warming,
        "./outputs/global.temp.RDS")

