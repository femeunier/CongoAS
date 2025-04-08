rm(list = ls())

library(dplyr)
library(ggplot2)

scenarios <- c("historical","ssp126","ssp245","ssp370","ssp585")
# scenarios <- c("historical","ssp245","ssp585")

# dir <- "~/Documents/projects/CongoAS/outputs/CMIP6.monthly.nbp.global."
dir <- "./outputs/CMIP6.monthly.nbp.global."

A.all <- data.frame()
models <- unique(sapply(strsplit(list.files(dirname(dir),
                                            pattern = "CMIP6.monthly.nbp.global.*"),split = "\\."),
                        "[",6))

# models <- "E3SM-1-1-ECA"
# models <- "E3SM-1-1"

for (cmodel in models){
  for (cscenario in scenarios){

    print(paste0(cmodel," - ",cscenario))

    cfile <- paste0(dir,cscenario,".",cmodel,".r1i1p1f1_rspld.RDS")

    if (!file.exists(cfile)){
      print(paste(cfile, "doesn't exist"))
      next()
    }

    A <- readRDS(cfile)

    A.all <- bind_rows(A.all,
                       A %>%
                         rename(nbp = value) %>%
                         ungroup() %>%
                         filter(!is.na(nbp)) %>%
                         mutate(scenario = cscenario,
                                variant = case_when(is.na(variant) ~ "r1i1p1f1",
                                                    TRUE ~ variant),
                                model = cmodel))

  }
}

saveRDS(A.all,
        "./outputs/Emergent.World.rspld.RDS")

# scp /home/femeunier/Documents/projects/CongoAS/scripts/Emergent.constraints.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
