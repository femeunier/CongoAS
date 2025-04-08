rm(list = ls())

library(dplyr)
library(tidyr)

# Get the wget files https://aims2.llnl.gov/search
files <- list.files("~/Downloads/CMIP5files/","wget_script*",full.names = TRUE)

files2down <- filenames <- c()
for (ifile in seq(1,length(files))){

  print(paste0(ifile,"/",length(files)))
  cfile <- files[ifile]
  A <- readLines(cfile)
  files2down_form <- A[grepl(pattern = "nbp_Lmon",A)]

  files2down <- c(files2down,
                  gsub("'","",sapply(strsplit(files2down_form," "),"[",2)))

}

filenames <- basename(files2down)
Splitted <- strsplit(filenames,"_")
models <- sapply(Splitted,"[",3) ; scenarios <- sapply(Splitted,"[",4) ; variants <- sapply(Splitted,"[",5)
timing <- tools::file_path_sans_ext(sapply(Splitted,"[",6)) ; times_init <- sapply(strsplit(timing,"-"),"[",1) ; times_end <- sapply(strsplit(timing,"-"),"[",2)

df <- data.frame(path = files2down,
                 filename = filenames,
                 model = models,
                 scenario = scenarios,
                 variant = variants,
                 time_init = times_init,
                 time_end = times_end) %>%
  mutate(year_init = as.numeric(substr(time_init,1,4)),
         year_end = as.numeric(substr(time_end,1,4)))

df.selected <- df %>%
  filter(year_init < 2100) %>%
  distinct()

sort(unique(df.selected$model))


View(df.selected %>%
  group_by(model,scenario) %>%
  mutate(init_end = paste0(min(year_init),"_",max(year_end))) %>%
  dplyr::select(model,scenario,init_end) %>%
  distinct() %>%
  mutate(present = TRUE) %>%
  pivot_wider(names_from = scenario,
              values_from = present))

saveRDS(df.selected,"./outputs/CMIP5.2.download.RDS")

# scp /home/femeunier/Documents/projects/CongoAS/outputs/CMIP5.2.download.RDS hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/
