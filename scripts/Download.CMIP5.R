rm(list = ls())

files <- readRDS("./outputs/CMIP5.2.download.RDS")

dest.dir <- "/data/gent/vo/000/gvo00074/felicien/CMIP5"

overwrite <- FALSE
for (i in seq(1,nrow(files))){

  cscenario <- files$scenario[i] ; cvar <- "nbp" ;

  if (!dir.exists( file.path(dest.dir,cscenario))){
    dir.create(file.path(dest.dir,cscenario),showWarnings = FALSE)
  }

  if (!dir.exists( file.path(dest.dir,cscenario,cvar))){
    dir.create(file.path(dest.dir,cscenario,cvar),showWarnings = FALSE)
  }

  dest.file <- file.path(dest.dir,cscenario,cvar,
                         files$filename[i])
  print(paste(i,"-",dest.file))

  if (overwrite | !file.exists(dest.file)){
    system2("wget",
            c(files$path[i],"-P",dirname(dest.file)))
  }
}

# scp /home/femeunier/Documents/projects/CongoAS/scripts/Download.CMIP5.R hpc:/data/gent/vo/000/gvo00074/felicien/R/


# rm(list = ls())
#
# files <- c("~/Downloads/wget_script_2025-2-4_11-12-33.sh",
#            "~/Downloads/wget_script_2025-2-4_11-13-23.sh")
#
# files2down <- filenames <- c()
# for (cfile in files){
#
#   A <- readLines(cfile)
#   files2down_form <- A[grepl(pattern = "nbp_Lmon",A)]
#
#   files2down <- c(files2down,
#                   gsub("'","",sapply(strsplit(files2down_form," "),"[",2)))
#
# }
#
# filenames <- basename(files2down)
# Splitted <- strsplit(filenames,"_")
# models <- sapply(Splitted,"[",3) ; scenarios <- sapply(Splitted,"[",4) ; variants <- sapply(Splitted,"[",5)
# timing <- tools::file_path_sans_ext(sapply(Splitted,"[",6)) ; times_init <- sapply(strsplit(timing,"-"),"[",1) ; times_end <- sapply(strsplit(timing,"-"),"[",2)
#
# df <- data.frame(path = files2down,
#                  filename = filenames,
#                  model = models,
#                  scenario = scenarios,
#                  variant = variants,
#                  time_init = times_init,
#                  time_end = times_end) %>%
#   mutate(year_init = as.numeric(substr(time_init,1,4)),
#          year_end = as.numeric(substr(time_end,1,4)))
#
# df.selected <- df %>%
#   filter(year_init < 2100)
#
# unique(df.selected$model)
#
