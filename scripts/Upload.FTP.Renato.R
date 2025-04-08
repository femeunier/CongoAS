rm(list = ls())

library(RCurl)

# ml lftp/4.9.2-GCCcore-11.2.0

# sshfs anonymous@ftp.gps.caltech.edu:/pub/renatob /data/gent/vo/000/gvo00074/felicien/R/data/renato
# Password: gcb-2020

dir <- "./outputs/"
files <- c(list.files(dir,
                      pattern = "CMIP6.monthly.nbp.global.*.RDS$",
                      full.names = TRUE))

# FTP settings
login<-"anonymous"
secret<-"felicien.meunier@gmail.com"
uploadsite<-"ftp://ftp.gps.caltech.edu/pub/renatob/CMIP6"

for (ifile in seq(1,length(files))){
  cfile <- files[ifile]

  ftpUpload(cfile,
            paste(uploadsite,"/",basename(cfile),sep=""),
            userpwd=paste(login,secret,sep=":"))

  stop()
}
