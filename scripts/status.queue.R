a <- as.list(system2("qstat","-a",stdout = TRUE))
pos <- 4:(length(a)-2)
status <- data.frame(status = sapply(sapply((lapply(a[pos],strsplit,split = "\\s+")),'[',1),"[",9))
length(which(status$status == "R"))
length(which(status$status == "Q"))

