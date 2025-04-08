rm(list = ls())

A <- readRDS("~/Downloads/CMIP6.monthly.nbp.global.historical.GISS-E2-2-G.r1i1p1f1.RDS")
whist(A$nbp)
unique(A$year)

library(ncdf4)

nc <- nc_open("~/Downloads/nbp_Lmon_GISS-E2-2-G_historical_r1i1p1f1_gn_199101-201412.nc")
A <- ncvar_get(nc,"nbp")
hist(as.vector(A))
