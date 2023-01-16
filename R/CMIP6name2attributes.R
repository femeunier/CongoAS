CMIP6name2attributes <- function(CMIP6name){

  # CMIP6name = "cVeg_Lmon_CanESM5_1pctCO2_r2i1p1f1_gn_185001-200012.nc"
  CMIP6name.noext <- tools::file_path_sans_ext(basename(CMIP6name))

  all.attributes <- strsplit(CMIP6name.noext,split = "_")[[1]]
  attributes <- list(var = all.attributes[1],
                     timestep = all.attributes[2],
                     model = all.attributes[3],
                     scenario = all.attributes[4],
                     variant = all.attributes[5],
                     grid = all.attributes[6],
                     init.year = as.numeric(substr(all.attributes[7],1,4)),
                     init.month = as.numeric(substr(all.attributes[7],5,6)),
                     end.year = as.numeric(substr(all.attributes[7],8,11)),
                     end.month = as.numeric(substr(all.attributes[7],12,13)))


  return(attributes)

}
