CMIP6name2attributes <- function(CMIP6name){

  # CMIP6name = "cVeg_Lmon_CanESM5_1pctCO2_r2i1p1f1_gn_185001-200012.nc"
  CMIP6name.noext <- tools::file_path_sans_ext(basename(CMIP6name))

  all.attributes <- strsplit(CMIP6name.noext,split = "_")
  attributes <- purrr::map_dfr(1:length(all.attributes),
                              function(i){
                                print(i)
                                data.frame(var = all.attributes[[i]][1],
                                           timestep = all.attributes[[i]][2],
                                           model = all.attributes[[i]][3],
                                           scenario = all.attributes[[i]][4],
                                           variant = all.attributes[[i]][5],
                                           grid = all.attributes[[i]][6],
                                           init.year = as.numeric(substr(all.attributes[[i]][7],1,4)),
                                           init.month = as.numeric(substr(all.attributes[[i]][7],5,6)),
                                           end.year = as.numeric(substr(all.attributes[[i]][7],8,11)),
                                           end.month = as.numeric(substr(all.attributes[[i]][7],12,13)))})







  return(attributes)

}
