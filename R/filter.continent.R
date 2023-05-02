filter.continent <- function(df,continent = "World",type = "analysis"){

  coord.list <- continent2coord(continent)

  if (type == "analysis"){
    min.lon <- coord.list[[1]][1]; max.lon <- coord.list[[1]][2]; min.lat <- coord.list[[1]][3]; max.lat <- coord.list[[1]][4]
  } else{
    min.lon <- coord.list[[2]][1]; max.lon <- coord.list[[2]][2]; min.lat <- coord.list[[2]][3]; max.lat <- coord.list[[2]][4]
  }

  return(df %>% filter(lon  >= min.lon, lon <= max.lon,
                       lat >= min.lat, lat <= max.lat))
}
