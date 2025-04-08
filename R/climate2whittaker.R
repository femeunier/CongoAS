climate2whittaker <- function(tmp,pre,extrapol = FALSE){

  input <- data.frame(
    id = 1:length(tmp),
    tmp,pre)

  input2 <- input %>%
     filter(!is.na(tmp),
           !is.na(pre)) %>%
    mutate(point.id = 1:length(tmp))

  input <- input %>%
    left_join(input2 %>%
                dplyr::select(id,point.id),
              by = "id")

  biome.id <- (plotbiomes::Whittaker_biomes) %>%
    dplyr::select(biome_id,biome) %>%
    distinct() %>%
    mutate(id = 1:9)
  polys <- geometry(plotbiomes::Whittaker_biomes_poly)

  sp <- SpatialPoints(input2[,c(2,3)])
  e <- as.data.frame(raster::extract(polys,sp)) %>%
    rename(point.id = id.y,
           id = id.x) %>%
    left_join(biome.id %>%
                dplyr::select(-biome_id),
              by = "id")

  if (extrapol){

    # transform to sf objects
    psf   <- sf::st_as_sf(sp) %>%
      dplyr::mutate(ID_point = 1:dim(.)[1])
    polsf <- sf::st_as_sf(plotbiomes::Whittaker_biomes_poly)

    in_points  <- lengths(sf::st_within(psf,polsf))
    out_points <- psf[in_points == 0, ]

    # find nearest poly
    nearest <- polsf[sf::st_nearest_feature(out_points, polsf) ,]  %>%
      dplyr::mutate(id_point = out_points$ID)

    e[["biome"]][nearest$id_point] <- nearest$biome

  }

  e.final <- input %>%
    left_join(e %>% dplyr::select(-id),
              by = "point.id")

  return(e.final[["biome"]])

}
