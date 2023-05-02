mask <- function(cdf,mask.ocean){
  return(cdf %>% left_join(mask.ocean,
                           by = c("lat","lon")) %>%
           filter(is.land) %>%
           dplyr::select(-is.land))
}
