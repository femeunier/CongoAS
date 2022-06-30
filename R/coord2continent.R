coord2continent <- function(lon){

  continent = rep("Asia",length(lon))
  continent[lon >= -90 & lon <= -30] <- "America"
  continent[lon >= -20 & lon <= 55] <- "Africa"

  continent <- factor(continent,levels = c("America","Africa","Asia"))

  return(continent)

}
