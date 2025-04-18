continent2coord <- function(continent){

  if (continent == "America"){
    min.lon.analysis <- -90
    max.lon.analysis <- -30
    min.lat.analysis <- -40
    max.lat.analysis <- 20

    min.lon.plot <- min.lon.analysis
    max.lon.plot <- max.lon.analysis
    min.lat.plot <- min.lat.analysis
    max.lat.plot <- max.lat.analysis

  } else if (continent == "Temperate"){

    min.lon.analysis <- -180
    max.lon.analysis <- 180
    min.lat.analysis <- 20
    max.lat.analysis <- 70

    min.lon.plot <- min.lon.analysis
    max.lon.plot <- max.lon.analysis
    min.lat.plot <- min.lat.analysis
    max.lat.plot <- max.lat.analysis

  } else if (continent == "Africa"){
    min.lon.analysis <- -10
    max.lon.analysis <- 45
    min.lat.analysis <- -15
    max.lat.analysis <- 10

    min.lon.plot <- -10
    max.lon.plot <- 45
    min.lat.plot <- -15
    max.lat.plot <- 10

  } else if (continent == "Equator"){
    min.lon.analysis <- -90
    max.lon.analysis <- 150
    min.lat.analysis <- -5
    max.lat.analysis <- 5

    min.lon.plot <- min.lon.analysis
    max.lon.plot <- max.lon.analysis
    min.lat.plot <- min.lat.analysis
    max.lat.plot <- max.lat.analysis

  } else if (continent == "Tropics"){
    min.lon.analysis <- -180
    max.lon.analysis <- 180
    min.lat.analysis <- -23
    max.lat.analysis <- 23

    min.lon.plot <- min.lon.analysis
    max.lon.plot <- max.lon.analysis
    min.lat.plot <- min.lat.analysis
    max.lat.plot <- max.lat.analysis

  } else if (continent == "World"){

    min.lon.analysis <- -180
    max.lon.analysis <- 180
    min.lat.analysis <- -90
    max.lat.analysis <- 90

    min.lon.plot <- min.lon.analysis
    max.lon.plot <- max.lon.analysis
    min.lat.plot <- min.lat.analysis
    max.lat.plot <- max.lat.analysis

  } else if (continent == "NSA" ){

    min.lon.analysis <- -70
    max.lon.analysis <- -50
    min.lat.analysis <- -10
    max.lat.analysis <- 10

    min.lon.plot <- min.lon.analysis
    max.lon.plot <- max.lon.analysis
    min.lat.plot <- min.lat.analysis
    max.lat.plot <- max.lat.analysis

  } else{
    stop()
  }

  return(list(coord.analysis = c(min.lon.analysis,max.lon.analysis,min.lat.analysis,max.lat.analysis),
              coord.plot = c(min.lon.plot,max.lon.plot,min.lat.plot,max.lat.plot)))
}
