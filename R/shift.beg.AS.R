shift.beg.AS <- function(AS,years,N){

  AS.mod <- AS
  pos <- which(AS)

  if (length(pos)>0){
    pos.init <- pos[1]
    pos2change <- unique(pmax(1,seq((pos.init - N),pos.init)))

    AS.mod[pos2change] <- TRUE
  }

  return(AS.mod)

}
