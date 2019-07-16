dmsTOdd <- function(data, type = 'lat') {
  # Function to transform data from fisheries logbook data
  # Data is in DDMMMM, needs to be transfored in degree decimals (Dd)
  # The formula is: Dd = DD + MM.MM/60
  # All data is either NAs or > 6 characters long (e.g. 634094) for logbook data

  dd <- as.numeric(substr(format(data, scientific = FALSE),1,2)) # degrees
  mm <- as.character(substr(format(data, scientific = FALSE),3,4)) # minutes

  if(nchar(format(data, scientific = FALSE)) == 6) {
      ss <- as.character(substr(format(data, scientific = FALSE),5,6)) # seconds
  }

  if(nchar(format(data, scientific = FALSE)) > 6) {
      ss <- as.character(substr(format(data, scientific = FALSE),5,nchar(format(data, scientific = FALSE)))) # seconds
      ss <- as.character(gsub('\\.','',ss))
  }

  if(nchar(format(data, scientific = FALSE)) < 6) {
      ss <- "00"
  }

  mmss <- as.numeric(paste(mm, '.', ss, sep = '')) / 60 # mm.ss
  Dd <- dd + mmss

  if(type == 'long') Dd <- -Dd
  return(Dd)
} # dmsTOdd
