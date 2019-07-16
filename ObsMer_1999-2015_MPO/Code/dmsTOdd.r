#' @name
#'
#' @title
#'
#' @description
#'
#' @param
#'
#' @return
#'
#' @author
#' David Beauchesne
#'
#' @references
#'
#' @importFrom
#'
#' @example
#'
#' @rdname
#'
#' @export

dmsTOdd <- function(data, type = 'lat') {
  # Function to transform data from sea observers
  # Data is in DDMMMM, needs to be transfored in degree decimals (Dd)
  # The formula is: Dd = DD + MM.MM/60
  # All data is either NAs or 6 characters long (e.g. 634094) for sea observers

  dd <- as.numeric(substr(data,1,2)) # degrees
  mm <- as.character(substr(data,3,4)) # minutes
  ss <- as.character(substr(data,5,6)) # seconds
  mmss <- as.numeric(paste(mm, '.', ss, sep = '')) / 60 # mm.ss

  Dd <- dd + mmss

  if(type == 'long') {
      Dd <- -Dd
  }

  return(Dd)
} # dmsTOdd
