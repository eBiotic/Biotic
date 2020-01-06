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
    # Function to transform data from northern gulf plurispecific surveys
    # Data is in DDMM.%%, needs to be transfored in degree decimals (Dd)
    # The formula is: Dd = DD + MM.%%/60

    dd <- as.numeric(substr(data,1,2)) # degrees
    mm <- as.numeric(substr(data,3,nchar(data))) / 60 # minutes

    Dd <- dd + mm

    if(type == 'long') {
        Dd <- -Dd
    }

    return(Dd)
} # dmsTOdd
