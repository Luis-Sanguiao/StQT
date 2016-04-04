#' @title Method \code{length} for the class \linkS4class{StQT}
#'
#' @description \code{length} gets the number of rules of a StQT object.
#'
#' This method overloads the method \code{\link{length}}.
#'
#' @param x Object of class \code{StQT}.
#'
#' @return Number of rules of a StQT object.
#'
#' @examples
#' x <- 1
#'
#' @include StQT-class.R getRules.R
#'
#' @import data.table
#'
#' @export
setMethod(
  f = "length",
  signature = c("StQT"),
  definition = function(x){
    return(nrow(getRules(x)))
  }
)
