#' Show an object of class \linkS4class{StQT}
#'
#' \code{show} displays the slot \code{Rules} of the input \linkS4class{StQT}
#' object. For too wide columns the values are cut and ... is displayed.
#'
#' This method displays only the content of slot \code{Rules} from the input
#' \linkS4class{StQT} object. It is indeed the method \link[methods]{show} adapted
#' to class \linkS4class{StQT}.
#'
#' @param object Object of class \linkS4class{StQT}.
#'
#' @return Invisible \code{\link{NULL}}.
#'
#' @examples
#' # A trivial example
#' show(new(Class = 'StQT'))
#'
#' @include StQT-class.R getRules.R Utils.R
#'
#' @export
setMethod(
  f = "show",
  signature = c("StQT"),
  function(object){

    tab <- getRules(object)
    width <- max(unlist(lapply(tab,nchar)),colnames(tab))
    if (width > 23) {
      width <- 23
      fcut <- function(x) {
        x[nchar(x) > 23] <- paste0(substr(x[nchar(x) > 23],1,20),"...")
        return(x)
      }
      tab <- as.data.frame(lapply(tab,fcut))
    }
    colnames(tab) <- fillSpaces(toupper(colnames(tab)),width)
    show(format(tab,width = width,justify = "centre"))

    invisible(NULL)
  }
)

