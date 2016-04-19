#' Returns slot \code{Rules} from an object of class \linkS4class{StQT}
#'
#' \code{getRules} extracts slot \code{Rules} from an \code{\linkS4class{StQT}} object.
#'
#' @param object Object of class \code{\linkS4class{StQT}}
#'
#' @return A \code{\link{data.frame}} corresponding to the slot \code{Rules} of the input parameter.
#'
#' @examples
#' T1 <- NewStQT(data.frame(
#'                 domain = c("Next==Unit","","Value < 0"),
#'                 output = c("Value_next","Value_next",""),
#'                    fun = c("FunAutoLink","FunDelVar","FunDelRow"),
#'                  input = c("Value","",""),
#'                          stringsAsFactors = FALSE))
#' T1
#' getRules(T1)
#'
#'
#' @include StQT-class.R
#'
#' @export
setGeneric("getRules", function(object){standardGeneric("getRules")})

#' @rdname getRules
#'
#' @include StQT-class.R
#'
#'
#' @export
setMethod(
  f = "getRules",
  signature = c("StQT"),
  function(object){ object@Rules }
)
