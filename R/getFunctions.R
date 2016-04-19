#' Returns slot \code{Functions} from an object of class \linkS4class{StQT}
#'
#' \code{getFunctions} extracts slot \code{Functions} from an \code{\linkS4class{StQT}} object.
#'
#' @param object Object of class \code{\linkS4class{StQT}}
#'
#' @return A \code{\link{list}} corresponding to the slot \code{Functions} of the input parameter.
#'
#' @examples
#' rules <- data.frame(domain = c('!is.na(Variable)','is.na(Variable)',''),
#'                     output = c('MediaVariable','Variable','MediaVariable'),
#'                     fun =    c('mean','identity','FunDelVar'),
#'                     input =  c('Variable','MediaVariable',''),
#'                     by =     c('Estrato','',''),
#'                     key =    c('Estrato','',''),
#'                     stringsAsFactors = FALSE)
#' T1 <- NewStQT(rules)
#' T1
#' getFunctions(T1)
#'
#' @include StQT-class.R
#'
#' @export
setGeneric("getFunctions", function(object){standardGeneric("getFunctions")})

#' @rdname getFunctions
#'
#' @include StQT-class.R
#'
#'
#' @export
setMethod(
  f = "getFunctions",
  signature = c("StQT"),
  function(object){ object@Functions }
)
