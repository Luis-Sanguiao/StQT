#' Devuelve el slot Rules de un objeto de clase StQT.
#'
#' \code{getRules} devuelve el slot \code{Rules} del input
#' \code{\linkS4class{StQT}}.
#'
#' @param object Objeto de clase \code{\linkS4class{StQT}} del que se desea
#' extraer las reglas.
#'
#' @return Un \code{\link{data.frame}} con las reglas de la transformación.
#'
#' @examples
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
