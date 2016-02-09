#' Devuelve el slot Functions de un objeto de clase StQT.
#'
#' \code{getFunctions} devuelve el slot \code{Functions} del input
#' \code{\linkS4class{StQT}}.
#'
#' @param object Objeto de clase \code{\linkS4class{StQT}} del que se desea
#' extraer las funciones.
#'
#' @return Una \code{\link{list}} con las funciones de transformación.
#'
#' @examples
#'
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
