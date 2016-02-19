#' Extrae partes de un objeto de clase StQT.
#'
#' \code{[} extrae o reemplaza partes de un objeto de clase \linkS4class{StQT}.
#'
#' Se trata del método \code{[} para la clase \linkS4class{StQT}. Este método
#' obtiene subconjuntos del objeto de clase \code{StQT} especificado como entrada.
#'
#' @param x objeto de clase \linkS4class{StQT} del que se van a extraer los
#' elementos.
#'
#' @param i índices correspondientes a los elementos a extraer.
#'
#' @param drop Incluido por coherencia.
#'
#' @return Objeto de clase \linkS4class{StQT}, que consiste en un subconjunto del
#'  objeto \code{StQT} de entrada.
#'
#' @examples
#'
#'
#' @include StQT-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
  f = "[",
  signature = c("StQT"),
  function(x, i, ..., drop = TRUE){

    rules<-getRules(x)
    funcs<-getFunctions(x)
    rules<-rules[(1:nrow(rules))[i],]
    output<-new(Class = 'StQT', Rules = rules, Functions = funcs[RemoveInternal(rules$fun)])
    return(output)

  }
)
