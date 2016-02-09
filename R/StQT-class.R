#' Clase S4 de transformaciones de un conjunto de datos
#'
#' Definición de una clase S4 \code{StQT} de transformaciones de un conjunto de datos
#'
#' La estructura de la clase S4 \code{StQT} se compone del slot \code{Rules}
#' que es un data.frame que contiene las reglas de transformación según el modelo
#' \describe{
#' \item{domain}{dónde se aplica la regla}
#' \item{output}{variable(s) de salida}
#' \item{fun}{función a aplicar}
#' \item{input}{variable(s) de entrada}
#' \item{by}{agrupación}
#' \item{key}{variable(s) por las que ordenar los datos}
#' } y del slot \code{Functions} que es una lista con todas las funciones utilizadas en dichas reglas.
#'
#' @examples
#' x <- new(Class = 'StQT', Rules=data.frame(domain=c("y<3",""),output=c("y=4","exponencial"),fun=c("sum","exp"),input=c("v","v"),by=c("x",""),stringsAsFactors=FALSE))
#'
#'
#'
#' @export
setClass(Class = "StQT",
         slots = c(Rules = 'data.frame', Functions = 'list'),
         validity = function(object){

           if (!setequal((names(object@Rules)),c("domain","output","fun","input","by","key"))) stop('[Validity StQT] Conjunto de reglas incorrecto.')
           if (!all(unlist(lapply(object@Rules,is.character)))) stop('[Validity StQT] Las reglas deben definirse como variables character.')
           if (!all(unlist(lapply(object@Functions,is.function)))) stop('[Validity StQT] La lista Functions tiene elementos que no son functiones.')
           if (!setequal(setdiff(object@Rules$fun,c("FunDelRow","FunDelCol","FunAutoLink")),names(object@Functions)))
             if (length(setdiff(setdiff(object@Rules$fun,c("FunDelRow","FunDelCol","FunAutoLink")),names(object@Functions)))==0)
               warning('[Validity StQT] En la lista Functions figuran funciones innecesarias.')
             else
               stop('[Validity StQT] Alguna de las funciones no figura en la lista Functions')

           return(TRUE)

         }
)