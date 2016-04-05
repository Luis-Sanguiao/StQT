#' S4 class of dataset transformations.
#'
#' Definition of an S4 class named \code{StQT} for dataset transformation.
#'
#' The structure of the class \code{StQT} comprises 2 attributes:
#' \itemize{
#' \item The attribute \code{Rules}, which is a \link{data.frame} with the transformation rules.
#' \item The attribute \code{Functions}, which is a list of the functions used in the rules.
#' }
#' The rules are represented with the following fields in the \code{Rules} data.frame.
#' \itemize{
#' \item{domain}{Specifies where the rule is applied}
#' \item{output}{variable(s) de salida}
#' \item{fun}{función a aplicar}
#' \item{input}{variable(s) de entrada}
#' \item{by}{agrupación}
#' \item{order}{variable(s) por las que ordenar los datos}
#' \item{key}{key of the output variables}
#' }
#'
#' @examples
#' x <- new(Class = 'StQT', Rules=data.frame(domain=c("y<3",""),output=c("y=4","exponencial"),fun=c("sum","exp"),input=c("v","v"),by=c("x",""),stringsAsFactors=FALSE))
#'
#'
#'
#' @export
setClass(Class = "StQT",
         slots = c(Rules = 'data.frame', Functions = 'list'),
         prototype = list(Rules = data.frame(domain = character(0),
                                             output = character(0),
                                             fun    = character(0),
                                             input  = character(0),
                                             by     = character(0),
                                             order  = character(0),
                                             key    = character(0),
                                             stringsAsFactors = FALSE),
                          Functions = list()),
         validity = function(object){

           if (!setequal((names(object@Rules)),c("domain","output","fun","input","by","key","order")))
             stop('[Validity StQT] Conjunto de reglas incorrecto.')

           if (!all(unlist(lapply(object@Rules,is.character))))
             stop('[Validity StQT] Las reglas deben definirse como variables character.')

           if (!all(unlist(lapply(object@Functions,is.function))))
             stop('[Validity StQT] La lista Functions tiene elementos que no son functiones.')

           if (!setequal(RemoveInternal(object@Rules$fun),names(object@Functions)))
             if (length(setdiff(RemoveInternal(object@Rules$fun),names(object@Functions))) == 0)
               warning('[Validity StQT] En la lista Functions figuran funciones innecesarias.')
             else
               stop('[Validity StQT] Alguna de las funciones no figura en la lista Functions')

           return(TRUE)

         }
)
