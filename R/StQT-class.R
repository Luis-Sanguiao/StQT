#' S4 class of dataset transformations.
#'
#' Definition of an S4 class named \code{StQT} for data transformation.
#'
#' The structure of the class \code{StQT} comprises 2 attributes:
#' \itemize{
#' \item The attribute \code{Rules}, which is a \link{data.frame} with the transformation rules.
#' \item The attribute \code{Functions}, which is a list of the functions used in the rules.
#' }
#' The rules are represented with the following fields in the \code{Rules} data.frame.
#' \itemize{
#' \item{domain}{Specifies where the rule is applied}
#' \item{output}{Output variables}
#' \item{fun}{Function to apply}
#' \item{input}{Input variables}
#' \item{by}{Variables to group by}
#' \item{order}{Variables to order by}
#' \item{key}{Key of the output variables}
#' }
#'
#' @examples
#' T1 <- new(Class = 'StQT', Rules = data.frame(
#'                              domain="",
#'                              output="VAR",
#'                              fun="*",
#'                              input="SD,SD",
#'                              by="",
#'                              order="",
#'                              key="",
#'                              stringsAsFactors = FALSE),
#'                              Functions = list(`*`=`*`))
#' T1
#' getRules(T1)
#' getFunctions(T1)
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
             stop('[Validity StQT] Wrong rules set.')

           if (!all(unlist(lapply(object@Rules,is.character))))
             stop('[Validity StQT] Rules fields must be class character.')

           if (!all(unlist(lapply(object@Functions,is.function))))
             stop('[Validity StQT] Functions list contains non-function elements.')

           if (!setequal(RemoveInternal(object@Rules$fun),names(object@Functions)))
             if (length(setdiff(RemoveInternal(object@Rules$fun),names(object@Functions))) == 0)
               warning('[Validity StQT] Functions list contains unneeded functions.')
             else
               stop('[Validity StQT] Functions list does not contain all the functions in the rules.')

           return(TRUE)

         }
)
