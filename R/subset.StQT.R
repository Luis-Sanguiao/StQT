#' Extracts parts of an StQT object.
#'
#' \code{[} extracts parts of an object of class \linkS4class{StQT}.
#'
#' It is indeed the method \code{[} for the class \linkS4class{StQT}. This method
#' returns an object of class \linkS4class{StQT} whose rules are the specified subset
#' of the original rules.
#'
#' @param x object of class \linkS4class{StQT} from which to extract element(s).
#'
#' @param i indices specifying elements to extract.
#'
#' @param drop Included by coherence.
#'
#' @return Object of class \linkS4class{StQT}, with the subset of rules.
#'
#' @examples
#' rules <- data.frame(domain = c('!is.na(Variable)','is.na(Variable)',''),
#'                     output = c('MediaVariable','Variable','MediaVariable'),
#'                     fun =    c('mean','identity','FunDelVar'),
#'                     input =  c('Variable','MediaVariable',''),
#'                     by =     c('Estrato','',''),
#'                     key =    c('Estrato','',''),
#'                     stringsAsFactors = FALSE)
#' imputa_media <- NewStQT(rules)
#' imputa_media
#' imputa_media[1:2]
#' imputa_media[3:1]
#'
#' @include StQT-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
  f = "[",
  signature = c(x = "StQT"),
  function(x, i, j, ..., drop=FALSE){

    if (nargs() > 2) stop("[StQT::subset.StQT] Subsetting only admits one argument.")
    rules <- getRules(x)
    funcs <- getFunctions(x)
    index <- (1:nrow(rules))[i]
    if (is.na(sum(index))) stop("[StQT::subset.StQT] Subsetting out of bounds.")
    rules <- rules[index,]
    output <- new(Class = 'StQT', Rules = rules, Functions = funcs[RemoveInternal(rules$fun)])
    return(output)

  }
)
