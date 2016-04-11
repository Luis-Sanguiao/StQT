#' Output variables from a \linkS4class{StQT} object.
#'
#' \code{output} gets the output variables from a StQT object.
#'
#' @param x Object of class \code{StQT}.
#'
#' @return Character vector with the variable names
#'
#' @examples
#' x <- 1
#' @export
setGeneric("output", function(x){standardGeneric("output")})

#' @rdname output
#' @include StQT-class.R getRules.R Utils.R length.StQT.R subset.StQT.R
#'
#' @export
setMethod(
  f = "output",
  signature = c("StQT"),
  definition = function(x){
    if (length(x) == 0) return(character(0))

    rules <- getRules(x)
    out <- ssplit(rules$output[length(x)])

    if (length(x) == 1)
      if (rules$fun[1] != "FunDelVar") return(out)
      else return(character(0))
    else {
      output_n <- output(x[1:(length(x) - 1)])
      if (rules$fun[length(x)] != "FunDelVar")
        return(union(output_n,out))
      else
        return(setdiff(output_n,out))
    }

  }
)
