#' @title Method \code{input} for the class \linkS4class{StQT}
#'
#' @description \code{input} gets the input variables needed for a StQT object to work.
#'
#' @param x Object of class \code{StQT}.
#'
#' @return Character vector with the variable names
#'
#' @examples
#' x <- 1
#' @export
setGeneric("input", function(x){standardGeneric("input")})
#'
#' @rdname input
#' @include StQT-class.R getRules.R Utils.R length.StQT.R subset.StQT.R
#'
#' @export
setMethod(
  f = "input",
  signature = c("StQT"),
  definition = function(x){
    if (length(x) == 0) return(character(0))

    rules <- getRules(x)
    output <- unique(c(expand(rules$domain[1]),expand(rules$input[1]),
                       expand(rules$by[1]),expand(rules$order[1])))
    if (length(x) == 1) return(output)
    else {
      input_1 <- input(x[2:length(x)])
      temp <- ssplit(rules$output[1])
      input_1 <- setdiff(input_1,temp)
      return(union(output,input_1))
    }

  }
)
