#' StQT class constructor
#'
#' This constructor returns an \code{\linkS4class{StQT}} object
#'
#' @return \linkS4class{StQT} object.
#'
#' @details You don't need to specify all rules fields, the constructor will
#' generate empty fields when necessary.
#'
#' @examples
#' NewStQT()    # Empty transformation (identity)
#' NewStQT(data.frame(
#'            output="VAR",
#'            fun="*",
#'            input="SD,SD",
#'            stringsAsFactors = FALSE))
#' # If we want to read the rules from a semicolon separated csv file:
#' # NewStQT("<filename>")
#'
#' @include StQT-class.R
#'
#' @import methods
#'
#' @export
setGeneric("NewStQT", function(x){standardGeneric("NewStQT")})

#' @rdname NewStQT
#'
#' @include StQT-class.R
#'
#'
#' @export
setMethod(
  f = "NewStQT",
  signature = c("missing"),
  definition = function(){
    return(new(Class = 'StQT'))
  }
)
#' @rdname NewStQT
#'
#' @include StQT-class.R Utils.R
#'
#'
#' @export
setMethod(
  f = "NewStQT",
  signature = c("data.frame"),
  definition = function(x){

    miscols <- setdiff(colnames(NewStQT()@Rules),colnames(x))
    if (length(miscols))
      x <- cbind(x,setNames(as.list(rep("",length(miscols))),miscols),stringsAsFactors = FALSE)
    x[is.na(x)] <- ""

    if (!all(unlist(lapply(RemoveInternal(x$fun),exists))))
      stop('[StQT Constructor] There are one or more undefined functions')
    if (!all(unlist(lapply(mget(RemoveInternal(x$fun),inherits = TRUE),is.function))))
      stop('[StQT Constructor] One or more functions are another kind of object than functions.')

    funcs <- mget(RemoveInternal(x$fun),inherits = TRUE)
    output <- new(Class = 'StQT', Rules = x, Functions = funcs)
    return(output)

    }

)

#' @rdname NewStQT
#'
#' @include StQT-class.R
#'
#'
#' @export
setMethod(
  f = "NewStQT",
  signature = c("character"),
  definition = function(x){
    return(NewStQT(read.csv2(file = x, stringsAsFactors = FALSE)))
  }

)
