#' Constructor de objetos de clase StQT
#'
#' Este constructor devuelve un objeto de clase
#' \code{\linkS4class{StQT}}
#'
#'
#' @return Objeto de clase \linkS4class{StQT}.
#'
#' @examples
#' x
#'
#' @include StQT-class.R
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

    x[is.na(x)] <- ""

    if (!all(unlist(lapply(RemoveInternal(x$fun),exists))))
      stop('[StQT Constructor] Una de las funciones no está definida.')
    if (!all(unlist(lapply(mget(RemoveInternal(x$fun),inherits = TRUE),is.function))))
      stop('[StQT Constructor] Una de las funciones está definida como otro tipo de objeto.')

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
