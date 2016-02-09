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
  signature = c("data.frame"),
  definition = function(x){

    x[is.na(x)]<-""

    if (!all(unlist(lapply(setdiff(x$fun,c("FunDelRow","FunDelCol","FunAutoLink")),exists))))
      stop('[StQT Constructor] Una de las funciones no está definida.')
    if (!all(unlist(lapply(mget(setdiff(x$fun,c("FunDelRow","FunDelCol","FunAutoLink")),inherits=TRUE),is.function))))
      stop('[StQT Constructor] Una de las funciones está definida como otro tipo de objeto.')

    funcs<-mget(setdiff(x$fun,c("FunDelRow","FunDelCol","FunAutoLink")),inherits=TRUE)
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

    return(NewStQT(read.csv2(file=x,stringsAsFactors=FALSE)))
  }

)
