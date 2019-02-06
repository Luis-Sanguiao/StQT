#' Change variable names from a \linkS4class{StQT} object.
#'
#' \code{rename} changes the names of the variables involved in a StQT object, which
#' is useful if the same transformation pattern has to be applied to different data.
#'
#' @param x Object of class \code{StQT}.
#' @param oldnames String vector with variable names to change
#' @param oldnames String vector with the new variable names
#'
#' @return Character vector with the variable names
#'
#' @examples
#' rules <- data.frame(domain = c('','Turnover > 0'),
#'                     output = c('AggTurnover','LogTurnover'),
#'                        fun = c('sum','log'),
#'                      input = c('Turnover','Turnover'),
#'                         by = c('NACE09',''),
#'                        key = c('NACE09',''),
#'                          stringsAsFactors = FALSE)
#' T2 <- NewStQT(rules)
#' T2
#' T2 <- rename(T2,input(T2),toupper(input(T2)))
#' T2
#'
#' @include StQT-class.R
#'
#' @export
setGeneric("rename", function(x, oldnames, newnames){standardGeneric("rename")})

#' @rdname rename
#' @include StQT-class.R getRules.R getFunctions.R length.StQT.R
#'
#'
#' @export
setMethod(
  f = "rename",
  signature = c("StQT", "character", "character"),
  definition = function(x, oldnames, newnames){
    if (length(x) == 0) return(x)

    if (length(oldnames) != length(newnames))
      stop(paste0("[StQT::rename.StQT] The number of old names does ",
        "not match the number of new names."))

    rules <- getRules(x)
    rules <- rules[,setdiff(colnames(rules),"fun")]

    for (i in 1:length(oldnames))
      rules <- as.data.frame(lapply(rules, function(x) gsub(
        paste0("(^|[^[:alpha:]])", oldnames[i], "([^[:alnum:]]|$)"),
        paste0("\\1",newnames[i],"\\2"), x)), stringsAsFactors = FALSE)

    rules[,"fun"] <- getRules(x)[,"fun"]
    output <- new(Class = 'StQT', Rules = rules, Functions = getFunctions(x))
    return(output)
  }
)
