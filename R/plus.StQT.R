#' @title Method \code{+} for the class \linkS4class{StQT}
#'
#' @description \code{+} joins two objects of class \linkS4class{StQT} in a
#' single object of the same class.
#'
#' This method overloads the operator \code{\link{+}} and builds a new
#' \linkS4class{StQT} object joining both input objects.
#'
#' @param e1 Object of class \code{StQT}.
#'
#' @param e2 Object of class \code{StQT}.
#'
#' @return Object of class \code{StQT} that binds e2 rules after e1 rules.
#'
#' @examples
#' x <- 1
#'
#' @include StQT-class.R
#'
#' @import data.table
#'
#' @export
setMethod(
    f = "+",
    signature = c("StQT", "StQT"),
    definition = function(e1, e2){
    funs1 <- getFunctions(e1)
    funs2 <- getFunctions(e2)
    rules1 <- getRules(e1)
    rules2 <- getRules(e2)

    comparefun <- unlist(lapply(funs1,function(x) lapply(funs2,function(y) identical(x,y))))
    if (is.null(comparefun)) {
      if (nrow(rules1) == 0) return(e2)
      else return(e1)
    }
    comparefun <- matrix(comparefun, ncol = length(funs1))
    selected2 <- funs2[apply(!comparefun,1,all)]
    names2 <- make.unique(c(names(funs1),names(selected2)))[-1:-length(funs1)]
    for (i in 1:length(names2)) rules2$fun[rules2$fun == names(selected2)[i]] <- names2[i]
    names(selected2) <- names2
    for (i in length(rules2$fun)) {
      vlogic <- comparefun[which.max(names(funs2) == rules2$fun[i]),]
      if (any(vlogic)) rules2$fun[i] <- names(funs1)[which.max(vlogic)]
    }

    output.rules <- rbindlist(list(rules1,rules2))
    output.functions <- c(funs1,selected2)

    output <- new(Class = 'StQT', Rules = output.rules, Functions = output.functions)

    return(output)

    }
)
