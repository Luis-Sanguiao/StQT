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
#' @details Operator \code{+} is non-commutative for \linkS4class{StQT} objects. It is defined to
#' fulfill the equality: Tapply(Data,e1+e2) == Tapply(Tapply(Data,e1),e2)
#'
#' @examples
#' x <- 1
#'
#' @include StQT-class.R getRules.R getFunctions.R
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
      output.rules <- rbindlist(list(rules1,rules2))
      if (length(funs1) == 0) output.functions <- funs2
      else output.functions <- funs1
      output <- new(Class = 'StQT', Rules = output.rules, Functions = output.functions)

      return(output)
    }

    comparefun <- matrix(comparefun, ncol = length(funs1))
    selected2 <- funs2[apply(!comparefun,1,all)]
    names2 <- make.unique(c(names(funs1),names(selected2)))[-1:-length(funs1)]
    for (i in 1:length(names2)) rules2$fun[rules2$fun == names(selected2)[i]] <- names2[i]
    names(selected2) <- names2
    for (i in 1:length(rules2$fun)) {
      if (!length(RemoveInternal(rules2$fun[i]))) next
      vlogic <- comparefun[which.max(names(funs2) == rules2$fun[i]),]
      if (any(vlogic)) rules2$fun[i] <- names(funs1)[which.max(vlogic)]
    }

    output.rules <- rbindlist(list(rules1,rules2))
    output.functions <- c(funs1,selected2)

    output <- new(Class = 'StQT', Rules = output.rules, Functions = output.functions)

    return(output)

    }
)
