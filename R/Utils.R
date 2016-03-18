# Removes internal functions from a character vector with function names
RemoveInternal <- function(x)
{
  return(setdiff(x,c("FunDelRow","FunDelCol","FunAutoLink")))
}

# expand splits comma separated variables from a string into a character vector
expand <- function(string)
{
  string <- all.vars(parse(text = paste0("list(",string,")"))[[1]])
  return(string)
}

# ssplit extracts left hand side and right hand side elements
ssplit <- function(string,left=TRUE)
{
  expr <- parse(text = paste0("list(",string,")"))[[1]]
  output <- names(expr)[-1]
  if (all(output != "") & (length(all.vars(expr)) == length(output))) {
    if (left) return(output)
    else return(all.vars(expr))
  }
  if (left)
  {

    noequal <- all.vars(expr)
    if (!is.null(output))
      output[output == ""] <- noequal
    else output <- noequal
    return(output)
  }
  else
  {
    output <- unname(lapply(expr,function(x) if (is.atomic(x)) x else NA))[-1]
  }
}

# getQuals returns colnames Qual* from a data.table
getQuals <- function(object)
{
  onames <- names(object)
  return(onames[grep("Qual",onames)])
}

# getFormula returns the formula for dcast
getFormula <- function(quals)
{
  return(as.formula(paste(paste0("`",paste(quals,collapse = "`+`"),"`"),"IDDD", sep = "~")))
}

# getVars returns a list with the vars in a transformation grouped by qualifiers
getVars <- function(rules,DD)
{
  microdata <- getData(DD)
  varset <- unique(unlist(lapply(as.list(unname(unlist(rules))),ssplit,left = TRUE)))
  varset <- intersect(varset,unlist(microdata[Sort == "IDDD",Variable]))
  output <- list(varset[1])
  for (variable in varset[-1]) {
    vlogic <- unlist(lapply(lapply(output,`[`,i = 1),
                function(x) setequal(unlist(microdata[Variable == x,getQuals(microdata), with = FALSE]),
                  unlist(microdata[Variable == variable,getQuals(microdata), with = FALSE]))))
    if (!any(vlogic)) output <- c(output,variable)
    else output[[which.max(vlogic)]] <- c(output[[which.max(vlogic)]],variable)
  }

  return(output)
}

# setOrderVars puts main variable in first place
setOrderVars <- function(vars,DTList,DD)
{
  microdata <- getData(DD)

  DTListQuals <- lapply(DTList,key)
  varsQuals <- lapply(lapply(vars,`[`,i = 1), function(x) {
    output <- unlist(microdata[Variable == x, getQuals(microdata),with = FALSE])
    output <- output[output != ""]
    return(output)
  })
  allQuals <- unique(unname(unlist(varsQuals)))
  vlogic <- unlist(lapply(varsQuals,function(x) setequal(intersect(x,allQuals),allQuals)))
  if (any(vlogic))
    return(c(vars[which.max(vlogic)],vars[-which.max(vlogic)]))
  else return(NULL)
}


# getDataTableList returns a list of data.tables from a StQ and a list of variables grouped by qualifier

getDataTableList <-  function(object, vars)
{

  # Get Slots from objects
  DD <- getDD(object)
  microdata <- getData(DD)
  Data <- getData(object)

  output <- lapply(vars,function(newvars) {
    quals <- unlist(microdata[Variable == newvars[1],getQuals(microdata), with = FALSE])
    quals <- quals[quals != ""]
    subtable <- dcast(Data, getFormula(quals), subset = .(IDDD %in% newvars), value.var = "Value")
    subtable[,colnames(subtable) :=
        lapply(as.list(colnames(subtable)), function(x) as(get(x),unlist(microdata[Variable == x,"Class",with = FALSE]))),
        with = FALSE]
    setkeyv(subtable,unname(quals))
    return(subtable)
    })

  return(output)
}

# mergeDataTable creates the data.table where we will apply the rule from DTList and vars
mergeDataTable <- function(DTList,vars,DD) {

  #get Slots from objects
  microdata <- getData(DD)

  newdata <- lapply(vars,function(x) {
    vlogic <- unlist(lapply(DTList, function(y) x[1] %in% colnames(y)))
    output <- DTList[[which.max(vlogic)]][,c(key(DTList[[which.max(vlogic)]]),x),with = FALSE]
    return(output)
  })

  Reduce(merge,newdata)

}

DDadd <- function(variables,DD,DT) {

  #get Slots from objects
  microdata <- getData(DD)

  variables <- setdiff(variables,microdata$Variable)
  if (length(variables) == 0) return(DD)

  incDD <- do.call(data.table,c(list(variables, ifelse(variables %in% key(DT),"NonIDQual","IDDD"),
                unname(unlist(lapply(DT[,variables,with = FALSE],class)))),
                unname(split(unlist(lapply(variables %in% key(DT),
                  function(x) if (x) return(rep("",length(key(DT)))) else return(key(DT)))),
                rep(1:length(key(DT)),length(variables))))))
  colnames(incDD) <- c("Variable","Sort","Class",paste0("Qual",1:(ncol(incDD) - 3)))

  microdata <- rbindlist(list(microdata,incDD),fill = TRUE)
  microdata <- as.data.table(lapply(microdata,function(x) {
    x[is.na(x)] <- ""
    return(x)
  }))

  return(new(Class = "DD", VarNameCorresp = getVNC(DD), MicroData = microdata, Aggregates = getAggr(DD)))
}


# combine substitutes x with y for not NA elements
combine <- function(x,y) {
    x[!is.na(y)] <- y[!is.na(y)]
  return(x)
}

# sidelink extracts left/right sides of a link a==b & c==d ...
sidelink <- function(x,left) {
  if (identical(x[[1]],quote(`&`))) return(c(sidelink(x[[2]],left),sidelink(x[[3]],left)))
  if (identical(x[[1]],quote(`==`))) if (left) return(as.character(x[[2]])) else return(as.character(x[[3]]))
}
