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

  if (left)
  {
    noequal <- all.vars(expr)
    if (!is.null(output))
      output[output == ""] <- noequal
    else output <- noequal
  }
  else
  {
    output <- unname(lapply(expr,function(x) if (is.atomic(x)) x else NA))[-1]
  }

  return(output)
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
  DDdata <- getDDdata(DD)

  varset <- unique(unlist(lapply(as.list(unname(unlist(rules))),ssplit,left = TRUE)))
  linkvars <- unlist(DDdata[Variable %in% varset & Sort == "IDDD", getQuals(DDdata), with = FALSE])
  if (DDdata[Sort == "IDQual",Variable][1] %in% linkvars)
    varset <- union(varset,linkvars)
  varset <- intersect(varset,unlist(DDdata[Sort == "IDDD",Variable]))

  output <- list(varset[1])
  for (variable in varset[-1]) {
    vlogic <- unlist(lapply(lapply(output,`[`,i = 1),
                function(x) setequal(unlist(DDdata[Variable == x & Sort == "IDDD",getQuals(DDdata), with = FALSE]),
                  unlist(DDdata[Variable == variable & Sort == "IDDD",getQuals(DDdata), with = FALSE]))))
    if (!any(vlogic)) output <- c(output,variable)
    else output[[which.max(vlogic)]] <- c(output[[which.max(vlogic)]],variable)
  }

  return(output)
}

# setOrderVars puts main variable in first place
setOrderVars <- function(vars,DTList,DD)
{
  DDdata <- getDDdata(DD)

  DTListQuals <- lapply(DTList,key)
  varsQuals <- lapply(lapply(vars,`[`,i = 1), function(x) {
    output <- unlist(DDdata[Variable == x & Sort == "IDDD", getQuals(DDdata),with = FALSE])
    output <- output[output != ""]
    return(output)
  })
  AggIDQuals <- unlist(DDdata[Sort == "IDQual",Variable])[-1]
  allQuals <- setdiff(unique(unname(unlist(varsQuals))),AggIDQuals)
  if (length(allQuals) == 0) allQuals <- unique(unname(unlist(varsQuals)))
  vlogic <- unlist(lapply(varsQuals,function(x) setequal(intersect(x,allQuals),allQuals)))
  if (any(vlogic))
    output <- c(vars[which.max(vlogic)],vars[-which.max(vlogic)])
  else return(NULL)

  vlogic <- unlist(lapply(output[-1], function(x) return(length(intersect(x,AggIDQuals)) != 0)))
  if (length(vlogic) == 0) return(output)
  else return(c(output[1],output[-1][vlogic],output[-1][!vlogic]))
}


# getDataTableList returns a list of data.tables from a StQ and a list of variables grouped by qualifier

getDataTableList <-  function(object, vars)
{

  # Get Slots from objects
  DD <- getDD(object)
  Data <- getData(object)
  DDdata <- getDDdata(DD)

  output <- lapply(vars,function(newvars) {
    quals <- unlist(DDdata[Variable == newvars[1] & Sort == "IDDD",getQuals(DDdata), with = FALSE])
    quals <- quals[quals != ""]
    subtable <- dcast(Data, getFormula(quals), subset = .(IDDD %in% newvars), value.var = "Value")
    subtable[,colnames(subtable) :=
        lapply(as.list(colnames(subtable)), function(x) as(get(x),unlist(DDdata[Variable == x,"Class",with = FALSE])[1])),
        with = FALSE]
    setkeyv(subtable,unname(quals))
    return(subtable)
    })

  return(output)
}

# mergeDataTable creates the data.table where we will apply the rule from DTList and vars
mergeDataTable <- function(DTList,vars) {


  newdata <- lapply(vars,function(x) {
    vlogic <- unlist(lapply(DTList, function(y) x[1] %in% setdiff(colnames(y),key(y))))
    output <- DTList[[which.max(vlogic)]][,c(key(DTList[[which.max(vlogic)]]),x),with = FALSE]
    return(output)
  })

  output <- newdata[[1]]
  for (y in newdata[-1]) output <- merge(output,y,by = key(y))
  return(output)
}

# DDadd adds new variables to DD file
DDadd <- function(variables,DD,DT) {

  #get Slots from objects
  microdata <- getData(DD)
  if (microdata[Sort == "IDQual",Variable] %in% key(DT)) DDdata <- microdata
  else DDdata <- getAggr(DD)

  variables <- union(variables,key(DT))
  variables <- setdiff(variables,DDdata$Variable)
  if (length(variables) == 0) return(DD)

  incDD <- do.call(data.table,c(list(variables, ifelse(variables %in% key(DT),"Qual","IDDD"),
                unname(unlist(lapply(DT[,variables,with = FALSE],class)))),
                unname(split(unlist(lapply(variables %in% key(DT),
                  function(x) if (x) return(rep("",length(key(DT)))) else return(key(DT)))),
                rep(1:length(key(DT)),length(variables))))))
  colnames(incDD) <- c("Variable","Sort","Class",paste0("Qual",1:(ncol(incDD) - 3)))

  if (microdata[Sort == "IDQual",Variable] %in% key(DT)) {
    incDD[Sort == "Qual",Sort := "NonIDQual"]
    DDdata <- rbindlist(list(DDdata,incDD),fill = TRUE)
    microdata <- DDdata
    DDdata <- getAggr(DD)
  }
  else {
    incDD[Sort == "Qual" & (Variable %in% microdata[Sort == "IDDD",Variable]),Sort := "IDQual"]
    incDD[Sort == "Qual" & !(Variable %in% microdata[Sort == "IDDD",Variable]),Sort := "NonIDQual"]
    DDdata <- rbindlist(list(DDdata,incDD),fill = TRUE)
  }

  DDdata <- as.data.table(lapply(DDdata,function(x) {
    x[is.na(x)] <- ""
    return(x)
  }))

  microdata <- as.data.table(lapply(microdata,function(x) {
    x[is.na(x)] <- ""
    return(x)
  }))

  return(new(Class = "DD", VarNameCorresp = getVNC(DD), MicroData = microdata, Aggregates = DDdata))

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

# getDDdata returns a data.table with information from DD slot

getDDdata <- function(DD) {
  DDdata <- rbindlist(list(getData(DD),getAggr(DD)),fill = TRUE, use.names = TRUE)
  DDdata <- as.data.table(lapply(DDdata,function(x) {
    x[is.na(x)] <- ""
    return(x)
  }))
  return(DDdata)
}
