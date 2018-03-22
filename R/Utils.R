#' @import StQ data.table
# Default behaviour changed in data.table package
unique.data.table <- function(x,...)
{
    nameparm <- names(list(...))
    if ("by" %in% nameparm)
      getS3method("unique",class = "data.table",envir = as.environment("package:data.table"))(x, ...)
    else
      getS3method("unique",class = "data.table", envir = as.environment("package:data.table"))(x, by = key(x), ...)
}


# Removes internal functions and atomic expressions from a character vector with function names
RemoveInternal <- function(x)
{
  x <- x[!unlist(lapply(x,function(x) tryCatch(is.atomic(parse(text = x)[[1]]) ||
                ((substr(x, 1, 1) == "-") && is.numeric(parse(text = x)[[1]][[2]])),
                  error = function(cond){return(FALSE)})))]
  return(setdiff(x,c("FunDelRow","FunDelVar","FunAutoLink")))
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

# getVars returns a list with the vars in a transformation grouped by qualifiers from DD object
getVars <- function(rules,DD)
{
  DDdata <- getDDdata(DD)

  varset <- unique(unlist(lapply(as.list(unname(unlist(rules))),expand)))
  linkvars <- unlist(DDdata[Variable %in% varset & Sort == "IDDD", getQuals(DDdata), with = FALSE])
  if (any(DDdata[Sort == "IDQual",Variable][1] %in% linkvars))
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

# getVars2 returns a list with the vars in a transformation grouped by qualifiers from data.table list
getVars2 <- function(rules,DTList)
{
  keyvalues <- lapply(DTList,key)
  idddvalues <- mapply(setdiff,lapply(DTList,colnames),keyvalues,SIMPLIFY = FALSE)

  varset <- unique(unlist(lapply(as.list(unname(unlist(rules))),expand)))
  linkvars <- unique(unlist(keyvalues[unlist(lapply(idddvalues,function(x) length(intersect(x,varset)) != 0))]))
  varset <- union(varset,linkvars)
  output <- lapply(idddvalues,function(x) intersect(x,varset))
  output <- output[unlist(lapply(output,function(x) length(x) != 0))]

  return(output)
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
    subtable[,(colnames(subtable)) :=
        lapply(as.list(colnames(subtable)), function(x) as(get(x),unlist(DDdata[Variable == x,Class])[1]))]
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
  for (y in newdata[-1]) {
    merged <- merge(output, y, all = TRUE, by = intersect(colnames(output), colnames(y)), allow.cartesian = TRUE)
    output <- rbind(merged, output,y, fill = TRUE)
  }
  newkey <- setdiff(unique(unlist(lapply(newdata,key))),unlist(vars))
  setkeyv(output,newkey)
  return(output)
}

# DDadd adds new variables to DD file
DDadd <- function(DD,DTList) {

  keyvalues <- lapply(DTList,key)
  idddvalues <- mapply(setdiff,lapply(DTList,colnames),keyvalues,SIMPLIFY = FALSE)
  common <- intersect(unlist(keyvalues),unlist(idddvalues))

  for (variable in common) lapply(DTList, function(x) if (variable %in% key(x)) setnames(x,variable,paste0("Agg",variable)))
  keyvalues <- lapply(DTList,key)

  keyvalues <- lapply(keyvalues,function(x) union(getIDQual(DD,"MicroData"),x))
  QualNumber <- max(unlist(lapply(keyvalues,length)))

  cols <- union(colnames(getMicroData(DD)),paste0("Qual",1:QualNumber))
  microdata <- setnames(as.data.table(matrix(character(),ncol = length(cols), nrow = 0)),cols)

  DDList <- lapply(1:length(DTList),function(i) {
    output <- data.table(Variable = setdiff(c(keyvalues[[i]],idddvalues[[i]]),getIDQual(DD,"MicroData")))
    output[Variable %in% idddvalues[[i]], Sort := "IDDD"]
    output[Variable %in% keyvalues[[i]], Sort := "NonIDQual"]
    classList <- lapply(DTList[[i]],class)
    f <- function(x) unlist(classList[x])
    output[,Class := f(Variable)]
    QualNumber <- length(keyvalues[[i]])
    output[Sort == "IDDD",(paste0("Qual",1:QualNumber)) := as.list(keyvalues[[i]])]
    return(output)
  })

  microdata <- rbindlist(c(list(microdata),list(getMicroData(DD)[Variable %in% getIDQual(DD,"MicroData")]),DDList),fill = TRUE)

  microdata <- as.data.table(lapply(microdata,function(x) {
    x[is.na(x)] <- ""
    return(x)
  }))

  microdata <- unique(microdata,by = "Variable")
  microdata <- microdata[!(Variable %in% getMicroData(DD)$Variable) | Sort != "IDDD"]

  VarNameCorresp = DDdtToVNC(microdata,'MicroData')
  return(DD + BuildDD( list(VNC = VarNameCorresp, MicroData = microdata)))

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
  DDdata <- rbindlist(list(getMicroData(DD),getAggregates(DD)),fill = TRUE, use.names = TRUE)
  DDdata <- as.data.table(lapply(DDdata,function(x) {
    x[is.na(x)] <- ""
    return(x)
  }))
  return(DDdata)
}

# fillSpaces fills a character with spaces to achive a specified width
fillSpaces <- function(x,n) {
  dif <- n - nchar(x)
  if (any(dif < 0)) warning("fillSpaces error: n is smaller than the length of one of the strings")
  x[dif == 1] <- paste0(x[dif == 1]," ")
  x[dif > 1] <- paste0(" ",x[dif > 1]," ")
  if (any(dif > 2)) return(fillSpaces(x,n))
  else return(x)
}
