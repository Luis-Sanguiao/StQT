#' Applies a transformation object on a StQ or data.table.
#'
#' \code{TApply} returns a transformed object according to the transformation rules.
#'
#' There are three kinds of transformation rules:
#' internal functions, row insertion and column insertion.
#' \itemize{
#' \item{Internal functions}{FunDelRow, FunDelVar and FunAutoLink so far. The first one deletes
#' rows in domain. The second one deletes the variables specified in the output. The third one
#' copies the input variables from the table linking according to the expression in domain.}
#' \item{Row insertion}{Row are inserted. This kind of rule is applied when there exists an assignment in the output.
#' Unassigned variables are calculated according output = fun(input). Any variable included in by is copied into the new rows.}
#' \item{Column insertion}{Rows are calculated according output = fun (input)}
#' }
#' Parameter domain specifies the subset of the dataset where we are going to do the calculation.
#' Parameter by sets the variables where we are applying the rule by.
#' Parameter order orders the table, previously to apply the transformation.
#' Parameter key (irrelevant for data.table) sets which of the variables are qualifiers.
#'
#'
#' @param x Object of class \linkS4class{StQ} or \linkS4class{data.table} with data
#' to transform
#'
#' @param Tr Object of class \linkS4class{StQT} with the transformation we are going
#' to apply.
#'
#' @return \linkS4class{StQ} or \linkS4class{data.table} respectively with transformed
#' data
#'
#' @examples
#' require(data.table)
#' dt <- data.table(Region = c("1","2","3"), GDP = c(1.38,0.94,1.23))
#' dt
#' T1 <- NewStQT(data.frame(
#'   output = "Region='Total',GDP",
#'   fun = "sum",
#'   input = "GDP",
#'   stringsAsFactors = FALSE))
#' TApply(dt,T1)
#' require(StQ)
#' data(ExampleStQ)
#' rules <- data.frame(domain = c('','Turnover > 0'),
#'                     output = c('AggTurnover','LogTurnover'),
#'                        fun = c('sum','log'),
#'                      input = c('Turnover','Turnover'),
#'                         by = c('NACE09',''),
#'                        key = c('NACE09',''),
#'                          stringsAsFactors = FALSE)
#' T2 <- NewStQT(rules)
#' (ExampleStQ2 <- TApply(ExampleStQ,T2))
#'
#' @export
setGeneric("TApply", function(x,Tr){standardGeneric("TApply")})

#' @rdname TApply
#'
#' @include StQT-class.R getRules.R getFunctions.R Utils.R input.R
#' @import data.table
#'
#' @export
setMethod(
  f = "TApply",
  signature = c("data.table", "StQT"),
  function(x,Tr){

      # Get slots from objects
      rules <- getRules(Tr)
      if (nrow(rules) == 0) return(x)

      if (!setequal(intersect(input(Tr),colnames(x)),input(Tr)))
        stop("[StQT::TApply] The transformation input needs some variables that are missing in the data.table")

      functions <- getFunctions(Tr)

      # Se aplican por orden todas las reglas
      for (i in 1:nrow(rules))
      {
        # Se ordenan los datos
        if (rules$order[i] != "")
        {
          setkeyv(x,expand(rules$order[i]))
        }

        # Se comprueban las funciones especiales:
        # FunDelRow elimina las filas que cumplen la condición especificada

        if (rules$fun[i] == "FunDelRow")
        {
          x <- do.call("subset",list(x,parse(text = paste("!(",rules$domain[i],")",sep = ""))))
          next
        }

        # FunDelVar elimina las columnas especificadas en output
        if (rules$fun[i] == "FunDelVar")
        {
          x[,(expand(rules$output[i])) := NULL]
          next
        }

        # FunAutoLink extrae información, especificada en el output, de otras filas de la tabla,
        # según los enlaces indicados en el input
        if (rules$fun[i] == "FunAutoLink")
        {
          link1 <- sidelink(parse(text = rules$domain[i])[[1]],TRUE)
          link2 <- sidelink(parse(text = rules$domain[i])[[1]],FALSE)

          selected <- expand(rules$input[i])
          fieldnames <- expand(rules$output[i])
          xtemp <- merge(unique(na.omit(x[,mget(link1)])),
                         unique(na.omit(x[,mget(link2)])),by.x = link1, by.y = link2)
          xdata <- unique(x[xtemp,mget(union(link2,selected)),on = setNames(link1,link2)])

          x[xdata,(fieldnames) := mget(paste0("i.",selected)),on = setNames(link2,link1)]

          next
        }

        # Pasamos a las funciones normales:
        # Inserción de columnas

        if (!grepl("=",rules$output[i]))
        {
          if (rules$fun[i] %in% names(functions))
            f <- functions[[rules$fun[i]]]
          else {
            f <- function() return(1)
            body(f) <- substitute(return(x), list(x = eval(parse(text = rules$fun[i]))))
          }

          outnames <- expand(rules$output[i])

          command <- paste0("x[",rules$domain[i],",(outnames):=f(",rules$input[i],")")
          if (rules$by[i] != "") command <- paste0(command,",by = .(",rules$by[i],")]")
          else command <- paste0(command,"]")
          eval(parse(text = command))
          next
        }

        # Inserción de filas
        else
        {
          # Primero calculamos las filas a insertar
          vars <- ssplit(rules$output[i],TRUE)
          values <- ssplit(rules$output[i],FALSE)

          if (rules$fun[i] %in% names(functions))
            f <- functions[[rules$fun[i]]]
          else {
            f <- function() return(1)
            body(f) <- substitute(return(x), list(x = eval(parse(text = rules$fun[i]))))
          }

          if (length(vars[is.na(values)]) > 1 || rules$by[i] != "")
            command <- paste0("xtemp <- x[",rules$domain[i],",f(",rules$input[i],")")
          else
            command <- paste0("xtemp <- x[",rules$domain[i],",c(list(),f(",rules$input[i],"))")
          if (rules$by[i] != "") command <- paste0(command,",by = .(",rules$by[i],")]")
          else command <- paste0(command,"]")
          eval(parse(text = command))
          setnames(xtemp,(ncol(xtemp) - sum(is.na(values)) + 1):ncol(xtemp),vars[is.na(values)])
          xtemp[,(vars[!is.na(values)]) := values[!is.na(values)]]

          # Se insertan las filas nuevas

          x <- rbindlist(list(x,xtemp),use.names = TRUE,fill = TRUE)

          next
        }

      }

    return(x)
  }
)

#' @rdname TApply
#'
#' @import StQ data.table
#'
#' @export
setMethod(
  f = "TApply",
  signature = c("StQ", "StQT"),
  function(x, Tr){

    # Get slots from objects
    rules <- getRules(Tr)
    if (nrow(rules) == 0) return(x)

    DD <- getDD(x)
    if (!setequal(intersect(input(Tr),getDDdata(DD)$Variable),input(Tr)))
      stop("[StQT::TApply] The transformation input needs some variables that are missing in the StQ")
    functions <- getFunctions(Tr)

    DATA <- getData(x)

    vars <- getVars(rules[,c("domain","output","input","by","order")],DD)
    DTList <- getDataTableList(x,vars)

    DATA <- DATA[!(IDDD %in% unlist(vars)),]

    for (i in 1:nrow(rules)) {

      # FunDelVar
      if (rules$fun[i] == "FunDelVar") {
        vars <- expand(rules$output[i])
        # Remove variables from DD
        microdata <- getMicroData(DD)
        aggregates <- getAggr(DD)
        microdata <- subset(microdata, !(Variable %in% vars))
        # We have to remove any var with missing qualifiers
        vlogic <- apply(aggregates[,getQuals(aggregates),with = FALSE],1,
                        function(x) length(intersect(x,vars)) != 0)
        vars <- union(vars,unlist(aggregates[vlogic,Variable]))
        aggregates <- subset(aggregates, !(Variable %in% vars))
        # We also have to remove unneeded qualifiers
        quals <- union(unlist(microdata[Sort == "IDDD", getQuals(microdata), with = FALSE]),
                       unlist(aggregates[Sort == "IDDD", getQuals(aggregates), with = FALSE]))
        removequals <- setdiff(union(unlist(microdata[Sort != "IDDD", Variable]),
                          unlist(aggregates[Sort != "IDDD", Variable])),quals)
        microdata <- subset(microdata, !(Variable %in% removequals) | Sort == "IDDD")
        aggregates <- subset(aggregates, !(Variable %in% removequals) | Sort == "IDDD")
        # Finally we discard any empty Qual* column except for the first one
        if (length(getQuals(microdata)) > 1)
          vlogic <- apply(microdata[,getQuals(microdata)[-1],with = FALSE],2,function(x) all(x == ""))
        else
          vlogic <- FALSE
        if (any(vlogic)) microdata[,(getQuals(microdata)[-1][vlogic]) := NULL]
        if (length(getQuals(aggregates)) > 1)
          vlogic <- apply(aggregates[,getQuals(aggregates)[-1],with = FALSE],2,function(x) all(x == ""))
        else
          vlogic <- FALSE
        if (any(vlogic)) aggregates[,(getQuals(aggregates)[-1][vlogic]) := NULL]
        VarNameCorresp <- DDdtToVNC(microdata,'MicroData') + DDdtToVNC(aggregates, 'Aggregates')
        DD <- BuildDD(list( VNC = getVNC(DD), MicroData = microdata, Aggregates = aggregates))

        # Remove variables from DTList
        correspvars <- lapply(DTList,function(x) intersect(setdiff(colnames(x),key(x)),vars))
        mapply(function(x,y) {if (length(y)) x[,(y) := NULL]
                              return(NULL)
                              },DTList,correspvars)
        # Keep only the data.table's with variables
        DTList <- DTList[unlist(lapply(DTList,function(x) !setequal(key(x),colnames(x))))]
        # Remove unneeded qualifiers in the unmodified DATA
        if (length(intersect(removequals,colnames(DATA))))
          DATA[(intersect(removequals,colnames(DATA))) := NULL]
        next
      }

        # Extract variables
      vars <- getVars2(rules[i,][,c("domain","input","by","order")],DTList)

        # Set data.table
      DT <- mergeDataTable(DTList,vars)

       # Set qualifiers
      if (rules$key[i] == "") ref <- key(DT)
      else ref <- expand(rules$key[i])

      # Restrict domain
      if (rules$domain[i] != "") DT <- DT[eval(parse(text = rules$domain[i])),]

        # Apply rule in data.table
      rows <- nrow(DT)
      DT <- TApply(DT,Tr[i])

      if (nrow(DT) > rows) DT <- DT[-1:-rows,]

        # Store new variables / Update DD

      DT <- DT[,c(ref, setdiff(ssplit(rules$output[i]),ref)),with = FALSE]
      setkeyv(DT,ref)
      DT <- unique(DT)
      DT <- na.omit(DT,cols = setdiff(ssplit(rules$output[i]),ref))

      vlogic <- unlist(lapply(DTList,function(x) setequal(key(x),ref)))
      if (any(vlogic)) {
        DTList[[which.max(vlogic)]] <- merge(DTList[[which.max(vlogic)]],DT,all = TRUE, suffixes = c("",".NEW"))
        newcols <- colnames(DTList[[which.max(vlogic)]])[grepl(".NEW",colnames(DTList[[which.max(vlogic)]]))]
        if (length(newcols)) {
          oldcols <- substr(newcols,1,nchar(newcols) - 4)
          DTList[[which.max(vlogic)]][, (c(oldcols,newcols)) :=
            c(mapply(combine,mget(oldcols),mget(newcols),SIMPLIFY = FALSE),rep(list(NULL),length(newcols)))]
        }
      }
      else {
        DTList <- c(DTList,list(DT))
      }
    }

    # merge new data in x

    DD <- DDadd(DD,DTList)

    DTList <- c(list(DATA),DTList)

    DTList <- c(DTList[1],lapply(DTList[-1],function(x)
      melt(data = x,id.vars = key(x), variable.name = "IDDD", value.name = "Value", variable.factor = FALSE)))
    DATA <- rbindlist(DTList,fill = TRUE)
    DATA <- as.data.table(lapply(DATA, function(x) {
      x[is.na(x)] <- ""
      return(x)
    }))
    setcolorder(DATA,c(setdiff(colnames(DATA),c("IDDD","Value")),"IDDD","Value"))

    return(StQ(Data = DATA, DD = DD))

  }
)
