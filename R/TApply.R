#' Aplica una transformación a una tabla de datos.
#'
#' \code{TApply} devuelve una tabla de datos transformada.
#'
#' Existen tres tipos de reglas de transformación básicas:
#' funciones internas, inserción de filas e inserción de columnas.
#' \describe{
#' \item{Funciones internas}{FunDelRow, FunDelCol y FunAutoLink. La primera borra
#' las filas que cumplen la condición especificada en domain. La segunda borra las columnas
#' especificadas en output. La tercera extrae datos de la propia tabla, enlazando según las
#' igualdades de domain, copiando el input en el output}
#' \item{Inserción de filas}{Se insertan filas. Los output que contienen una igualdad se asignan directamente
#' y el resto se calculan según el esquema output=fun(input)}
#' \item{Inserción de columnas}{Se insertan columnas calculadas según el esquema output = fun (input)}
#' }
#' El parámetro domain restringe los registros en los que se realiza el cálculo.
#' El parámetro by aplica la transformación por subtablas según los valores de las variables
#' contenidas en dicho parámetro.
#' El parámetro key aplica una ordenación de la tabla (útil para incrementar la velocidad),
#' que se realiza previamente a la transformación correspondiente.
#'
#'
#' @param x Objeto de clase \linkS4class{StQ} o \linkS4class{data.table} con los
#' datos a transformar
#'
#' @param Tr Objeto de clase \linkS4class{StQT} con las reglas de transformación
#' a aplicar.
#'
#' @return \linkS4class{StQ} o \linkS4class{data.table} con los datos transformados
#'
#' @examples
#' x
#' @export
setGeneric("TApply", function(x,Tr){standardGeneric("TApply")})

#' @rdname TApply
#'
#' @include StQT-class.R getRules.R getFunctions.R Utils.R
#' @import data.table
#'
#' @export
setMethod(
  f = "TApply",
  signature = c("data.table", "StQT"),
  function(x,Tr){

      # Se extraen las reglas
      rules <- getRules(Tr)
      functions <- getFunctions(Tr)

      # Se aplican por orden todas las reglas
      for (i in 1:nrow(rules))
      {
        # Se ordenan los datos
        if (rules$key[i] != "")
        {
          setkeyv(x,expand(rules$key[i]))
        }

        # Se comprueban las funciones especiales:
        # FunDelRow elimina las filas que cumplen la condición especificada

        if (rules$fun[i] == "FunDelRow")
        {
          x <- do.call("subset",list(x,parse(text = paste("!(",rules$domain[i],")",sep = ""))))
          next
        }

        # FunDelCol elimina las columnas especificadas en output
        if (rules$fun[i] == "FunDelCol")
        {
          x[,expand(rules$output[i]) := NULL]
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
          xtemp <- x[,mget(link1)]
          setkeyv(xtemp,link1)
          setkeyv(x,link2)
          xdata <- x[xtemp,mget(selected)]
          setkeyv(x,link1)
          x[,fieldnames := xdata,with = FALSE]
          next
        }

        # Pasamos a las funciones normales:
        # Inserción de columnas

        if (!grepl("=",rules$output[i]))
        {
          f <- functions[[rules$fun[i]]]
          outnames <- expand(rules$output[i])

          command <- paste0("x[",rules$domain[i],",outnames:=f(",rules$input[i],")")
          if (rules$by[i] != "") command <- paste0(command,",by = .(",rules$by[i],"),with=FALSE]")
          else command <- paste0(command,",with=FALSE]")
          eval(parse(text = command))
          next
        }

        # Inserción de filas
        else
        {
          # Primero calculamos las filas a insertar
          vars <- ssplit(rules$output[i],TRUE)
          values <- ssplit(rules$output[i],FALSE)
          f <- functions[[rules$fun[i]]]

          command <- paste0("xtemp <- x[",rules$domain[i],",f(",rules$input[i],")")
          if (rules$by[i] != "") command <- paste0(command,",by = .(",rules$by[i],")]")
          else command <- paste0(command,"]")
          eval(parse(text = command))
          setnames(xtemp,(ncol(xtemp) - sum(is.na(values)) + 1):ncol(xtemp),vars[is.na(values)])
          xtemp[,vars[!is.na(values)] := values[!is.na(values)]]

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
#' @include StQT-class.R getRules.R
#' @import StQ
#'
#' @export
setMethod(
  f = "TApply",
  signature = c("StQ", "StQT"),
  function(x, Tr){

    # Extract slots from objects
    rules <- getRules(Tr)
    functions <- getFunctions(Tr)
    DD <- getDD(x)
    DATA <- getData(x)

    vars <- getVars(rules,DD)
    DTList <- getDataTableList(x,vars)
    DATA <- DATA[!(IDDD %in% unlist(vars)),]

    for (i in 1:nrow(rules)) {

        # Extract variables
      vars <- getVars(rules[i,],DD)
      vars <- setOrderVars(vars,DTList,DD)
      if (is.null(vars)) stop("[StQT::TApply] There is no obvious way to link variables at rule ",i)

        # Set data.table
      DT <- mergeDataTable(DTList,vars,DD)
      if (rules$domain[i] != "") DT <- DT[eval(parse(text = rules$domain[i])),]

        # Apply rule in data.table
      rows <- nrow(DT)
      DT <- TApply(DT,Tr[i])
      if (nrow(DT) > rows) DT <- DT[-1:-rows,]

        # Store new variables / Update DD

      if (rules$ref[i] == "") {
         # Default qualifiers are those from main variable
        microdata <- getData(DD)
        ref <- unname(unlist(microdata[Variable == vars[[1]][1], getQuals(microdata),with = FALSE]))
        ref <- ref[ref != ""]
      }
      else ref <- expand(rules$ref[i])

      DT <- DT[,c(ref, setdiff(ssplit(rules$output[i]),ref)),with = FALSE]
      setkeyv(DT,ref)
      DT <- unique(DT)

      vlogic <- unlist(lapply(DTList,function(x) setequal(key(x),ref)))
      if (any(vlogic)) {
        DTList[[which.max(vlogic)]] <- merge(DTList[[which.max(vlogic)]],DT,all = TRUE, suffixes = c("",".NEW"))
        newcols <- colnames(DTList[[which.max(vlogic)]])[grepl(".NEW",colnames(DTList[[which.max(vlogic)]]))]
        if (length(newcols)) {
          oldcols <- substr(newcols,1,nchar(newcols) - 4)
          DTList[[which.max(vlogic)]][, c(oldcols,newcols) :=
            c(mapply(combine,mget(oldcols),mget(newcols),SIMPLIFY = FALSE),rep(list(NULL),length(newcols))),with = FALSE]
        }
      }
      else {
        DTList <- c(DTList,list(DT))
      }
      DD <- DDadd(ssplit(rules$output[i]),DD,DT)
    }

    # merge new data in x

    setkeyv(DATA,setdiff(colnames(DATA),c("IDDD","Value")))
    DTList <- c(list(DATA),DTList)
    quals <- unique(unname(unlist(lapply(DTList,key))))
    DTList <- lapply(DTList,function(x) {
      x[,setdiff(quals,colnames(x)) := "", with = FALSE]
      return(x)
    })
    DTList <- c(DTList[1],lapply(DTList[-1],melt,id.vars = quals, variable.name = "IDDD", value.name = "Value"))
    DATA <- rbindlist(DTList,fill = TRUE)
    DATA[is.na(Value),Value := ""]
    return(new(Class = "StQ", Data = DATA, DD = DD))

  }
)
