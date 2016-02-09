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
#' igualdades de input, copiando según las igualdades especificadas en output}
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
#' @include StQT-class.R getRules.R
#' @import data.table
#'
#' @export
setMethod(
  f = "TApply",
  signature = c("data.table", "StQT"),
  function(x, Tr){

      # La funcion expand separa en un vector las cadenas de caracteres con varias variables
      expand <- function(string)
      {
        string<-unlist(strsplit(string,"[,]"))
        return(string)
      }

      # La funcion ssplit extrae las variables a la izquierda o a la derecha de una igualdad
      ssplit <- function(string,left=TRUE)
      {
        string<-expand(string)
        if(left)
        {
          return(unlist(lapply(strsplit(string,"="),"[",i=1)))
        }
        else
        {
          return(unlist(lapply(strsplit(string,"="),"[",i=2)))
        }
      }

      # Se extraen las reglas
      rules<-getRules(Tr)
      functions<-getFunctions(Tr)

      # Se aplican por orden todas las reglas
      for(i in 1:nrow(rules))
      {
        # Se ordenan los datos
        if (rules$key[i]!="")
        {
          setkeyv(x,expand(rules$key[i]))
        }

        # Se comprueban las funciones especiales:
        # FunDelRow elimina las filas que cumplen la condición especificada

        if (rules$fun[i]=="FunDelRow")
        {
          x<-do.call("subset",list(x,parse(text=paste("!(",rules$domain[i],")",sep=""))))
          next
        }

        # FunDelCol elimina las columnas especificadas en output
        if (rules$fun[i]=="FunDelCol")
        {
          x[,expand(rules$output[i]):=NULL]
          next
        }

        # FunAutoLink extrae información, especificada en el output, de otras filas de la tabla,
        # según los enlaces indicados en el input
        if (rules$fun[i]=="FunAutoLink")
        {
          link1<-ssplit(rules$input[i],TRUE)
          link2<-ssplit(rules$input[i],FALSE)
          selected<-ssplit(rules$output[i],FALSE)
          fieldnames<-ssplit(rules$output[i],TRUE)
          xtemp<-x[,mget(link1)]
          setkeyv(xtemp,link1)
          setkeyv(x,link2)
          xdata<-x[xtemp,mget(selected)]
          setkeyv(x,link1)
          x[,fieldnames:=xdata,with=FALSE]
          next
        }

        # Pasamos a las funciones normales:
        # Inserción de columnas

        if (!grepl("=",rules$output[i]))
        {
          if (rules$domain[i]=="")
            do.call(`[`,list(x,j=quote(expand(rules$output[i]):=do.call(functions[[rules$fun[i]]],unname(mget(expand(rules$input[i]))))),
              by=expand(rules$by[i])))
          else
          do.call(`[`,list(x,i=parse(text=rules$domain[i]),
              j=quote(expand(rules$output[i]):=do.call(functions[[rules$fun[i]]],unname(mget(expand(rules$input[i]))))),
              by=expand(rules$by[i])))
          next
        }

        # Inserción de filas
        else
        {
          # Primero calculamos las filas a insertar
          vars<-ssplit(rules$output[i],TRUE)
          values<-ssplit(rules$output[i],FALSE)

          if (rules$domain[i]=="")
            xtemp<-do.call(`[`,list(x,
                    j=quote(setNames(c(list(),do.call(functions[[rules$fun[i]]],unname(mget(expand(rules$input[i]))))),vars[is.na(values)])),
                    by=expand(rules$by[i])))
          else
            xtemp<-do.call(`[`,list(x,i=parse(text=rules$domain[i]),
              j=quote(setNames(c(list(),do.call(functions[[rules$fun[i]]],unname(mget(expand(rules$input[i]))))),vars[is.na(values)])),
              by=expand(rules$by[i])))

          xtemp[,vars[!is.na(values)]:=as.list(values[!is.na(values)])]

          # Se insertan las filas nuevas

          x<-rbindlist(list(x,xtemp),use.names=TRUE,fill=TRUE)

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
    
    rules<-getRules(Tr)
    functions<-getFunctions(Tr)
    
    # Se aplican por orden todas las reglas
    for(i in 1:nrow(rules))
    {
    
   
    }
  }
)
