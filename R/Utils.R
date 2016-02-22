# Removes internal functions from a character vector with function names
RemoveInternal<-function(x)
{
  return(setdiff(x,c("FunDelRow","FunDelCol","FunAutoLink")))
}

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
