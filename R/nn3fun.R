#' Returns the nearest neighbour for each element by stratum
#'
#' \code{nn3fun} returns a list with the nearest neighbour, order of the donor use, and distance.
#'
#' Mahalanobis distance is used
#'
#' @param id Vector with elements identification
#'
#' @param stratum Vector with stratum identification
#'
#' @param values Vector with variables to compute distance with
#'
#' @param donor Class vector for receivers: 0, donors: 1, both: 2
#'
#' @param nvar Number of variables in vector values
#'
#' @return list as said
#'
#' @examples
#' nn3fun
#' @import MASS
#'
#' @export
nn3fun<-function(id,stratum,values,donor,nvar) {

  # Computation of the inverse of the covariance matrix. It may be singular, so we use ginv
  cinv<-ginv(var(t(matrix(values,nrow=nvar)),use="na.or.complete"))

  # We create receivers matrix and vectors for unit and stratum identification
  matreceivers<-matrix(values[donor!=1],nrow=nvar)
  if(!ncol(matreceivers)) {
    if (is.numeric(id)) return(list(as.numeric(NA),as.numeric(NA),as.numeric(NA)))
    if (is.character(id)) return(list(as.character(NA),as.numeric(NA),as.numeric(NA)))
  }
  idreceivers<-id[donor!=1][seq.int(1,length(matreceivers),nvar)]
  strreceivers<-stratum[donor!=1][seq.int(1,length(matreceivers),nvar)]

  # Same for donors
  matdonors<-matrix(values[donor!=0],nrow=nvar)
  iddonors<-id[donor!=0][seq.int(1,length(matdonors),nvar)]
  strdonors<-stratum[donor!=0][seq.int(1,length(matdonors),nvar)]

  # Mahalanobis distance
  distance<-function(x) x%*%cinv%*%x

  # Computation of the nearest neighbour for each receiver by stratum
  idneighbours<-idreceivers
  countneighbours<-numeric(length(idreceivers))
  distneighbours<-numeric(length(idreceivers))
  for(str in unique(stratum)) {
    if (prod(dim(matdonors[,strdonors==str,drop=FALSE]))) {
      distmat<-apply(rbind(1:length(idreceivers),matreceivers)[,strreceivers==str,drop=FALSE],2,function(x) {
        temp<-matdonors[,iddonors==idreceivers[x[1]]]
        matdonors[,iddonors==idreceivers[x[1]]]<-NA
        output<-apply(matdonors[,strdonors==str,drop=FALSE]-x[-1],2,distance)
        matdonors[,iddonors==idreceivers[x[1]]]<-temp
        return(output)})
      if(is.null(dim(distmat))) distmat<-matrix(distmat,nrow=1)
      selecteddonors<-apply(distmat,2,function(x) {if (all(is.na(x))) {return (NA)} else {return(which.min(x))}})
      selecteddistances<-distmat[(0:(ncol(distmat)-1))*nrow(distmat)+selecteddonors]
      orderedreceivers<-order(selecteddonors,selecteddistances)
      ordereddonors<-selecteddonors[orderedreceivers]
      auxvector<-ordereddonors-c(ordereddonors[1],ordereddonors)[-length(ordereddonors)-1]
      countneighbours[strreceivers==str][orderedreceivers]<-
        (1:length(auxvector))-cummax(as.numeric(auxvector!=0)*(0:(length(auxvector)-1)))
      distneighbours[strreceivers==str]<-selecteddistances
      idneighbours[strreceivers==str]<-iddonors[strdonors==str][selecteddonors]
    }
    else {
        idneighbours[strreceivers==str]<-NA
        countneighbours[strreceivers==str]<-NA
        distneighbours[strreceivers==str]<-NA
    }
  }
  output.id<-rep(NA,length=length(id))
  output.count<-rep(NA,length=length(id))
  output.dist<-rep(NA,length=length(id))
  output.id[donor!=1]<-rep(idneighbours,each=nvar)
  output.count[donor!=1]<-rep(countneighbours,each=nvar)
  output.dist[donor!=1]<-rep(distneighbours,each=nvar)
  return(list(output.id,output.count,output.dist))

}
