nn3fun<-function(id,stratum,values,donor,nvar) {

  # Computation of the inverse of the covariance matrix. Cholesky should be faster than solve.

  cinv<-chol2inv(chol(var(t(matrix(values,nrow=nvar)),use="na.or.complete")))

  # We create receivers matrix and vectors for unit and stratum identification
  matreceivers<-matrix(values[!donor],nrow=nvar)
  idreceivers<-id[!donor][seq.int(1,ncol(matreceivers),nvar)]
  strreceivers<-stratum[!donor][seq.int(1,ncol(matreceivers),nvar)]

  # Same for donors
  matdonors<-matrix(values[donor],nrow=nvar)
  iddonors<-id[donor][seq.int(1,ncol(matdonors),nvar)]
  strdonors<-stratum[donor][seq.int(1,ncol(matdonors),nvar)]

  # Mahalanobis distance
  distance<-function(x) t(x)%*%cinv%*%x

  # Computation of the nearest neighbour for each receiver by stratum
  idneighbours<-idreceivers
  countneighbours<-numeric(length(idreceivers))
  for(str in unique(stratum)) {
    distmat<-apply(matreceivers[,strreceivers==str,drop=FALSE],2,function(x)
      apply(matdonors[,strdonors==str,drop=FALSE]-x,2,distance))
    if(is.null(dim(distmat))) distmat<-matrix(distmat,nrow=1)
    selecteddonors<-apply(distmat,2,which.min)
    selecteddistances<-distmat[(0:(ncol(distmat)-1))*nrow(distmat)+selecteddonors]
    orderedreceivers<-order(selecteddonors,selecteddistances)
    ordereddonors<-selecteddonors[orderedreceivers]
    auxvector<-ordereddonors-c(ordereddonors[1],ordereddonors)[-length(ordereddonors)-1]
    countneighbours[strreceivers==str][orderedreceivers]<-
      (1:length(auxvector))-cummax(as.numeric(auxvector!=0)*(0:(length(auxvector)-1)))
    if (length(selecteddonors)) idneighbours[strreceivers==str]<-iddonors[strdonors==str][selecteddonors]
    else idneighbours[strreceivers==str]<-NA
  }
  output.id<-rep(NA,length=length(id))
  output.count<-rep(NA,length=length(id))
  output.id[!donor]<-rep(idneighbours,each=nvar)
  output.count[!donor]<-rep(countneighbours,each=nvar)
  return(list(output.id,output.count))

}
