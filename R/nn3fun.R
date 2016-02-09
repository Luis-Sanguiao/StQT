nn3fun<-function(id,stratum,values,donor,nvar) {

  # Calculamos la inversa de la covarianza

  cinv<-solve(var(t(matrix(values,nrow=nvar)),use="na.or.complete"))

  # Se crea la matriz de perceptores junto con el vector de identificación y el de estratos
  matreceivers<-matrix(values[!donor],nrow=nvar)
  idreceivers<-id[!donor][seq.int(1,ncol(matreceivers),nvar)]
  strreceivers<-stratum[!donor][seq.int(1,ncol(matreceivers),nvar)]

  # Lo mismo para los donantes
  matdonors<-matrix(values[donor],nrow=nvar)
  iddonors<-id[donor][seq.int(1,ncol(matdonors),nvar)]
  strdonors<-stratum[donor][seq.int(1,ncol(matdonors),nvar)]

  distance<-function(x) t(x)%*%cinv%*%x

  for(str in unique(stratum)) {
    distmat<-apply(matreceivers[strreceivers==str,],2,function(x)
      apply(donors[strdonors==str]-x,2,distance))
    selecteddonors<-apply(distmat,2,which.min)

  }



}
