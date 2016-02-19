RemoveInternal<-function(x)
{
  return(setdiff(x,c("FunDelRow","FunDelCol","FunAutoLink")))
}
