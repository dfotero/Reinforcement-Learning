pageRank<-function(probs,c,iter)
{
  z<-matrix(1/nrow(probs),nrow=nrow(probs),ncol=1)
  
  for(j in 1:iter)
  {
    zN<-matrix(0,nrow=nrow(probs),ncol=1)
    for(i in 1:nrow(probs))
    {
      zN[i]<-z[i]+((1-c)/nrow(probs)+c*probs[,i]%*%z)/j
    }
    z<-zN/sum(zN)

  }
  z<-z/sum(z)
  return(z)
}
