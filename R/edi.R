edi<-function(datatype, data){
  if (datatype=="daily") {
    k=365
  } else {
    k=12
  }
  data<-data[[1]]
  p1<-data
  #p1<-c(2,3,2,1,2)
  p<-rev(p1)
  l=length(p)
  sm<-rep(0,l)
  EP<-rep(0,l-k+1)
  for(j in 1:(l-k+1)){
    n=0
    for(i in j:(j+k-1)){
      s=i-j
      for(m in i:(j+k-1)){
        if(s<m){s=s+1}
        n<-n+1
        sm[n]<-p[i]*(1/s)
      }
    }
    EP[j]<-sum(sm[1:n])
  }
  EDI<-scale(EP)
  EDI<-EDI[,1]
  EDI<-data.frame(EDI)
  return("$EDI-Values"=EDI)
}
