"rlog" <-
function(n,param){ 
sim<- rep(-1,n)
if(length(param)!=1){ 
    print("the Logarithmic parameter dimension is wrong")
    param<-NULL
}
else{
    unif<-runif(n)
    k<-1:500
    pdf<-k^(-1)*exp(-k/param)/(-log(1-exp(-1/param)))
    cdf<-c(0,cumsum(pdf))
    for(i in 1:500){ sim[cdf[i]<unif & unif<=cdf[i+1]]<-i }
}
return(sim)
}
