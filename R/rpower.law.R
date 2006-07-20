"rpower.law" <-
function(n,param){
sim<- rep(-1,n)
if(length(param)!=1){ 
    print("the power law parameter dimension is wrong (it must be of length 1)or the parameter value is incorrect")
    param<-NULL
}
else{
    unif<-runif(n)
    k<-1:1000
    pdf<-k^(-param)
    constant<-1/sum(pdf)
    pdf<-constant*c(0,pdf)
    cdf<-cumsum(pdf)
    for(i in 1:700){ sim[cdf[i]<unif & unif<=cdf[i+1]]<-i }
}
return(sim)
}
