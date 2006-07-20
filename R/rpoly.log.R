"rpoly.log" <-
function(n,param){
sim<- rep(-1,n)
if(length(param)!=2){ 
    print("the poly logarithmic parameter dimension is wrong")
    alpha<-kappa<-NULL
}
else{
    alpha<- param[1]
    kappa<- param[2]
    unif<-runif(n)
    k<-1:1000
    pdf<-k^(-alpha)*exp(-k/kappa)
    constant<-1/sum(pdf)
    pdf<-constant*c(0,pdf) 
    cdf<-cumsum(pdf)

    for(i in 1:500){ sim[cdf[i]<unif & unif<=cdf[i+1]]<-i }
}
return(sim)
}
