"rzerotpoisson" <-
function(n,p){
u<-runif(n)
cdf1 <- cumsum(dpois(0:30,p))               
cdf2 <- (cdf1-exp(-p))/(1-exp(-p))          
res <- 0                                    
for(i in 1:n){
res2<-max(which(cdf2<u[i]))
res<-c(res,res2)
}
return(res[-1])
}
