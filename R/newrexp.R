"newrexp" <-
function(n,p){
if (n!=length(p)) print("dimensions do to match in newrexp")
p.tem<-p
p.tem[p==0]<-1
if (n>0){
a<-rexp(n,p.tem)
a[p==0]<-1000000
}
else a<- 2000000    # case when n<=0
return(a)
}
