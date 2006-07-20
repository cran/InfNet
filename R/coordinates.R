"coordinates" <-
function(n,indexes,random=FALSE){
if(n>1){
    indexes<- indexes[order(indexes[,1]),]              #ordering the nodes wrt local network

    p.all<-sapply(X=1:n, FUN=number.elements, mat=indexes, col=1)
    radius<-matrix(c(1,1,rep(0,n-1)),n,n+1)
    radius[,n]<-radius[,n]+radius[,n+1]
    radius<-radius[,1:n]
    radius<- t(p.all)%*%radius
    t<-(0:(n-1))*(2*pi/n)
    radius1<-2*sin(t[2]/2)*((1/radius)*p.all) 
    radius2<-2*sin(t[2]/2)*((1/radius)*c(p.all[2:n],p.all[1]))
    radius2<-c(radius2[n],radius2[1 :(n-1)])
    radius<-pmin(radius1,radius2)

    Xcent<-rep(cos(t), p.all)
    Ycent<-rep(sin(t), p.all)
    if(random==TRUE){X<- runif(sum(p.all),0.8*rep(radius,p.all),rep(radius,p.all)) }    

    else{X<-rep(radius*.90,p.all)}
    Y<- sapply(X=p.all,FUN=angles)
    w<-0
     for(i in Y){w<-c(w,i)}
    Y<-w[-1]
    r<-X
    X<-r*cos(Y)+Xcent
    Y<-r*sin(Y)+Ycent
    coord<-cbind(indexes, X, Y)
}
else if(n==1){
    t<-(0:(rows(indexes)-1))*(2*pi/rows(indexes)) 
    r <- 1
    if(random==TRUE){r<-runif(rows(indexes),.7,1)}
    X<-r*cos(t)
    Y<-r*sin(t)
    coord<-cbind(indexes,X,Y)
    coord<-as.data.frame(coord)
}
return(coord)
}
