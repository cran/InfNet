"concatenate" <-
function(edges.hist, vec.mat, constant=NULL){ 
if((sum(dim.all(vec.mat)>0)==2)==TRUE){                #vec.mat is not empty (is.null is not good enought)
    if (is.vector(vec.mat)==TRUE) vec.mat<-t(as.matrix(vec.mat))   #vector (scalars are vectors) is now it is a matrix with one row 
vec.mat<-cbind(vec.mat,matrix(rep(constant,nrow(vec.mat)),nrow(vec.mat),length(constant),byrow=TRUE))
vec.mat<-as.data.frame(vec.mat)
names(vec.mat)<-names(edges.hist)           #to be able to use rbind next
edges.hist<-rbind(edges.hist, vec.mat)
}   
return(edges.hist)
}
