"grouping" <-
function(vec.mat, groups){
last.col<- ncol(vec.mat)+2 
r<- dim(rowsum(vec.mat[,1], groups))
vec.mat<-cbind(vec.mat,0,groups)
for(i in 1: r[1]){                          
lvec.mat<-vec.mat[vec.mat[,last.col-1]==0,last.col]
lvec.mat<-lvec.mat[1]
vec.mat[vec.mat[,last.col]==lvec.mat,last.col-1]<-i
}
vec.mat<-vec.mat[,-last.col]
return(vec.mat)

#return(matrix(as.real(vec.mat),dim(vec.mat)[1],last.col-1))
}
