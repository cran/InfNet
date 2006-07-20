"number.elements" <-
function(mat,col, x){
a<-length(mat[mat[,col]==x,col])
return(a)
}
