"cols" <-
function(a,as.row=FALSE){
if(is.matrix(a)==TRUE | is.data.frame(a)==TRUE) cols<-dim(a)[2]
else if((is.vector(a)==TRUE | is.list(a)==TRUE) & as.row==FALSE) cols<-1
else if((is.vector(a)==TRUE | is.list(a)==TRUE) & as.row==TRUE) cols<-length(a)
else cols<-0
return(cols)
}
