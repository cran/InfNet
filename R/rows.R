"rows" <-
function(a,as.row=FALSE){
if(is.matrix(a)==TRUE | is.data.frame(a)==TRUE) rows<-dim(a)[1]
else if((is.vector(a)==TRUE | is.list(a)==TRUE) & as.row==FALSE) rows<-length(a)
else if((is.vector(a)==TRUE | is.list(a)==TRUE) & as.row==TRUE) rows<-1
else rows<-0
return(rows)
}
