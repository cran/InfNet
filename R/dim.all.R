"dim.all" <-
function(a, as.row=FALSE){
if(is.matrix(a)==TRUE | is.data.frame(a)==TRUE) dim.all<-dim(a)
else if((is.vector(a)==TRUE | is.list(a)==TRUE) & as.row==FALSE) dim.all<-c(1,length(a))
else if((is.vector(a)==TRUE | is.list(a)==TRUE) & as.row==TRUE) dim.all<-c(length(a),1)
else dim.all<-c(0,0)
return(dim.all)
}
