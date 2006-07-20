"order.edges" <-
function(edges,ord.col=TRUE){

if(is.matrix(edges)==TRUE){
    if(ord.col==TRUE){
        keep<-edges[,1]<edges[,2]
        edges[keep==0,]<-cbind(edges[keep==0,2],edges[keep==0,1])
    }
    edges<-edges[order(edges[,1],edges[,2]),]
    return(edges)
}
else edges<-sort(edges)
}
