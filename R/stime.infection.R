"stime.infection" <-
function(network.edges,indexes,infectives,BETA){
#time.infection<-function(network.edges,...){
    if(dim.all(network.edges)[1]!=0){               #at least one connection
        no.erase<-matrix(sapply(network.edges,compare.vectors,infectives),dim.all(network.edges)) 
        no.erase<-rowSums(no.erase)
        eff.connections<- network.edges[no.erase==1,]  
        if(is.vector(eff.connections)==TRUE) eff.connections<-t(as.matrix(eff.connections))

        eff.infectives<-sapply(infectives, compare.vectors, eff.connections)    
        con.infectives<-rep(infectives,eff.infectives)
        eff.Beta<-BETA[indexes[con.infectives,1]]               
        infection<-newrexp(rows(eff.connections,as.row=TRUE),eff.Beta) 
    }
    else{
        infection<-NULL
        eff.connections<-NULL
    }


return(infection,eff.connections)
}
