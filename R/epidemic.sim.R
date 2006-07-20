"epidemic.sim" <-
function(n,p,obs.time=0,ini.inf,distrib,param,distrib2,param2,BETA,GAMMA,ETA,SUSC){

o.trans<-o.time<-FALSE
if (obs.time!=0){
    cat("Period to observe: form 0 to ",obs.time,"\n")
    o.time<-TRUE
    limit<-obs.time
    counter<-0
}
else {
    print("You must select the period to observe")
    counter<-10000000
}

if(n>1){                    
    if (length(p)==1) p<-rep(p,n)
    if (length(ini.inf)==1) ini.inf<-rep(ini.inf,n)
    if (length(distrib)==1) distrib<-rep(distrib,n)     #I do need this here
    if (length(param)==1) param<-rep(param,n)       #I do need this here
    if (length(BETA)==1) BETA<-rep(BETA,n)
    if (length(GAMMA)==1) GAMMA<-rep(GAMMA,n)
    if (length(ETA)==1) ETA<- rep(ETA,n)
    if (length(SUSC)==1) SUSC<-rep(SUSC,n)
}

#******************Initial and constant values***********************
p.hist<-p                   
susp.hist<-susp<-p-ini.inf  
infp.hist<-infp<-ini.inf    
remp.hist<-remp<-rep(0,n)   
time.hist<-0                

    size<-5                     
    inv<-1                      
    arc.width<-5                
    arc.widthinv<-1.5           
    arc.color<-1                #0-invisible, 1-active


#***************INDEXES*****************
indexes<-cbind(rep(1:n,p), rep(1, sum(p)),1:sum(p)) #will help to follow all the changes  
                        #local network, status(0-inactive 1-healty 2-infective), ID
#selection of infectives:
infectives<-0       
for (j in 1:n){
    local.nodes<-indexes[indexes[,1]==j,3]
    if(ini.inf[j]>0) infectives<-c(infectives,resample(local.nodes,ini.inf[j]))
    
}
infectives<- sort(infectives)[2:(sum(ini.inf)+1)]           
indexes[infectives,2]<-2                   

indexes<-as.data.frame(indexes)
names(indexes)<-c("LocalNetwork","Status","NodeId")
#***************NODES*****************
nodes.hist<-cbind(1:sum(p),0,0,rep(size, sum(p)),indexes[,2])       #****NodeId, Label, StartTime, NodeSize, ColorName****
                                #color: 0-invisible, 1-blue-no.infective and 2-red-infective:

nodes.hist<-as.data.frame(nodes.hist)
names(nodes.hist)<-c("NodeId", "Label", "StartTime", "NodeSize", "ColorName")
#***************EDGES*****************

network.edges <- global.network(n,p,distrib,param,distrib2,param2)$global.edges

     
            #creation of the network. It is a matrix of two columns that indicate which member (by ID) are connected

#print("network.edges at the beginning"); print(network.edges)
if (is.null(network.edges)==FALSE){ 
    edges.hist<-cbind(network.edges,0, arc.width, arc.color)   
                                                                #****FromId, ToId, StartTime,  ArcWidth, ColorName****
    edges.hist<-as.data.frame(edges.hist)
    names(edges.hist)<-c("FromId", "ToId", "StartTime", "ArcWidth", "ColorName")
}
else edges.hist<-NULL
#*****************************************



no.more.events<-FALSE               #flag tha turns true when no change of network and no more events are possible


while(counter<=limit & no.more.events==FALSE){

        infectives<-indexes[indexes[,2]==2,3]           #the Id of active infectives
        susceptibles<-indexes[indexes[,2]==1,3]         #the Id of active susceptibles
        n.infectives<-length(infectives)
        n.susceptibles<- length(susceptibles)
        if(is.vector(network.edges)==TRUE) network.edges<-t(as.matrix(network.edges))

        #--------Time Events--------
        infection<-stime.infection(network.edges,indexes,infectives,BETA)$infection
        removal<-newrexp(length(infectives),GAMMA[indexes[infectives,1]])
        imm.inf<-newrexp(n, ETA)                    
        imm.sus<-newrexp(n,SUSC)                    
        times.events<-c(infection, removal, imm.inf, imm.sus)
        times.events<-times.events[times.events<2000000]
        time.af<-min(times.events)    
        event.time<-time.af+sum(time.hist) 
                                                    #Includes transmission, migrations, removals and change of network because of time
        #---------------------------
        
        if(o.trans==TRUE) counter<-counter+1
        else if(o.time==TRUE) counter<-event.time

        cat("| time = ", counter, "\t")
        eff.connections<-stime.infection(network.edges,indexes,infectives,BETA)$eff.connections
        
        if (time.af>=1000000) {
                print("no new event is possible")
                no.more.events<-TRUE
        }
        else{
            time.hist<-cbind(time.hist, time.af)            #the historical times between consecutive events
            
                who.is<-which(times.events==time.af)                
                criteria<-c(0, rows(eff.connections,as.row=TRUE), rows(eff.connections,as.row=T)+n.infectives, rows(eff.connections,as.row=T)+n.infectives+n, rows(eff.connections,as.row=T)+n.infectives+(2*n))

                if (criteria[1]<who.is & who.is<=criteria[2]) {
                    cat("*** transmission \n")
                    who.trans<-sapply(eff.connections[who.is,],compare.vectors, infectives)
                    who.inf<-eff.connections[who.is,who.trans>0]            #the infective individual who transmit the infection
                    who.is.inf<-eff.connections[who.is,who.trans==0]        #the susceptible that becomes infected
                    indexes[who.is.inf,2]<-2                        
    
                    nodes.hist<-rbind(nodes.hist, c(who.inf, max(nodes.hist[nodes.hist[,1]==who.inf,2])+1, event.time, size, 2), c(who.is.inf,max(nodes.hist[nodes.hist[,1]==who.is.inf,2]), event.time, size, 2))
                    susp[indexes[who.is.inf,1]]<-susp[indexes[who.is.inf,1]]-1 
                    infp[indexes[who.is.inf,1]]<-infp[indexes[who.is.inf,1]]+1 
                }
                else if (criteria[2]<who.is & who.is<=criteria[3]){ 
                    cat("*** removal \n")
                    removed<-infectives[who.is-criteria[2]]                 
                    indexes[removed,2]<-0                                   
                    p[indexes[removed,1]]=p[indexes[removed,1]]-1           
                    infp[indexes[removed,1]]<-infp[indexes[removed,1]]-1    
                    remp[indexes[removed,1]]<-remp[indexes[removed,1]]+1    
                    nodes.hist<-rbind(nodes.hist, c(removed, max(nodes.hist[nodes.hist[,1]==removed,2]),event.time, inv,0))
                    if((sum(dim(network.edges)>0)==2)==TRUE){      #network.edges is a non empty matrix
                        to.remove<-rowSums(network.edges==removed)
                        edges.removed<-network.edges[to.remove==1,]    
                        edges.hist<-concatenate(edges.hist, edges.removed,c(event.time,arc.widthinv,0))
                        network.edges <-network.edges[to.remove==0,]   
                    }

                }
                else if (criteria[3] <who.is & who.is<=criteria[4]){      
                    cat("*** infective arrival \n")
                    chlocal.net<- who.is- criteria[3]           
                    p[chlocal.net]<- p[chlocal.net]+1           
                    infp[chlocal.net]<-infp[chlocal.net]+1      
                    new.id<-rows(indexes)+1
                    nodes.hist<-rbind(nodes.hist, c(new.id,0,event.time,size,2))
                    indexes<-rbind(indexes, c(chlocal.net, 2, new.id))
    
                    new.network.edges<-new.edges(new.id, distrib[chlocal.net],param[chlocal.net], indexes[indexes[,1]==chlocal.net & indexes[,2]==1,3])
                    edges.hist<-concatenate(edges.hist,new.network.edges,c(event.time,arc.width,1))
                    network.edges<-rbind(network.edges,new.network.edges)
                }
                else if (criteria[4]<who.is & who.is<= criteria[5]) {      
                    cat("*** susceptible arrival \n")
                    chlocal.net<- who.is- criteria[4]           
                    p[chlocal.net]<- p[chlocal.net]+1               
                    susp[chlocal.net]<-susp[chlocal.net]+1      
                    new.id<-rows(indexes)+1 
                    nodes.hist<- rbind(nodes.hist, c(new.id,0,event.time,size,1))
                    indexes<-rbind(indexes, c(chlocal.net, 1,new.id))
    
                    new.network.edges<-new.edges(new.id, distrib[chlocal.net],param[chlocal.net], indexes[indexes[,1]==chlocal.net & indexes[,2]==1,3])
                    edges.hist<-concatenate(edges.hist,new.network.edges,c(event.time,arc.width,1))
                    network.edges<-rbind(network.edges,new.network.edges)
                }
                else{
                    print("there is something wrong with who.is and criteria")
                    cat("criteria= ", criteria, "\n")
                    cat("whois= ", who.is, "\n")
                }
        
            p.hist<-rbind(p.hist,p)
            susp.hist<-rbind(susp.hist,susp)
            infp.hist<-rbind(infp.hist,infp)
            remp.hist<-rbind(remp.hist,remp)
        }               #end case when time.af<1000000


}                       #end while

dimnames(network.edges)<-NULL               #to eliminate headers
time.hist <- as.vector(time.hist)
dimnames(p.hist) <- NULL
dimnames(susp.hist) <- NULL
dimnames(infp.hist) <- NULL
dimnames(remp.hist) <- NULL

return(n,nodes.hist, edges.hist, p, p.hist,susp.hist,infp.hist,remp.hist,network.edges,time.hist,indexes)     #values the function returns

}                       #end function
