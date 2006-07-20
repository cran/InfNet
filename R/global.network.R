"global.network" <-
function(n,p,distrib,param=0,distrib2="full",param2=0){

if(n==1){                           #when we have only one local network
    global.edges<-local.network(p,distrib,param=unlist(param))$edges #at most one connection among two indiv. within a local net.
    among.locals<-0
}
else{
    if (length(p)==1) p<-p*c(rep(1,n))
    if (length(distrib)==1) distrib<-rep(distrib,n)             #Copy the same distribution for all
    if (length(param)==1)param<-rep(param,n)            #for list type variables is ok to use the function rep
    if (length(distrib)!=n | length(param)!=n ) {
        print("Any, the length of the vectors distrib, the param or the degrees for each node is incorrect")
        p<-distrib<-NULL
    }

    csum.p<-cumsum(c(0,p))                  #the cumulative sum of p
    global.edges<-c(0,0)                    #Is still a matrix but only contain the effective connections
    #------------------------------The contact within local networks-----------------------------
    for (i in 1:n){
        parameter.loc<-unlist(param[i])  
        local<- local.network(p[i],distrib[i],parameter.loc)$edges  
        global.edges<-rbind(global.edges, local+csum.p[i])      
    }

    #------------------------------The contacts between local networks------------------------
    among.locals<-local.network(n,distrib2,param2, one.connection=FALSE)$edges  #allow more than one connection among local net.
    # Selecting the individuals to connect (we can select the same individual):
    sam.position<-c(0,0)
    for(i in 1:n){
        n.conn<- sum(among.locals==i)       #the desired number of times the network i is connected to some other
        if (n.conn>0){
            if((csum.p[i]+1)<=csum.p[i+1]){         #there is at least one individual in the network i
                sam<- resample((csum.p[i]+1):csum.p[i+1], n.conn, TRUE)
                rows.pairs<- elements.matrix(among.locals, i, as.row=TRUE)$rows
                sam<-cbind(sam, rows.pairs) 
            }
            else sam<-NULL

            sam.position<-rbind(sam.position,sam)
        }
    }
    if(length(sam.position)[1]>2){          
        for(i in sam.position[,2]){
            if (sum(sam.position[,2]==i)<2) sam.position<-rbind(sam.position,c(-1,i)) 
        }
        sam.position<-sam.position[order(sam.position[,2],sam.position[,1]),]   
                                    #I am ordering wrt row and ID (order important)
        sam.position<-sam.position[-c(1,2),]                #to remove the first two rows
        odd<-(1:ceiling(rows(sam.position)/2))*2-1              #odd indexes
        even<-(1:floor(rows(sam.position)/2))*2             #even indexes
        if(rows(sam.position)>2){
            sam.position<-cbind(sam.position[odd,],sam.position[even,])
            sam.position<-sam.position[sam.position[,1]>0,c(1,3)]
        }
        else if(rows(sam.position)==2) sam.position<-c(sam.position[1,1],sam.position[2,1])
    }   
    else sam.position<-NULL
    #----------------------------The final connection information-------------------------------
    global.edges<-rbind(global.edges,sam.position)
    global.edges<-global.edges[-1,]
    dimnames(global.edges)<-NULL               
    global.edges<-order.edges(global.edges,ord.col=TRUE)
}
return(global.edges,among.locals)
}
