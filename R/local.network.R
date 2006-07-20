"local.network" <-
function(n,distrib, param, one.connection=TRUE){       #param is not a list

####----------------------------------------------------------------------------------------------------------------------#####

degree<-3
flag<-1

while(sum(degree)%%2==1){       #first step to assure the sequence of degrees is realizable (Newman, Stogatz and Watts, 2001)
    if(distrib=="fixed"){
        if(length(param)!=n | sum(param)%%2==1){
        print("the length of the degrees must be equal to the number of nodes and the sum of degree must to be even")
        param<-NULL
        }
    degree<-param
    }
    else if(distrib=="poisson") degree<-rpois(n,param)
    else if(distrib=="zerotpoisson") degree<- rzerotpoisson(n,param) 
    else if(distrib=="geometric"){                           #sometimes called exponential graph
        newparam<-1/(param+1)                                #param is then the expected value
        degree<- rgeom(n,newparam)
    }
    else if(distrib=="zerotgeometric"){     
        newparam<-1/(param+1)             
        degree<- rgeom(n,newparam)+1
    }
    else if (distrib=="poly.logarithmic"){  
        alpha<-param[1]
        kappa<-param[2]
        degree<-rpoly.log(n,param)
        if(sum(degree==-1)>0){     
            degree<-0
            flag<-0
        }
    }
    else if(distrib=="logarithmic"){ 
        degree<- rlog(n,param)
        if(sum(degree==-1)>0){     
            degree<-0
            flag<-0
        }        
    }
    else if(distrib=="power.law"){ 
        degree<- rpower.law(n,param)
        if(sum(degree==-1)>0){     
            degree<-0
            flag<-0
        }
    }
        
    else if (distrib=="full") degree<-rep(n-1,n)
    else if (distrib=="none") degree<-rep(0,n)
    else {
        print("incorrect distribution specification")
        degree<-NULL
    }
} #end while


####----------------------------------------------------------------------------------------------------------------------#####


nodes<-1:n
edges<-c(0,0)
other<-0
degree.left<-degree

while (length(degree.left[degree.left>0])>1 & flag==1){          #at lest two units to connect and they are not already connected

    if (other==0){

        
        #---Havel-Hakimi algorithm---###
        new.edge1<-resample(nodes[degree.left>0],1)      

        HH<-cbind(1:n,degree.left)
        HH <- HH[order(HH[,2],decreasing=TRUE),1]
        HH<-HH[HH!=new.edge1]
        
        new.edge2<-HH[1]   

        new.edge <- c(new.edge1,new.edge2)
        
        new.edge <- sort(new.edge)         
            
        #----------------------------####
        
        if(length(edges)!=2 & one.connection==TRUE){        #avoid to repeat new edges when whe have more to compare and one.connection==TRUE
            a<- which(edges==new.edge[1])                
            b<- elements.matrix(edges, new.edge[2])$rows 
            if(length(a)==0 | length(b)==0){
                a<-1                   
                b<-2
            }
            comp<- sapply(a,compare.vectors,b)
        }
        else comp<-0
    }
    else{
        other<-0
        comp<-0
    }
    
    ######----------------------------------------------------------------------------------####

    if(sum(comp)==0){           #new.edges are not repeated (or it doesn't matter if they are repeated)
                           
        count<-0              
        edges<-rbind(edges, new.edge)   
        degree.left[new.edge[1]]<-degree.left[new.edge[1]]-1
        degree.left[new.edge[2]]<-degree.left[new.edge[2]]-1
    }
    else{
        count<-count+1              #Number of times we select two conncected nodes
        if(count>=5){               #check if the all possible connections are already made but after count>=5
            paired.inf<-c(0,0,1)
            nodes.degree.left<-nodes[degree.left>0]  
            for(i in 1:(length(nodes.degree.left)-1)){
                for(j in (i+1): length(nodes.degree.left)){
                    a<- elements.matrix(edges, nodes.degree.left[i])$rows
                    b<- elements.matrix(edges, nodes.degree.left[j])$rows
                    if(length(a)==0 | length(b)==0){
                        a<-1
                        b<-2   
                    }
                    paired<- sum(sapply(a,compare.vectors,b))   
                    paired<-c(nodes.degree.left[i],nodes.degree.left[j],paired)
                    paired.inf<-rbind(paired.inf, paired)

                    } #for
            } #for
                       
            paired.inf<-paired.inf[paired.inf[,3]==0,1:2]   
            
            if(length(paired.inf)[1]>3){                    
                new<-resample(1:dim(paired.inf)[1],1)       
                new.edge<-sort(paired.inf[new,])
                other<-1                                    
            }
            else if(length(paired.inf)>0){
                new.edge<-sort(paired.inf)
                other<-1
            } 
            else flag<-0                    #no more nodes can be connected
        } 
    } 

    ######----------------------------------------------------------------------------------####

}#while("at leat two units to connect and they are not already connected")

####----------------------------------------------------------------------------------------------------------------------#####

if(rows(edges, as.row=T)>1){
    edges<- edges[-1,]
    if(rows(edges, as.row=TRUE)>1) edges<-edges[order(edges[,1],edges[,2]),]
    else if(rows(edges, as.row=TRUE)==1) edges<-t(as.matrix(edges))
    dimnames(edges)<-NULL
}
else edges<-NULL

return(edges,degree,degree.left)

}           #end of function
