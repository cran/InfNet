"sformat.output" <-
function(a,random=FALSE){
border.width<-0               
last.time<-sum(a$time.hist)

#**************prepare nodes********************
nodes<-a$nodes.hist
nodes<-nodes[order(nodes[,1],nodes[,3]),]         

s<-sapply(1:max(nodes[,1]),compare.vectors,nodes[,1]) 
EndTime <-rep(last.time, nrow(nodes))               
cut.time<-nodes[-1,3]                      
EndTime [-cumsum(s)]<-cut.time[-cumsum(s[-length(s)])]  

nodes<-cbind(nodes[,1:3],EndTime,nodes[,4 :5], border.width,nodes[,5])
nodes[nodes[,6]==0, c(6,8)]<-"LightGray"  
nodes[nodes[,6]==1, c(6,8)]<-"blue"       
nodes[nodes[,6]==2, c(6,8)]<-"red"        

nodes<-merge(nodes, coordinates(a$n,a$indexes,random),by="NodeId")
nodes<-cbind(nodes[,1:2],nodes$X,nodes$Y,nodes[,3:6])


names(nodes)<- c("NodeId", "Label", "X","Y","StartTime","EndTime", "NodeSize", "ColorName")

#**************prepare edges********************

edges<-a$edges.hist[order(a$edges.hist[,1],a$edges.hist[,2], a$edges.hist[,3]),]    #order: "FromId" - "ToId" -  "StartTime"


edges<-grouping(edges, paste(edges[,1],edges[,2]))          

s<-sapply(1:max(edges[,6]),compare.vectors, edges[,6])      
EndTime <-rep(last.time, nrow(edges))             
cut.time<- edges [-1,3]                    
EndTime [-cumsum(s)]<-cut.time[-cumsum(s[-length(s)])]  

edges<-cbind(edges[,1:3],EndTime,5,edges[,5])
edges<-edges[edges[,6]!=0,]         
edges[edges[,6]==1,6]<-"black"      

names(edges)<-c("FromId","ToId", "StartTime", "EndTime","ArcWidth", "ColorName")


#**************preparing the SoNIA's file fomat********************

edges.for<-rbind(names(edges),edges)
edges.for <- cbind(edges.for,"")
edges.for <- cbind(edges.for,"")
names(edges.for)<-NULL
one<-rbind(nodes,edges.for)

return(one)

}
