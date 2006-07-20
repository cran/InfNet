"elements.matrix" <-
function(matrix,esc,cases=0,rows.mat=0,as.row=FALSE){
if(cases==0) cases<-which(matrix==esc)
if(rows.mat==0) rows.mat<-rows(matrix,as.row)
rows<-(cases-1)%% rows.mat +1        
columns<-(cases-1)%/% rows.mat +1       
return(rows,columns)
}
