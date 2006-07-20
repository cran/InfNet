"resample" <-
function(x, size, rep=FALSE){
if(length(x) <= 1) { if(!missing(size) && size == 0) x[FALSE] else x} 
else sample(x, size, replace=rep)
}
