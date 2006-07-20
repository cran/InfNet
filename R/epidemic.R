"epidemic" <-
function(n,p,obs.time,ini.inf,distrib,param,distrib2="full",param2=0,BETA,GAMMA,ETA=0,SUSC=0,filename, sonia=FALSE, random=FALSE,filename.son){
result<-epidemic.sim(n,p,obs.time, ini.inf,distrib,param,distrib2,param2,BETA,GAMMA,ETA,SUSC)
save(result, file=filename)

if(sonia==TRUE){        
    print("formatting data for SoNIA")  #llrr
    for.sonia<-sformat.output(result,random)  
    print("done")
    write.table(for.sonia, file=filename.son , row.names=FALSE,quote=FALSE, sep="\t")
}
return(result)
}
