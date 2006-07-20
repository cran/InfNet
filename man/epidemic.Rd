\name{epidemic}
\alias{epidemic}

\title{ Directly executable for the epidemic simulation}
\description{
This function is the one to be called in R by the user to execute the simulation.
}
\usage{
epidemic(n, p, obs.time, ini.inf, distrib, param, distrib2 = "full", param2 = 0, BETA, GAMMA, ETA, SUSC, filename, sonia = FALSE, random = FALSE, filename.son)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ Number of local networks}
  \item{p}{ Number of individuals in each local network (vector or scalar if all entries are equal) }
  \item{obs.time}{ The time period to observe}
  \item{ini.inf}{ Initial infectives in each local network (vector or scalar if all entries are equal)}
  \item{distrib}{ Name of the distribution for the nodes' degree in each local network (vector of characters or only one string if all entries are equal)}
  \item{param}{ parameter distribution for each local network. It must be a list in wich each element is a number of vector. The elements can be: "fixed","poisson","zerotpoisson","geometric","zerotgeometric","poly.logarithmic","logarithmic","power.law","full","none"}
  \item{distrib2}{Name of the distribution for the connection among local networks. The option "fixed" has a vector of parameters that correspond to the nodes' degree. The"poly.logarithmic" distribution has a vector of legth two as parameter (alpha, kappa) }
  \item{param2}{ Parameter distribution for distrib2 (scalar or vector). The entries can be: "fixed","poisson","zerotpoisson","geometric","zerotgeometric","poly.logarithmic","logarithmic","power.law","full","none"}
  \item{BETA}{ Infection rate among connected nodes}
  \item{GAMMA}{ Removal rate of infective nodes}
  \item{ETA}{ Immigration rate of infectives (ETA=0 if not immigration)}
  \item{SUSC}{ Immigration rate of susceptibles (SUS=0 if not immigration)}
  \item{filename}{ File where to save the simulation result (R format)}
  \item{sonia}{ Sonia=T to generate the SoNIA file}
  \item{random}{random=T if the radious length are not equal within each local network }
  \item{filename.son}{Filename of the SoNIA file}
}

%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}

\value{

%  ~Describe the value returned
%  If it is a LIST, use

  \item{n}{Number of local networks}
  \item{nodes.hist}{Matrix with the status of the nodes during the observed period}
  \item{edges.hist}{Matrix of connection and their status during the observed period}
  \item{p}{Number of nodes in each local network}
  \item{p.hist}{Number of non removed nodes in each local network during the observed period}
  \item{suscp.hist}{Number or susceptible individual in each local network}
  \item{infp.hist}{Number of infective individual in each local network}
  \item{remp.hist}{Number of removed individula from each local network}
  \item{network.edges}{Non occupied nodes at the end time}
  \item{time.hist}{events time}
  \item{indexes}{local network and final status of all nodes}
}


%\references{ ~put references to the literature/web site here ~ }

\author{ Lilia Ramirez Ramirez and Mary Thompson}
\note{ FirsT Version }

% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }

\examples{
a<-epidemic(10,50,5,c(1,2,rep(1,8)),distrib="poisson",param=as.list(2,rep(1,7),rep(2.3,2)),distrib2="full",BETA=0.8,GAMMA=0.5,filename="temp",sonia=TRUE,filename.son="temp.son")

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(n,p,obs.time,ini.inf,distrib,param,distrib2="full",param2=0,BETA,GAMMA,ETA,SUSC,filename, sonia=FALSE, random=FALSE,filename.son){
result<-epidemic.sim(n,p,obs.time, ini.inf,distrib,param,distrib2,param2,BETA,GAMMA,ETA,SUSC)
save(result, file=filename)

if(sonia==TRUE){        
    print("inicia formato")
    for.sonia<-format.output(result,random)  
    print("done")
    write.table(for.sonia, file=filename.son , row.names=F,quote=F, sep="\t")
  }
return(result)
  }
}
\keyword{misc}
