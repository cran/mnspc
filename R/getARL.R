getARL<-function(f0, g, h, k, istart=0, iter=10000, print.results=TRUE)
{

################################
#### CHECK INPUT PARAMETERS ####
################################

if(!is.numeric(h) | h < 0)			
{
	stop("'h' must be a non-negative real number.")
}

if(!is.numeric(k) | k < 0)			
{
	stop("'k' must be a non-negative real number.")
}

if(istart%%1 != 0 | !is.numeric(istart) | istart < 0)			
{
	stop("'istart' must be a positive integer.")
}

if(iter%%1 != 0 | !is.numeric(iter) | iter <= 0)			
{
	stop("'iter' must be a positive integer.")
}

if(sum(f0) > 1.001 | sum(f0) < 0.999)
{
	stop("f0 does not sum to 1.")
}

if(sum(g) > 1.001 | sum(g) < 0.999)
{
	stop("g does not sum to 1.")
}

######################
#### MAIN PROGRAM ####
######################

arl<-0
se<-0

f0<-f0/sum(f0)
g<-g/sum(g)

RLs<-c(rep(0,iter))

out<-.C("getARL", hin=as.double(h), kin=as.double(k), f0=as.double(f0), 
g=as.double(g), iterin=as.integer(iter), arl=as.double(arl), length=as.integer(length(f0)),
istartin=as.integer(istart), sein=as.double(se), RLs=as.double(RLs))

results<-list("ARL"=out$arl, "SE"=out$se, "RLs"=out$RLs)

##########################
#### PRINT AND RETURN ####
##########################

if(print.results==TRUE)
{
	.C("printResults", as.double(out$arl), as.double(out$se))
}
invisible(results)
}
