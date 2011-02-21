hSearch<-function(f0, arl0, k, Lh, Uh, eh1=0.5, eh2=0.001, istart=0, iter=10000, print.hHistory=TRUE)
{

################################
#### CHECK INPUT PARAMETERS ####
################################

if(!missing(Lh) & !missing(Uh))
{
	if(!is.numeric(Lh) | Lh < 0)			
	{
		stop("'Lh' must be a non-negative real number.")
	}

	if(!is.numeric(Uh) | Uh <= 0)			
	{
		stop("'Uh' must be a non-negative real number.")
	}

	if(Lh > Uh)
	{
		stop("'Lh' must be smaller than 'Uh'.")
	}
}
if(!is.numeric(eh1) | eh1 <= 0)			
{
	stop("'eh1' must be a positive real number.")
}
if(!is.numeric(eh2) | eh2 <= 0)			
{
	stop("'eh2' must be a positive real number.")
}
if(istart%%1 != 0 | !is.numeric(istart) | istart < 0)			
{
	stop("'istart' must be a non-negative integer.")
}
if(iter%%1 != 0 | !is.numeric(iter) | iter <= 0)			
{
	stop("'iter' must be a positive integer.")
}
if(!is.numeric(arl0) | arl0 <= 0)			
{
	stop("'arl0' must be a positive real number.")
}
if(sum(f0) > 1.001 | sum(f0) < 0.999)
{
	stop("f0 does not sum to 1.")
}

#######################################
#### DETERMINE Lh, Uh IF NECESSARY ####
#######################################

seed<-sample(1:10000000, 1)

if(missing(Lh) | missing(Uh))
{
	temp<-hBound(k, f0, arl0, istart=istart)

	if(missing(Uh))
	{
		Uh<-temp$h.high
	}
	if(missing(Lh))
	{
		Lh<-temp$h.low
	}

	if(Lh > Uh)
	{
		stop("'Lh' must be smaller than 'Uh'.")
	}

}

######################
#### MAIN PROGRAM ####
######################

h<-c()
arl.temp<-c()

lh<-Lh
uh<-Uh

for(i in 1:1000)
{

	h[i]<-(lh+uh)/2

	set.seed(seed)

	arl.temp[i]<-getARL(f0, f0, h[i], k, istart, iter, print.results=FALSE)$ARL
		
	if(abs(arl.temp[i]-arl0)<eh1)
	{
		arl.final<-arl.temp[i]
		h.final<-h[i]
		break
	}

	if(i > 1)
	{
		if(abs(h[i]-h[i-1])<eh2)
		{
			arl.final<-arl.temp[i]
			h.final<-h[i]
			break
		}
	}

	if(arl.temp[i] < arl0)
	{
		lh<-h[i]	
	}

	if(arl.temp[i] > arl0)
	{
		uh<-h[i]
	}

}

#############################
### PRINT SEARCH HISTORY ####
#############################

results.length<-length(h)

if(print.hHistory==TRUE)
{
	.C("printhResults", arl0in=as.double(arl.final), hin=as.double(h.final), hhist=as.double(h), arlhist=as.double(arl.temp), lengthin=as.integer(results.length), Lhin=as.double(Lh), Uhin=as.double(Uh))
}

h.results<-list("h"=h.final, "arl0.actual"=arl.final)
invisible(h.results)
}





