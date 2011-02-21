kSearch<-function(f0, g, arl0, Lk=0, Uk, ek=0.01, istart=0, iter=10000, print.kHistory=TRUE)
{

################################
#### CHECK INPUT PARAMETERS ####
################################

if(!missing(Lk) & !missing(Uk))
{
	if(!is.numeric(Lk) | Lk < 0)			
	{
		stop("'Lk' must be a positive real number.")
	}

	if(!is.numeric(Uk) | Uk < 0)			
	{
		stop("'Uk' must be a positive real number.")
	}

	if(Lk > Uk)
	{
		stop("'Lk' must be smaller than 'Uk'.")
	}
}
if(!is.numeric(ek) | ek <= 0)			
{
	stop("'ek' must be a positive real number.")
}
if(istart%%1 != 0 | !is.numeric(istart) | istart < 0)			
{
	stop("'istart' must be a positive integer.")
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
if(sum(g) > 1.001 | sum(g) < 0.999)
{
	stop("g does not sum to 1.")
}

######################
#### MAIN PROGRAM ####
######################

if(missing(Uk))
{
	f.temp<-(1-f0)/f0
	Uk<-.98*max(f.temp)
}
							     
lk<-Lk		
uk<-Uk			

all.results<-c()

seed<-sample(1:10000000, 1)

for(n in 1:100)
{
	m<-6
	ktemp<-c()

	for(i in 1:(m+1))
	{
		ktemp[i]<-lk+(uk-lk)*((i-1)/m)
	}

	htemp<-c()
	oc.arltemp<-c()
	arl0.actual<-c()

	for(i in 1:(m+1))
	{	
		i.kflag<-0
			
		if(n > 1)	
		{	
			for(y in 1:nrow(all.results))
			{	
				if(abs(ktemp[i]-all.results[y,1]) < ek*1e-3)
				{
					oc.arltemp[i]<-all.results[y,4]
					htemp[i]<-all.results[y,2]
					arl0.actual[i]<-all.results[y,3]
					i.kflag<-1
					break
				}
			}
		}
			
		
	if(i.kflag==0)
	{
		set.seed(seed)

		h.result<-hSearch(f0,arl0, ktemp[i], istart=istart, iter=iter, print.hHistory=FALSE)
		htemp[i]<-h.result$h
		arl0.actual[i]<-h.result$arl0.actual
		
		set.seed(seed)

		oc.arltemp[i]<-getARL(f0, g, htemp[i], ktemp[i], istart, iter, print.results=FALSE)$ARL
		
		cur.results<-cbind(k.1=ktemp[i], h.1=htemp[i], arl0.1=arl0.actual[i], arl1.1=oc.arltemp[i])
		all.results<-rbind(all.results, cur.results)	
	}	

}

temp<-cbind(oc.arltemp, arl0.actual, ktemp, htemp)
good.subset<-subset(temp, temp[,2] > .85*arl0 & temp[,2] < 1.15*arl0)

if(nrow(good.subset) > 0)
{
	r<-which.min(good.subset[,1])
	oc.winner<-good.subset[r,1]
	ic.winner<-good.subset[r,2]
	k.winner<-good.subset[r,3]
	h.winner<-good.subset[r,4]
}

else
{
	r<-which.min(temp[,1])
	oc.winner<-temp[r,1]
	ic.winner<-temp[r,2]
	k.winner<-temp[r,3]
	h.winner<-temp[r,4]	
}


int.length<-(uk-lk)/m

if(int.length < ek)
{
	break
}

if(k.winner-int.length > Lk)
{
	lk<-k.winner-int.length
}

else
{
	lk<-Lk
}

if(k.winner+int.length < Uk)
{
	uk<-k.winner+int.length
}

else
{
	uk<-Uk
}


}

###########################
#### PRINT AND RETURN  ####
###########################

k.results<-list("k"=k.winner, "h"=h.winner, "arl0.actual"=ic.winner, "arl1"=oc.winner)

all.results<-as.data.frame(all.results)
all.results.sorted<-all.results[order(all.results$k.1),]

if(print.kHistory==TRUE)
{
	print.kResults(all.results.sorted, k.results)
}

if(ic.winner > 1.15*arl0 | ic.winner < 0.85*arl0)
{
warning("Target 'arl0' not achieved for searched value of k. See the 'kSearch' documentation for more information.")
}

invisible(k.results)
}
