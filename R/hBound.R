hBound<-function(k, f0, arl0, istart=0)
{
h.start<-0
increment<-.20

h.temp<-c()
h.temp[1]<-h.start

arl.temp<-c()

for(i in 1:2500)
{
	
	if(istart==0)
	{
		arl.temp[i]<-getARL(f0, f0, h.temp[i], k, istart=0, iter=2500, print.results=FALSE)$ARL
	}

   	else
   	{
		arl.temp[i]<-getARL(f0, f0, h.temp[i], k, istart=istart, iter=1500, print.results=FALSE)$ARL
   	}

   	if(i > 1)
	{
		if(arl.temp[i] > 1.25*arl0)
		{
			break
		}		
	}
   	h.temp[i+1]<-h.temp[i]+increment		
}

h.high<-max(h.temp)

temp.results<-cbind(h.temp, arl.temp)

lower.subset<-subset(temp.results, temp.results[,2] < .75*arl0)
h.low<-max(lower.subset[,1])

out<-list("h.low"=h.low, "h.high"=h.high)
return(out)
}
