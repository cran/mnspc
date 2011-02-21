catData<-function(indat, table=TRUE)	
{			               	   
	indat<-as.data.frame(indat)
	indat.temp<-as.matrix(indat)	  	

	n<-ncol(indat.temp)			
	m<-nrow(indat.temp)			

	incomplete.rows<-c()

	for(i in 1:m)				
	{
		for(j in 1:n)
		{
			if(is.na(indat.temp[i,j]))
			{
				incomplete.rows<-cbind(incomplete.rows, i)	
			}	
			if(!is.numeric(indat.temp[i,j]))
			{
				stop("Data must be numeric.")
			}
		}
	}


	if(length(incomplete.rows)!=0)
	{
		warning("Rows with missing data were removed.")
		indat<-indat.temp[-incomplete.rows,]

		n<-ncol(indat)			
		m<-nrow(indat)
	}
			
	med<-c()

	cat.dat<-matrix(nrow=m, ncol=n)

	for(j in 1:n)
	{
		med[j]<-median(indat[,j])
	}


	for(j in 1:n)	
	{
		for(i in 1:m)
		{
			if(indat[i,j] > med[j])
			{
				cat.dat[i,j]<-1
			}
			else
			{
				cat.dat[i,j]<-0
			}
		}
	}	

	if(table==TRUE)
	{
		cat.dat<-as.data.frame(cat.dat)
	        colnames(cat.dat)<-colnames(indat)
		rownames(cat.dat)<-c(1:m)
		return(table(cat.dat))
	}		
		
	else
	{
		results<-as.data.frame(cat.dat)
		colnames(results)<-colnames(indat)
		return(results)
	}

}










