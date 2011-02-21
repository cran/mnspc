#include<R.h>
#include<Rmath.h>

void getARL(double *hin, double *kin, double *f0, double *f1, int *iterin, double *arl, 
int *length, int *istartin, double *sein, double *RLs)
{	
	double h=hin[0];			/* Initialization statements*/
	double k=kin[0];
	int iter=iterin[0]; 		
	int p=length[0];
	int istart=istartin[0];
	int maxRL=25000;

	double sn_exp[p][2];
	double sn_obs[p][2];
	int g[p];

	double cumRL=0.0;
	double cumRL2=0.0;
	
	double un, cn;
	int j, cur, prev, temp, RL, RLadj;
	int i=1; int icount=0; int noconverge=0;
	


	while(i <= iter)		/* 'iter' run lengths are computed to get ARL */
	{

		cur=1;
		prev=0;

		for(j=0; j < p; j++)
		{
			sn_exp[j][prev]=0.0;
			sn_obs[j][prev]=0.0;
		} 

		while(un <= h)
		{

			GetRNGstate();
			if((int)RL >= istart)
			{
				rmultinom(1,f1,p,g);
			}

			if((int)RL < istart)
			{
				rmultinom(1,f0,p,g);
			}
			PutRNGstate();


		cn=0.0;

		for(j = 0; j < p; j++)
		{
			cn=cn+pow((sn_obs[j][prev]-sn_exp[j][prev] + (double)g[j]-f0[j]),2) \
				/(sn_exp[j][prev]+f0[j]);
		}
			
			if(cn <= k)
			{
				for(j=0; j < p; j++)
				{
					sn_exp[j][cur]=0.0;
					sn_obs[j][cur]=0.0;
				} 
			}

			
			else
			{
				for(j=0; j < p; j++)
				{
					sn_exp[j][cur]=(sn_exp[j][prev] + f0[j])*((cn-k)/cn);
					sn_obs[j][cur]=(sn_obs[j][prev] + (double)g[j])*((cn-k)/cn);
				} 	
				un=0.0;
				un=cn-k;	
			}

			RL++;

			temp=prev;		
			prev=cur;
			cur=temp;

			if(RL > maxRL)
			{
				break;
			}

		}
		
		if((int)RL >= istart)
		{
			RLadj=RL-istart;		
			cumRL=cumRL+(double)RLadj;
			cumRL2=cumRL2+(double)pow(RLadj,2);
			RLs[i-1]=RLadj;
			i++;
		}
		if((int)RL < istart)
		{
			icount++;
		}	

		if(icount > 500000 && i < 20)
		{
			noconverge=1;
			break;
		}
	
		RL=0;
		un=0;
	}

	if(noconverge==1)
	{
		arl[0]=0;
	}

	else
	{
		arl[0]=cumRL/(double)iter;
		sein[0]=sqrt((cumRL2/(double)iter-pow(arl[0],2))/((double)iter));			
	}

}
