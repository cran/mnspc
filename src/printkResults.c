#include<stdio.h>

void printkResults(double *printk, double *printh, double *printarl0, double *printarl1, double *kin, double *hin, double *arl1in, double *arl0in, int *nrowin)
{
double h=hin[0];
double k=kin[0];
double arl1=arl1in[0];
double arl0=arl0in[0];

char a1[]="k";
char a2[]="h";
char a3[]="arl0";
char a4[]="arl1";

int nrow=nrowin[0];

int i;

printf("\n");
printf("--------------------------\n" );
printf("     kSearch Procedure\n");
printf("--------------------------\n" );
printf("\n");
printf("Results:\n");
printf("\n");
printf("%*s = %-.4lf \n", 12, a1, k);
printf("%*s = %-.3lf \n", 12, a2, h);
printf("%*s = %-.2lf \n", 12, a3, arl0);
printf("%*s = %-.2lf \n", 12, a4, arl1);
printf("\n");
printf("Search History (sorted by 'k'):\n");
printf("\n");
printf("%*s", 12, a1);
printf("%*s", 12, a2);
printf("%*s", 12, a3);
printf("%*s \n", 12, a4);
for(i=0; i<nrow; i++)
{
	printf("%*.4lf", 12, printk[i]);
	printf("%*.3lf", 12, printh[i]);
	printf("%*.2lf", 12, printarl0[i]);
	printf("%*.2lf", 12, printarl1[i]);
	printf("\n");
}
printf("\n");
}
