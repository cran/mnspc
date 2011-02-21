#include<stdio.h>

void printhResults(double *arl0in, double *hin, double *hhist, double *arlhist, int *lengthin, double *Lhin, double *Uhin)
{
double h=hin[0];
double arl0=arl0in[0];
double Lh=Lhin[0];
double Uh=Uhin[0];

char a1[]="Lh";
char a2[]="Uh";
char a3[]="h";
char a4[]="arl0";

int nrow=lengthin[0];

int i;

printf("\n");
printf("--------------------------\n" );
printf("     hSearch Procedure\n");
printf("--------------------------\n" );
printf("\n");
printf("Results:\n");
printf("\n");
printf("%*s = %-.4lf \n", 12, a3, h);
printf("%*s = %-.3lf \n", 12, a4, arl0);
printf("%*s = %-.2lf \n", 12, a1, Lh);
printf("%*s = %-.2lf \n", 12, a2, Uh);
printf("\n");
printf("Search History:\n");
printf("\n");
printf("%*s", 12, a3);
printf("%*s \n", 12, a4);
for(i=0; i<nrow; i++)
{
	printf("%*.5lf", 12, hhist[i]);
	printf("%*.2lf", 12, arlhist[i]);
	printf("\n");
}
printf("\n");
}
