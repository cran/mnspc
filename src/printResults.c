#include<stdio.h>

void printResults(double *arlin, double *SEin)
{
double arl=arlin[0];
double se=SEin[0];


char a1[]="ARL";
char a2[]="SE";


printf("\n");
printf("--------------------------\n" );
printf("     getARL Procedure\n");
printf("--------------------------\n" );
printf("\n");
printf("Results:\n");
printf("\n");
printf("%*s = %-.4lf \n", 12, a1, arl);
printf("%*s = %-.3lf \n", 12, a2, se);
printf("\n");
}
