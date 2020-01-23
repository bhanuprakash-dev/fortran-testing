#include <stdio.h>

scall(com,contr)

char com[];
int contr[];

{
   int k;

   k = system(com);
   contr[0] = k;  

   return(k);
}
