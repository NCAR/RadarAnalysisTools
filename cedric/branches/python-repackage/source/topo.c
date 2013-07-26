#include <stdio.h>
#include <stdlib.h>  /* exit() */

#include "cedric.h"

#define RECLENGTH 59097

/* reads in a record from the ascii topo database */
void FORTRAN_NAME(rdtoprec)(IREC, CBUF)
     int *IREC, CBUF[];
{
  static int first = 1;
  static FILE *fp;
  int rval;
  static char filename[] = "topo.dat";

  if (first) {
    fp = fopen(filename, "r");
    if (fp == NULL) {
      printf("\n+++ERROR OPENING topo.dat+++\n");
      exit(1);
    }
    first = 0;
  }

  rval = fseek(fp, (*IREC - 1)*RECLENGTH, 0);  /* skip to desired record */
  if (rval != 0) {
    printf("\n+++ERROR SEEKING ON %s +++\n",filename);
    exit(-1);
  }

  
  fscanf(fp, "%59098c", CBUF);

}  

