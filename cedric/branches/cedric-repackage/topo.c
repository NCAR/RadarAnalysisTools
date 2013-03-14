#include <stdio.h>
#include <stdlib.h>  /* exit() */

#define RECLENGTH 59097

/* reads in a record from the ascii topo database */
#if defined (IBMRISC) || defined (HP)
void rdtoprec(IREC, CBUF)
#elif defined (CRAY)
void RDTOPREC(IREC, CBUF)
#else
void rdtoprec_(IREC, CBUF)
#endif
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

