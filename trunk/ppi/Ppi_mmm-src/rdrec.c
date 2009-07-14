/* ***Sprint - rdrec.c*** */

#include <stdio.h>
#include "op_sys.h"
extern FILE *fp,*wp;


#if defined (IBMRISC) || defined (HP)
     void rdrec(rp, length,ilen )
#elif defined (CRAY)
     void RDREC( rp, length,ilen)
#elif defined (LINUX)
     void rdrec_( rp, length,ilen)
#else
     void rdrec_( rp, length,ilen)
#endif
     char *rp;			/* record pointer */
     int *ilen;
     int *length;


{

  int i;
  char reclength[5];

/* read in record */

  i = fread(rp, *length, 1, fp);
  if( i <= 0 ) {
    *ilen = 0;
    return;
  }

/* read tail of record */

  *ilen = *length;

  return;

}



