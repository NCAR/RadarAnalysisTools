#include <stdio.h>
#include "cedric.h"

#define BYTSWRD 4

long   cswap32(long dat); /*prototype for swapping routine*/
extern FILE *fp;  /* file pointer */

#if defined (IBMRISC) || defined (HP)
     void rdsunrec(rp, ilen, swapping)
#elif defined (CRAY)
     void RDSUNREC( rp, ilen, swapping )
#elif defined (linux)
     void rdsunrec_( rp, ilen, swapping )
#else
     void rdsunrec_( rp, ilen, swapping )
#endif
     char *rp;			/* record pointer */
     int *ilen;
     int *swapping;
{

  int i, length;
  char reclength[5];

  /* read in first four bytes */

  i = fread(&length, BYTSWRD, 1, fp);
  if( i <= 0 ) {
    *ilen = 0;
    return;
  }

  length = length >> (WORD_SIZE -32);
  if(*swapping == 1) length = cswap32((long)length);
  if (length <= 0) {
    *ilen = 0;
    return;
  }
  
  /* read in record */

  i = fread(rp, length, 1, fp);
  if( i <= 0 ) {
    *ilen = 0;
    return;
  }

  /* read tail of record */

  i = fread(reclength, BYTSWRD, 1, fp);
  if( i <= 0 ) {
    *ilen = 0;
    return;
  }


  *ilen = length;

  return;

}
/***************************************************************/
long cswap32(long dat)
 
{
  int t1,t2,t3,t4;
  int c1,c2,c3,c4;
  
  c1 = 0xff;
  c2 = c1 << 8;
  c3 = c2 << 8;
  c4 = c3 << 8;
  
    t1 = ((dat & c1)  & c1) << 24;
    t2 = (((dat & c2) >> 8)  & c1) << 16; 
    t3 = (((dat & c3) >> 16)  & c1) << 8;
    t4 = ((dat & c4) >> 24) & c1;
    dat = t1 | t2 | t3 | t4;

  return (dat);
}
/***************************************************************/
unsigned short cswap16(dat)

unsigned short dat;

{
  int t1,t2;
  int c1,c2;
  
  c1 = 0xff;
  c2 = c1 << 8;

    t1 = ((dat & c1)  & c1) << 8;
    t2 = (((dat & c2) >> 8)  & c1) ; 

    dat = t1 | t2 ;

  return (dat);
}

void csleep_()
{

  sleep(5);
}
