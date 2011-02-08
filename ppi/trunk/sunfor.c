/* ***Sprint - SUNFOR.c*** */

#include <stdio.h>
#include "cedric.h"
#include "op_sys.h"

#define BYTSWRD 4

long   cswap32(long dat); /*prototype for swapping routine*/
extern FILE *fp;  /* file pointer */

#if defined (IBMRISC) || defined (HP)
     void rdsunrec( rp, ilen, dec, decwr )
#elif defined (CRAY)
     void RDSUNREC( rp, ilen, dec, decwr )
#elif defined (LINUX)
     void rdsunrec_( rp, ilen, dec, decwr )
#else
     void rdsunrec_( rp, ilen, dec, decwr )
#endif
     char *rp;			/* record pointer */
     int *ilen;
     float *dec;
     float *decwr;
{

  int i, length;
  char reclength[5];

  /* read in first four bytes */
 
  /* printf("+++ rdsunrec, rp=%d, ilen = %d, dec = %d, decwr= %d\n", *rp,*ilen,*dec,*decwr); */
  i = fread(&length, BYTSWRD, 1, fp);
  if( i <= 0 ) {
    *ilen = 0;
    return;
  }
  /* printf("+++ rdsunrec: after first fread, i=%d, length=%d\n", i,length); */

  length = length >> (WORD_SIZE -32);
  if((*dec == 1. && *decwr == 0.) ||
     (*dec == 0. && *decwr == 1.))
    length=cswap32(length);
  if (length <= 0) {
    *ilen = 0;
    return;
  }
  /* printf("+++ rdsunrec: after first fread, length=%d\n", length); */
  
  /* read in record */

  i = fread(rp, length, 1, fp);
  /* printf("+++ rdsunrec: after second fread, i=%d\n", i); */
  if( i <= 0 ) {
    *ilen = 0;
    return;
  }
  /* printf("+++ rdsunrec: after second fread, i=%d\n", i); */

  /* read tail of record */

  i = fread(reclength, BYTSWRD, 1, fp);
  /* printf("+++ rdsunrec: after third fread, i=%d\n", i); */
  if( i <= 0 ) {
    *ilen = 0;
    return;
  }
  /* printf("+++ rdsunrec: after third fread, i=%d\n", i); */

  *ilen = length;
  /* printf("+++ rdsunrec: after third fread, ilen=%d\n", *ilen); */

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

/*
void csleep_()
{

  sleep(5);
}
*/
