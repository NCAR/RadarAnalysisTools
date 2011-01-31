#include <stdio.h>
#include "dorade.h"

/* The following function searches the data stream for the ASCII
 * string "RYIB". This process is necessary because of the record
 * filler junk that is on the tape. We have to skip all that and get
 * back in sync with the format. This problem only seems to occur before
 * the RYIB descriptor.
 */
int find_ryib(fp,carry,array,len,istat,ctemp2)
     FILE *fp;
     char carry[];
     char array[];
     int len;
     int *istat;
     char ctemp2[8];
{
  char buf[100];
  int i,j,k,rval;
  int ipos, it1,it2,it3,it4;
  char ctemp[5];
  
  memcpy(buf, carry, 4);
  memcpy(&(buf[4]), &len, 4);
  rval = fread(&(buf[8]),92,1,fp);
  if (rval <= 0) {
    *istat = 3;  /* EOD status */
    return 0 ;  
  }
  
  ctemp[4] = '\0';
  
/* loop indefinitely searching for string; either
 * we'll find it, or we'll get a read error trying.
 */ 
  while (1) {  
    i = -1;
    while (strcmp(ctemp,"RYIB") != 0 && i < 92) {
      i++;
      for (j = 0 ; j < 4 ; j++) {
	ctemp[j] = buf[i+j];
      }
    }
    if (strcmp(ctemp,"RYIB") == 0) {  /* found it */
      for (k = 0; k < (100-i-8); k++) {
	array[k] = buf[k+i+8];
      }
      ctemp2[0] = 'R';
      ctemp2[1] = 'Y';
      ctemp2[2] = 'I';
      ctemp2[3] = 'B';
      ctemp2[4] = buf[i+4];
      ctemp2[5] = buf[i+5];
      ctemp2[6] = buf[i+6];
      ctemp2[7] = buf[i+7];
      
      len = four_bytes_2_int(buf[i+4],buf[i+5],buf[i+6],buf[i+7]);
      /* read in rest of dorade block */    
      rval = fread(&(array[100-i-8]),(len+i-100),1,fp);  
      if (rval <= 0) {
	*istat = 3;
	return 0 ;
      }
      return len ;  
    }
    
    /* save last 8 characters, read in next 92 to search for RYIB with
     * this set.
     */
    buf[0]=buf[92];
    buf[1]=buf[93];
    buf[2]=buf[94];
    buf[3]=buf[95];
    buf[4]=buf[96];
    buf[5]=buf[97];
    buf[6]=buf[98];
    buf[7]=buf[99];
    
    rval = fread(&(buf[8]),92,1,fp);
    if (rval <= 0) {
      *istat = 3;
      return 0  ;
    }
  }
  
}
