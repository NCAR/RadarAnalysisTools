/* workstation version */

#include <stdio.h>
#include "cedric.h"
/*#include "./include/mdv_file.h"*/


struct files *open_files;  /* linked list of open file information */
struct files *head;        /* head of linked list */


/* the following function takes a unit number and returns an id header
 * from the corresponding file
 */
#if defined (IBMRISC) || defined (HP)
void cinhead(INUNIT,NUM1,NUM2,NUM3,IBUF,IREW,MBYTE,FBYTE,NST)
#elif defined (CRAY)
void CINHEAD(INUNIT,NUM1,NUM2,NUM3,IBUF,IREW,MBYTE,FBYTE,NST)
#else
void cinhead_(INUNIT,NUM1,NUM2,NUM3,IBUF,IREW,MBYTE,FBYTE,NST)
#endif
     int *INUNIT, *NUM1, *NUM2, *NUM3, *IREW, *MBYTE, *FBYTE, *NST;
     int IBUF[510];
{
  int i = 0, iloc = 0;
  FILE *fp;
  char ident[5];
  int rval, byte, swap, tval;
  int start = FIRST_VOL;
  char filename[9];
  void swapem();
  void swapem64();

  *NST = 0;
  *MBYTE = ENDIAN;   /* get machine byte ordering */

  /* construct filename */
  filename[0] = 'f';
  filename[1] = 'o';
  filename[2] = 'r';
  filename[3] = 't';
  filename[4] = '.';
  filename[5] = *NUM1 + 48;  /* convert to ascii */
  filename[6] = *NUM2 + 48;  /* convert to ascii */
  if(*NUM3 == -1){
     filename[7] = '\0';
  }
  else{
    filename[7] = *NUM3 + 48;  /* convert to ascii */
    filename[8] = '\0';
  }
  ident[4] = '\0';
  
  if (head == NULL) {  /* start linked list of open files */
    open_files = (struct files *)malloc(sizeof(struct files));
    if (open_files == NULL) { /* error getting address */
      printf("\n+++OUT OF MEMORY IN CINHEAD+++\n");
      exit(-1);
    }
    head = open_files;
    open_files->next = NULL;
    open_files->unit = -99;
  } 
  open_files = head;
  while (open_files->next != NULL  &&  open_files->unit != *INUNIT) {
    open_files = open_files->next;
  }
  if (open_files->unit != *INUNIT) { /* file is not already in list;open it*/

    if (*IREW == 1) *IREW = 0;
    fp = fopen(filename,"r");
    if (fp == NULL) {
      printf("\n+++ERROR OPENING %s FOR READING+++ \n",filename);
      *NST=1;
      return;
    }
    rval = fread(ident, 4, 1, fp);
    if (rval <= 0) {
      printf("\n+++ERROR READING FROM %s +++\n",filename);
      *NST=1;
      return;
    }
    
    if (strcmp(ident, CED) != 0) {
      printf("\n+++FORMAT OF FILE %s NOT RECOGNIZED +++\n",filename);
      *NST=1;
      return;
    }
    
    rval = fread(&byte,1,4,fp);
    if (rval <= 0) {
      printf("\n+++ERROR READING FROM %s +++\n",filename);
      *NST=1;
      return;
    }
    byte = byte >> (WORD_SIZE - 32);
    
    if (byte == 0) {
      *FBYTE = 0;
    } else {
      *FBYTE = 1;
    }

    if (byte != ENDIAN) { /* byte swapping necessary */
      swap = TRUE;
    } 
    else {
      swap = FALSE;
    }
    
    
    rval = fseek(fp, start, 0);     /* skip to starting loc of 1st volume */
    if (rval != 0) {
      printf("\n+++ERROR SEEKING ON %s +++\n",filename);
      *NST=1;
      return;
    }
    
    
    
    if (open_files->unit > 0) {
      /* add new open file to list of open files */
      open_files->next = (struct files *) malloc(sizeof(struct files));
      if (open_files->next == NULL) {
	printf("\n+++OUT OF MEMORY IN CINHEAD+++\n");
	exit(-1);
      }
      open_files = open_files->next;
    }
    open_files->unit = *INUNIT;
    open_files->fps = fp;
    open_files->curr_vol = 1;
    open_files->swap = swap;
    open_files->next = NULL;
    
    /* now read in header and return it */
    
    rval = fread(IBUF, 2, 510, fp);
    if (rval <= 0) {
      printf("\n+++ERROR READING FROM %s +++\n",filename);
      *NST=1;
      return;
    }
    if (open_files->swap)  {  /* go and swap the header bytes */ 
      if (WORD_SIZE == 32) {
	iloc = 255;
	swapem(IBUF,&iloc);
      }
      else if (WORD_SIZE == 64) { /* word size is 64 */
	iloc = 128;
	swapem64(IBUF,&iloc);
      }
      else {  /* can't handle word size */
	printf("\n+++CANNOT HANDLE A WORD SIZE OF %d BITS+++\n",WORD_SIZE);
	*NST=1;
	return;
      }
    }

    
    return;
  }
  
  else  { /* file is already in the list (and open) */
    fp = open_files->fps;
    if (*IREW == 1) {
      rval = fseek(fp,start,0);  /* go to first volume */
      if (rval != 0) {
	printf("\n+++ERROR SEEKING ON %s +++\n",filename);
	*NST=1;
	return;
      }
      open_files->curr_vol = 1;
      *IREW = 0;
    }
    
    /* now read in the header */
    
    rval = fread(IBUF, 2, 510, fp);

    /* Debugging print statement for synthesis headers */
    /* for(i = 0; i < 510; i++) printf("id#%d=%d\n",i,IBUF[i]); */

    if (rval <= 0) {
      printf("\n+++ERROR READING FROM %s +++\n",filename);
      *NST=1;
      return;
    }
    if (open_files->swap)  {  /* go and swap the header bytes */ 
      if (WORD_SIZE == 32) {
	iloc = 255;
	swapem(IBUF,&iloc);
      }
      else if (WORD_SIZE == 64) { /* word size is 64 */
	iloc = 128;
	swapem64(IBUF,&iloc);
      }
      else {  /* can't handle word size */
	printf("\n+++CANNOT HANDLE A WORD SIZE OF %d BITS+++\n",WORD_SIZE);
	*NST=1;
	return;
      }
    }
    
    if (open_files->swap) {
      if (ENDIAN == 0) {
	*FBYTE = 1;
      }
      else {
	*FBYTE = 0;
      }
    }    
    else {
      if (ENDIAN == 0) {
	*FBYTE = 0;
      }
      else {
	*FBYTE = 1;
      }
    }
    
    return ;
  }
  
}

/************************************************************************/
/* the following function skips NSKIP logical cedric volumes in the file
 * corresponding to unit INUNIT 
 */
#if defined (IBMRISC) || defined (HP)
void cskpvol(INUNIT,NSKIP,NST)
#elif defined (CRAY)
void CSKPVOL(INUNIT,NSKIP,NST)
#else
void cskpvol_(INUNIT,NSKIP,NST)
#endif
    int *INUNIT, *NSKIP, *NST;
{
  int rval, byte, iloc, swap;
  int max_vol;
  int vol_loc[25];
  int i, tval[1];
  int j;
  FILE *fp;
  void swapem();

  open_files = head;
  while (open_files->next != NULL && open_files->unit != *INUNIT) 
    open_files = open_files->next;
  
  if (open_files->unit != *INUNIT) { 
    printf("\n+++ERROR SKIPPING ON UNIT %d; NOT OPENED FOR READING+++\n",*INUNIT);
    *NST=1;
    return;
  }

  /* found file; now do the skipping */
  fp = open_files->fps;

  rval = fseek(fp,16,0);
  if (rval != 0) {
    printf("\n+++ERROR SEEKING ON UNIT %d+++\n",*INUNIT);
    *NST=1;
    return;
  }

  if (open_files->swap) {
    swap = TRUE;
  }
  else {
    swap = FALSE;
  }
    

    
  max_vol = 0;
  j = 1;
  for (i = 0; i < 25; i++) {
    rval = fread(tval, 4, 1, fp);
    if (rval <= 0) {
      printf("\n+++ERROR READING FROM UNIT %d +++\n",*INUNIT);
      *NST=1;
      return;
    }
    tval[0] = tval[0] >> (WORD_SIZE - 32);
    if (swap) swapem(tval,&j) /* go do swapping */ ;
    vol_loc[i] = tval[0];
    if (tval[0] > 0) max_vol = max_vol +1;
  }

  iloc = open_files->curr_vol + *NSKIP;
  if (iloc > (max_vol + 1)) {
/*    printf("\n+++ERROR SEEKING PAST END OF FILE ON UNIT %d+++\n",*INUNIT); */
    *NST=1;
    return;
  }
  open_files->curr_vol = iloc;
  rval = fseek(fp, vol_loc[iloc - 1], 0);  /* seek to the new volume */
  if (rval != 0) {
    printf("\n+++ERROR SKIPPING ON UNIT %d +++\n",*INUNIT);
    *NST=1;
    return;
  }
  
  return;
}

/* the following function reads in 16 bit quantities from disk */
#if defined (IBMRISC) || defined (HP)
void cread(INUNIT,IARRAY,NVAL)
#elif defined (CRAY)
void CREAD(INUNIT,IARRAY,NVAL)
#else
void cread_(INUNIT,IARRAY,NVAL)
#endif
     int IARRAY[1];
     int *NVAL,*INUNIT;
{
  int ival;
  FILE *fp;
  void swapem();
  void swapem64();

  open_files = head;
  while (open_files->next != NULL && open_files->unit != *INUNIT) {
    open_files = open_files->next;
  }  
  if (open_files->unit !=  *INUNIT) { 
    printf("\n+++ERROR READING ON UNIT %d; NOT OPENED FOR READING+++\n",*INUNIT);
    exit(-1);
  }

  /* found file; now do the skipping */
  fp = open_files->fps;

  ival = fread(IARRAY, 2, *NVAL, fp);
  if (ival <= 0) {
    printf("\n+++ERROR READING FROM UNIT %d +++\n",*INUNIT);
    exit(-1);
  }
  if (open_files->swap) {
    if (WORD_SIZE == 32) {
      ival = 5;
      swapem(IARRAY,&ival);
    }
    else if (WORD_SIZE == 64){ /* 64 bits per word */
      ival = 3;
      swapem64(IARRAY,&ival);
    }
    else { /* can't handle word size */
      printf("\n+++CANNOT HANDLE A WORD SIZE OF %d BITS+++\n",WORD_SIZE);
      exit(-1);
    }
  }
  return;
}

/* the following function skips a specified number of bytes on a unit */

#if defined (IBMRISC) || defined (HP)
void cskprec(INUNIT,NSKIP)
#elif defined (CRAY)
void CSKPREC(INUNIT,NSKIP)
#else
void cskprec_(INUNIT,NSKIP)
#endif
     int *INUNIT, *NSKIP;
{
  int rval;

  FILE *fp;

  open_files = head;
  while (open_files->next != NULL && open_files->unit != *INUNIT) {
    open_files = open_files->next;
  }  
  if (open_files->unit !=  *INUNIT) { 
    printf("\n+++ERROR READING ON UNIT %d; NOT OPENED FOR READING+++\n",*INUNIT);
    exit(-1);
  }

  /* found file; now do the skipping */
  fp = open_files->fps;
  
  rval = fseek(fp, *NSKIP, 1);
  if (rval != 0) {
    printf("\n+++ERROR SEEKING ON UNIT %d IN CFETCHZ+++\n",*INUNIT);
    exit(-1);
  }

  return;
}





/* the following function returns a z-level slice for a single field
 * from the filename that corresponds to INUNIT
 */
#if defined (IBMRISC) || defined (HP)
void cfetchz(INUNIT,RBUF,NPLANE,ITEM,BAD,SCALE,NST,NSKIP)
#elif defined (CRAY)
void CFETCHZ(INUNIT,RBUF,NPLANE,ITEM,BAD,SCALE,NST,NSKIP)
#else
void cfetchz_(INUNIT,RBUF,NPLANE,ITEM,BAD,SCALE,NST,NSKIP)
#endif
     int *INUNIT, *NPLANE, *NST, *NSKIP;
     int ITEM[1], RBUF[1];
     float *SCALE, *BAD;
{
  int rval, iloc, swap, ival;
  int i, jval, idiff, fval;
  float test;
  FILE *fp;
  void swapem();
  void swapem64();

  open_files = head;
  while (open_files->next != NULL && open_files->unit != *INUNIT) 
    open_files = open_files->next;
  
  if (open_files->unit !=  *INUNIT) { 
    printf("\n+++ERROR READING ON UNIT %d; NOT OPENED FOR READING+++\n",*INUNIT);
    exit(-1);
  }

  /* found file */
  fp = open_files->fps;
  

  /* now read in the data */
  test = *NPLANE/(float)(WORD_SIZE/16.);
  jval = *NPLANE/(int)(WORD_SIZE/16);

  idiff = (test - (float)jval)*(WORD_SIZE/16.0);
  if (test != (float)jval && ENDIAN && !(open_files->swap)) {
    rval = fread(RBUF, 2, (*NPLANE - idiff), fp);
    if (rval <= 0) {
      printf("\n+++ERROR READING ON UNIT %d+++\n", *INUNIT);
      exit(-1);
    }
    rval = fread(&fval, 2, idiff, fp);
    if (rval <= 0) {
      printf("\n+++ERROR READING ON UNIT %d+++\n", *INUNIT);
      exit(-1);
    }
    RBUF[jval] = fval << (WORD_SIZE - idiff*16);
  }
  else {
    rval = fread(RBUF, 2, *NPLANE, fp);
    if (rval <= 0) {
      printf("\n+++ERROR READING ON %d+++\n", *INUNIT);
      exit(-1);
    }
  }
  
  i = (*NPLANE - 1)/(WORD_SIZE/16) + 1;
  if (open_files->swap) {
    if (WORD_SIZE == 32) {
      swapem(RBUF,&i);
    }
    else if (WORD_SIZE == 64) { /* assume 64 bits per word */
      swapem64(RBUF,&i);
    }
    else { /* can't handle word size */
      printf("\n+++CANNOT HANDLE A WORD SIZE OF %d BITS+++\n",WORD_SIZE);
      exit(-1);
    }
  }

  if (test != (float)jval && (ENDIAN == 0) && open_files->swap) {
    fval = RBUF[jval];
    if (idiff == 1) {
      RBUF[jval] = fval << 16;
    }
    else if (idiff == 3) {
      ival = 0;
      ival = fval & 0xffff;
      ival = ival << 16;
      RBUF[jval] = fval | ival;
    }

  }
  return;
}


/* perform byte swapping */
void
  swapem(INJ,NUM)
     int INJ[1];
     int *NUM;
{
  int t1,t2,t3,t4;
  int c1,c2,c3,c4;
  int k,i;

  c1 = 0xff;
  c2 = c1 << 8;
  c3 = c2 << 8;
  c4 = c3 << 8;

  for (i = 0; i < *NUM; i++) {
    t1 = ((INJ[i] & c1)  & c1) << 24;
    t2 = (((INJ[i] & c2) >> 8)  & c1) << 16; 
    t3 = (((INJ[i] & c3) >> 16)  & c1) << 8;
    t4 = ((INJ[i] & c4) >> 24) & c1;
    INJ[i] = t1 | t2 | t3 | t4;
  }

}
/* perform byte swapping for characters */
#if defined (IBMRISC) || defined (HP)
void swapchar(INJ,OUJ,NUM)
#elif defined (CRAY)
void SWAPCHAR(INJ,OUJ,NUM)
#else
void swapchar_(INJ,OUJ,NUM)
#endif
     int INJ[1],OUJ[1];
     int *NUM;
{
  int t1,t2,t3,t4;
  int c1,c2,c3,c4;
  int k,i;

  c1 = 0xff;
  c2 = c1 << 8;
  c3 = c2 << 8;
  c4 = c3 << 8;

  for (i = 0; i < *NUM; i++) {
    t1 = ((INJ[i] & c1)  & c1) << 8;
    t2 = (((INJ[i] & c2) >> 8)  & c1);
    OUJ[i] = t1 | t2 ;
  }

}
/*************************************************************/
#if defined (IBMRISC) || defined (HP)
void swapch2(
#elif defined (CRAY)
void SWAPCH2(
#else
void swapch2_(
#endif
     char cswap[50],int *num)

{

   printf("%s\n",cswap);
   exit(0);
}
/*************************************************************/
/* perform byte swapping for 64 bit words*/
void
swapem64(INJ,NUM)
     int INJ[1000];
     int *NUM;
{
  int t1,t2,t3,t4,t5,t6,t7,t8;
  int c1,c2,c3,c4,c5,c6,c7,c8;
  int k,i;

  c1 = 0xff;
  c2 = c1 << 8;
  c3 = c2 << 8;
  c4 = c3 << 8;
  c5 = c4 << 8;
  c6 = c5 << 8;
  c7 = c6 << 8;
  c8 = c7 << 8;

  for (i = 0; i < *NUM; i++) {
    t1 = ((INJ[i] & c1)  & c1) << 24;
    t2 = (((INJ[i] & c2) >> 8)  & c1) << 16; 
    t3 = (((INJ[i] & c3) >> 16)  & c1) << 8;
    t4 = (((INJ[i] & c4) >> 24) & c1) ;
    t5 = (((INJ[i] & c5) >> 32) & c1) << 56;
    t6 = (((INJ[i] & c6) >> 40) & c1) << 48;
    t7 = (((INJ[i] & c7) >> 48) & c1) << 40;
    t8 = (((INJ[i] & c8) >> 56) & c1) << 32;
    INJ[i] = t1 | t2 | t3 | t4 | t5 | t6 | t7 | t8;
  }

}

/* determines type of input file (pure, netCDF, or COS (Cray only))
 * *IVAL=0 ==> cos blocked 
 * *IVAL=1 ==> pure binary
 * *IVAL=2 ==> netcdf
 */

#if defined (CRAY)
int cos_units[MAXCOS];
#endif

int cdf_units[MAXCDF];
int mdv_units[MAXCDF];


#if defined (IBMRISC) || defined (HP)
void filetyp(INUNIT,ICDF,NST)
#elif defined (CRAY)
void FILETYP(INUNIT,ICDF,NST)
#else
void filetyp_(INUNIT,ICDF,NST)
#endif
     int *INUNIT, *ICDF, *NST;
{
  char filename[9], ident[4];
  int i1, i2, i3, rval,temp;
  FILE *fp;
  static int first = 1;
  /*MDV_master_header_t header;
    void MDV_master_header_to_BE();*/


  *NST = 0;
  open_files = head;
  *ICDF = -1;

#if defined (CRAY)
  if (first == 1) { /* init. cos_units and cdf_units array to neg. numbers */
    for (i1 = 0; i1 < MAXCOS; i1++)
      cos_units[i1] = -1;
    for (i1=0; i1 < MAXCDF; i1++) 
      cdf_units[i1] = -1;
    first = 0;
  }

  if (*ICDF == -1) {  /* see if cos and open */
    for (i1 = 0; i1 < MAXCOS; i1++) 
      if (*INUNIT == cos_units[i1]) *ICDF=0;
  }
#else
  if (first == 1) { /* init. cdf_units to -1 */
    for (i1=0; i1 < MAXCDF; i1++){
      cdf_units[i1] = -1;
      mdv_units[i1] = -1;
    }
    first = 0;
  }

#endif
  

  if (open_files != NULL) {   /* see if pure and open */
    while (open_files->next != NULL && open_files->unit != *INUNIT) 
      open_files = open_files->next;
    if (open_files->unit == *INUNIT) 
      *ICDF = CEDPURE;
    /*printf("   FILETYP:  ICDF = 1 pure binary format \n");*/
  }

  i3 = 0;
  while (cdf_units[i3] != -1) {  /* see if cdf*/
    if (cdf_units[i3] == *INUNIT) {
      *ICDF = CDFFMT;
      /*printf("   FILETYP:  ICDF = 2 revised CDF format \n");*/
      break;
    }
    i3++;
  }

  i3 = 0;
  while (mdv_units[i3] != -1) {  /* see if mdv*/
    if (mdv_units[i3] == *INUNIT) {
      *ICDF = MDVFMT;
      /*printf("   FILETYP:  ICDF = 4 MDV format \n");*/
      break;
    }
    i3++;
  }


  /* construct filename */
  /* and open it to see what format it's in */
  if (*ICDF == -1) {
    filename[0] = 'f';
    filename[1] = 'o';
    filename[2] = 'r';
    filename[3] = 't';
    filename[4] = '.';
    if(*INUNIT > 99){
       i1 = *INUNIT/100;
       temp = *INUNIT - (i1 *100);
       i2 = temp/10;
       temp = temp - (i2 * 10);
       filename[5] = (char)i1 + 48;  /* convert to ascii */
       filename[6] = (char)i2 + 48;  /* convert to ascii */
       filename[7] = (char)temp + 48;  /* convert to ascii */
       filename[8] = '\0';
    }
    else{
      i1 = *INUNIT/10;    /* grab ten's digit */
      i2 = *INUNIT % 10;  /* grab one's digit */
      filename[5] = (char)i1 + 48;  /* convert to ascii */
      filename[6] = (char)i2 + 48;  /* convert to ascii */
      filename[7] = '\0';
    }

    printf("the filename in filetyp in cin.c is %s\n",filename);
    ident[3] = '\0';

    fp = fopen(filename,"r");
    if (fp == NULL) {
      printf("\n+++ERROR OPENING %s FOR READING+++ \n",filename);
      *NST = 1;
      return;
    }
    rval = fread(ident, 3, 1, fp);
    if (rval <= 0) {
      printf("\n+++ERROR READING FROM %s +++\n",filename);
      *NST = 1;
      return;
    }
    if (strcmp(ident, "CED") == 0){
      *ICDF = CEDPURE;
    }
    else if (strcmp(ident, "CDF") == 0){
      i3 = 0;
      *ICDF = CDFFMT;
      cdf_units[i3] = *INUNIT;
    }
    else{
        rval = fseek(fp,0,0);
        if (rval != 0) {
            printf("\n+++ERROR SEEKING ON %s +++\n",filename);
            *NST=1;
            return;
	}
	/*if (fread(&header, sizeof(MDV_master_header_t),
		1, fp) != 1) {
            printf("%s: Error reading in file header\n",filename);
            fclose(fp);
         }
         if(header.struct_id == MDV_MASTER_HEAD_MAGIC_COOKIE)
         {
             i3 = 0;
             *ICDF = MDVFMT;
             mdv_units[i3] = *INUNIT;
             rval = fseek(fp,0,0);
	     }*/
         else 
         {
            *ICDF = -1;
            *NST = 1;
            /*MDV_master_header_to_BE(&header);
            if(header.struct_id == MDV_MASTER_HEAD_MAGIC_COOKIE){
                i3 = 0;
                *ICDF = MDVFMT;
                *NST = 0;
                mdv_units[i3] = *INUNIT;
		}*/
         }              

    }

    
    fclose(fp);

#if defined (CRAY)
    if (*ICDF == -1) { /* assume cos blocked */
      *ICDF = 0;
      i1=0;
      while (cos_units[i1] != -1 && i1 < MAXCOS)
	i1++;
      if (i1 < MAXCOS) {
	cos_units[i1] = *INUNIT;
      }
      else {
	printf("\n+++TOO MANY COS UNITS OPEN; ONLY %d ALLOWED+++\n",MAXCOS);
	*NST = 1;
	return;
      }
    }
#endif
}	     

  return;

}


/* this function adds a cos-blocked file to the list. this list
 * is used by the "ispure" function.
 */

#if defined (CRAY)
void UPDATE_COS(INUNIT)
     int *INUNIT;
{
  int i;

  for (i = 0; i < MAXCOS ; i++) {
    if (*INUNIT == cos_units[i]) {
      return;
    }
    else if (cos_units[i] == -1) {
      cos_units[i] = *INUNIT;
      return;
    }
  }
  printf("\n+++TOO MANY COS UNITS OPEN; ONLY %d ALLOWED+++\n",MAXCOS);
  exit(-1);
}
#endif  


void csleep_()
{
    sleep(1);
}


