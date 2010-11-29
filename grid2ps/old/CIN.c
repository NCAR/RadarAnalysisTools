/* workstation version */

#include <stdio.h>
#include "cedric.h"

struct files *open_files;  /* linked list of open file information */
struct files *head;        /* head of linked list */


/* the following function takes a unit number and returns an id header
 * from the corresponding file
 */
void cinhead_(INUNIT,NUM1,NUM2,IBUF,IREW,MBYTE,FBYTE,filename)
     int *INUNIT, *NUM1, *NUM2, *IREW, *MBYTE, *FBYTE;
     int IBUF[510];
     char filename[];
{
  int i = 0, iloc = 0;
  FILE *fp;
  char ident[5];
  int rval, byte, swap, tval;
  int start = FIRST_VOL;
  int index=0;
/*
  char filename[8];
*/
  void swapem_();
  void swapem64();

  /*printf ("name: %s\n",filename);*/
  while (filename[index]!=' ') {index++;} 
  filename[index]='\0';
  *MBYTE = BYTE_ORDER;   /* get machine byte ordering */

  /* construct filename */
/*
  filename[0] = 'f';
  filename[1] = 'o';
  filename[2] = 'r';
  filename[3] = 't';
  filename[4] = '.';
  filename[5] = *NUM1 + 48;   convert to ascii 
  filename[6] = *NUM2 + 48;   convert to ascii 
  filename[7] = '\0';
*/

  ident[4] = '\0';
  
  if (head == NULL) {  /* start linked list of open files */
    open_files = (struct files *)malloc(sizeof(struct files));
    if (open_files == NULL) { /* error getting address */
      printf("OUT OF MEMORY IN CINHEAD\n");
      exit(-1);
    }
    head = open_files;
    open_files->next = NULL;
    open_files->unit = -99;
  } 
  open_files = head;
/*
  while (open_files->next != NULL  &&  open_files->unit != *INUNIT) {
    open_files = open_files->next;
  }
*/
  open_files->unit=*INUNIT+1;
  if (open_files->unit != *INUNIT) { /* file is not already in list;open it*/

    if (*IREW == 1) *IREW = 0;
    fp = fopen(filename,"r+");
    if (fp == NULL) {
      printf("+++ERROR OPENING %s FOR READING \n",filename);
      exit(-1);
    }
    rval = fread(ident, 4, 1, fp);
    if (rval <= 0) {
      printf("+++ERROR READING FROM %s\n",filename);
      exit(-1);
    }
    
    if (strcmp(ident, CED) != 0) {
      printf("+++INPUT FILE FORMAT NOT RECOGNIZED+++\n");
      exit(-1);
    }
    
    rval = fread(&byte,1,4,fp);
    if (rval <= 0) {
      printf("+++ERROR READING FROM %s\n",filename);
      exit(-1);
    }
    byte = byte >> (WORD_SIZE - 32);
    
    if (byte == 0) {
      *FBYTE = 0;
    } else {
      *FBYTE = 1;
    }

    if (byte != BYTE_ORDER) { /* byte swapping necessary */
      swap = TRUE;
    } 
    else {
      swap = FALSE;
    }
    
    
    rval = fseek(fp, start, 0);     /* skip to starting loc of 1st volume */
    if (rval != 0) {
      printf("+++ERROR SEEKING ON %s\n",filename);
      exit(-1);
    }
    
    
    
    if (open_files->unit > 0) {
      /* add new open file to list of open files */
      open_files->next = (struct files *) malloc(sizeof(struct files));
      if (open_files->next == NULL) {
	printf("+++OUT OF MEMORY (RAM) IN CINHEAD+++\n");
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
      printf("+++ERROR READING FROM %s\n",filename);
      exit(-1);
    }
    if (open_files->swap)  {  /* go and swap the header bytes */ 
      if (WORD_SIZE == 32) {
	iloc = 255;
	swapem_(IBUF,&iloc);
      }
      else if (WORD_SIZE == 64) { /* word size is 64 */
	iloc = 128;
	swapem64(IBUF,&iloc);
      }
      else {  /* can't handle word size */
	printf("CANNOT HANDLE WORD SIZE %d\n",WORD_SIZE);
	exit(-1);
      }
    }

    
    return;
  }
  
  else  { /* file is already in the list (and open) */
    fp = open_files->fps;
    if (*IREW == 1) {
      rval = fseek(fp,start,0);  /* go to first volume */
      if (rval != 0) {
	printf("+++ERROR SEEKING ON %s\n",filename);
	exit(-1);
      }
      open_files->curr_vol = 1;
      *IREW = 0;
    }
    
    /* now read in the header */
    
    rval = fread(IBUF, 2, 510, fp);
    if (rval <= 0) {
      printf("+++ERROR READING FROM %s\n",filename);
      exit(-1);
    }
    if (open_files->swap)  {  /* go and swap the header bytes */ 
      if (WORD_SIZE == 32) {
	iloc = 255;
	swapem_(IBUF,&iloc);
      }
      else if (WORD_SIZE == 64) { /* word size is 64 */
	iloc = 128;
	swapem64(IBUF,&iloc);
      }
      else {  /* can't handle word size */
	printf("CANNOT HANDLE WORD SIZE %d\n",WORD_SIZE);
	exit(-1);
      }
    }
    
    if (open_files->swap) {
      if (BYTE_ORDER == 0) {
	*FBYTE = 1;
      }
      else {
	*FBYTE = 0;
      }
    }    
    else {
      if (BYTE_ORDER == 0) {
	*FBYTE = 0;
      }
      else {
	*FBYTE = 1;
      }
    }
    
    return ;
  }
  
}
/* the following function skips NSKIP logical cedric volumes in the file
 * corresponding to unit INUNIT 
 */
void cskpvol_(INUNIT,NSKIP)
    int *INUNIT, *NSKIP;
{
  int rval, byte, iloc, swap;
  int max_vol;
  int vol_loc[25];
  int i, tval[1];
  int j;
  FILE *fp;
  void swapem_();

  open_files = head;
  while (open_files->next != NULL && open_files->unit != *INUNIT) 
    open_files = open_files->next;
  
  if (open_files->unit != *INUNIT) { 
    printf("+++ERROR SKIPPING ON UNIT %d; NOT OPENED FOR READING\n",*INUNIT);
    exit(-1);
  }

  /* found file; now do the skipping */
  fp = open_files->fps;

  rval = fseek(fp,16,0);
  if (rval != 0) {
    printf("+++ERROR SEEKING ON %d+++\n",*INUNIT);
    exit(-1);
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
      printf("+++ERROR READING FROM %d\n",*INUNIT);
      exit(-1);
    }
    tval[0] = tval[0] >> (WORD_SIZE - 32);
    if (swap) swapem_(tval,&j) /* go do swapping */ ;
    vol_loc[i] = tval[0];
    if (tval[0] > 0) max_vol = max_vol +1;
  }

  iloc = open_files->curr_vol + *NSKIP;
  if (iloc > (max_vol + 1)) {
    printf("+++ERROR SEEKING PAST END OF FILE ON %d+++\n",*INUNIT);
    exit(-1);
  }
  open_files->curr_vol = iloc;
  rval = fseek(fp, vol_loc[iloc - 1], 0);  /* seek to the new volume */
  if (rval != 0) {
    printf("+++ERROR SKIPPING ON %d\n",*INUNIT);
    exit(-1);
  }
  
  return;
}

/* the following function reads in 16 bit quantities from disk */
void cread_(INUNIT,IARRAY,NVAL)
     int IARRAY[1];
     int *NVAL,*INUNIT;
{
  int ival;
  FILE *fp;
  void swapem_();
  void swapem64();

  open_files = head;
  while (open_files->next != NULL && open_files->unit != *INUNIT) {
    open_files = open_files->next;
  }  
  if (open_files->unit !=  *INUNIT) { 
    printf("+++ERROR READING ON UNIT %d; NOT OPENED FOR READING\n",*INUNIT);
    exit(-1);
  }

  /* found file; now do the skipping */
  fp = open_files->fps;

  ival = fread(IARRAY, 2, *NVAL, fp);
  if (ival <= 0) {
    printf("+++ERROR READING FROM CEDRIC FILE+++\n");
    exit(-1);
  }
  if (open_files->swap) {
    if (WORD_SIZE == 32) {
      ival = 5;
      swapem_(IARRAY,&ival);
    }
    else if (WORD_SIZE == 64){ /* 64 bits per word */
      ival = 3;
      swapem64(IARRAY,&ival);
    }
    else { /* can't handle word size */
      printf("CANNOT HANDLE WORD SIZE %d\n",WORD_SIZE);
      exit(-1);
    }
  }
  return;
}

/* the following function skips a specified number of bytes on a unit */

void cskprec_(INUNIT,NSKIP)
     int *INUNIT, *NSKIP;
{
  int rval;

  FILE *fp;

  open_files = head;
  while (open_files->next != NULL && open_files->unit != *INUNIT) {
    open_files = open_files->next;
  }  
  if (open_files->unit !=  *INUNIT) { 
    printf("+++ERROR READING ON UNIT %d; NOT OPENED FOR READING\n",*INUNIT);
    exit(-1);
  }

  /* found file; now do the skipping */
  fp = open_files->fps;
  
  rval = fseek(fp, *NSKIP, 1);
  if (rval != 0) {
    printf("+++ERROR SEEKING ON %d IN CFETCHZ+++\n",*INUNIT);
    exit(-1);
  }

  return;
}





/* the following function returns a z-level slice for a single field
 * from the filename that corresponds to INUNIT
 */
void cfetchz_(INUNIT,RBUF,NPLANE,ITEM,BAD,SCALE,NST,NSKIP)
     int *INUNIT, *NPLANE, *NST, *NSKIP;
     int ITEM[1], RBUF[1];
     float *SCALE, *BAD;
{
  int rval, iloc, swap, ival;
  int i, jval, idiff, fval;
  float test;
  FILE *fp;
  void swapem_();
  void swapem64();

  open_files = head;
  while (open_files->next != NULL && open_files->unit != *INUNIT) 
    open_files = open_files->next;
  
  if (open_files->unit !=  *INUNIT) { 
    printf("+++ERROR READING ON UNIT %d; NOT OPENED FOR READING\n",*INUNIT);
    exit(-1);
  }

  /* found file */
  fp = open_files->fps;
  

  /* now read in the data */
  test = *NPLANE/(float)(WORD_SIZE/16.);
  jval = *NPLANE/(int)(WORD_SIZE/16);

  idiff = (test - (float)jval)*(WORD_SIZE/16.0);
  if (test != (float)jval && BYTE_ORDER && !(open_files->swap)) {
    rval = fread(RBUF, 2, (*NPLANE - idiff), fp);
    if (rval <= 0) {
      printf("+++ERROR READING ON %d+++\n", *INUNIT);
      exit(-1);
    }
    rval = fread(&fval, 2, idiff, fp);
    if (rval <= 0) {
      printf("+++ERROR READING ON %d+++\n", *INUNIT);
      exit(-1);
    }
    RBUF[jval] = fval << (WORD_SIZE - idiff*16);
  }
  else {
    rval = fread(RBUF, 2, *NPLANE, fp);
    if (rval <= 0) {
      printf("+++ERROR READING ON %d+++\n", *INUNIT);
      exit(-1);
    }
  }
  
  i = (*NPLANE - 1)/(WORD_SIZE/16) + 1;
  if (open_files->swap) {
    if (WORD_SIZE == 32) {
      swapem_(RBUF,&i);
    }
    else if (WORD_SIZE == 64) { /* assume 64 bits per word */
      swapem64(RBUF,&i);
    }
    else { /* can't handle word size */
      printf("CANNOT HANDLE WORD SIZE %d\n",WORD_SIZE);
      exit(-1);
    }
  }

  if (test != (float)jval && (BYTE_ORDER == 0) && open_files->swap) {
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
  swapem_(INJ,NUM)
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
/* perform byte swapping */
void
  swapchar_(INJ,OUJ,NUM)
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
