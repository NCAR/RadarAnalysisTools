/* workstation version of C functions for edit volume management */

#include <stdio.h>
#include "cedric.h"

FILE *fp_edit;  /* file pointer for edit volume */
FILE *fp_remap; /* file pointer for remap volume */
int max_index;  /* maximum index for data kept in array in local memory */

/* This function allocates as much memory as it can for storing the
 * edit file. The maximum size of the edit file is 16Meg. If it can't
 * fit all 16Meg into RAM, the rest of the edit file will be kept on
 * disk.
 */
#if defined (IBMRISC) || defined (HP)
void cgetmem(LCMB,MEMUSE)
#elif defined (CRAY)
void CGETMEM(LCMB,MEMUSE)
#else
void cgetmem_(LCMB,MEMUSE)
#endif
     int LCMB[];
     int *MEMUSE;
{
  
  int memmax, request, ival;
  fp_edit = fopen(EDIT_NAME,"r+");
  if (fp_edit == NULL) { /* edit file doesn't exist; create one */
    fp_edit = fopen(EDIT_NAME,"w+");
    if (fp_edit == NULL) { /* error */
      printf("\n+++ERROR OPENING SCRATCH FILE %s +++\n",EDIT_NAME);
      exit(-1);
    }
  }
    
  fp_remap = fopen(REMAP_NAME,"r+");
  if (fp_remap == NULL) { /* file doesn't exist; create it */
    fp_remap = fopen(REMAP_NAME,"w+");
    if (fp_remap == NULL) {
      printf("\n+++ERROR OPENING SCRATCH FILE %s +++\n",REMAP_NAME);
      exit(-1);
    }
  }

  /* turn off buffering for standard output */
  setbuf(stdout, (char *)NULL);

  return;
}


/* This function writes out to disk whatever part of the edit file 
 * wouldn't fit in memory.
 */
#if defined (IBMRISC) || defined (HP)
void cput(ITEMP,LOCD,IBIT,NBITS,NSKIP,IRP,ITER,MAXNEED,MINNEED,MEMUSE,IEXTRA,
	   IFIXAX,NX,NY,LEV,IVOL)
#elif defined (CRAY)
void CPUT(ITEMP,LOCD,IBIT,NBITS,NSKIP,IRP,ITER,MAXNEED,MINNEED,MEMUSE,IEXTRA,
	   IFIXAX,NX,NY,LEV,IVOL)
#else
void cput_(ITEMP,LOCD,IBIT,NBITS,NSKIP,IRP,ITER,MAXNEED,MINNEED,MEMUSE,IEXTRA,
	   IFIXAX,NX,NY,LEV,IVOL)
#endif
     int ITEMP[], *LOCD, *IBIT, *NBITS, *NSKIP, *IRP, *ITER, *MAXNEED;
     int *IEXTRA, *IFIXAX, *NX, *NY, *LEV, *IVOL;
     int *MINNEED, *MEMUSE;
{
  
  int ipos, rval, i, ival, jval, fval, condx1;
  int skip, kval, j, jpos, jpos2, k, diff, kpos, lpos;
  float test;
  FILE *fp;

  /* determine which file pointer to use */
  if (*IVOL == 0) {
    fp = fp_edit;
  }
  else if (*IVOL == 1) {
    fp = fp_remap;
  }
		  
  condx1 = 0;
  if (ENDIAN && *IFIXAX==1) condx1=1;

  /* position the output file */
  ipos = (*MINNEED -  1)*WORD_SIZE/8 ;
  
  rval = fseek(fp, ipos, 0);
  if (rval != 0) {
    printf("\n+++ERROR SEEKING ON SCRATCH FILE+++\n");
    exit(-1);
  }
  
  if (condx1) { /* x level write, little endian */
    ipos = ftell(fp);
    if (ipos < 0) {
      printf("\n+++ERROR FINDING POSITION IN SCRATCH FILE+++\n");
      exit(-1);
    }
    for (i = 0; i < (*IRP - *ITER) ; i++) { /* loop and grab cons. x vals */
      jpos = (*NX) * i + (*LEV);
      kpos = jpos % (int) (WORD_SIZE/16);
      lpos = (int) (jpos-1)/(WORD_SIZE/16.0);
      if (WORD_SIZE == 64) {
	if (kpos == 1){
	  kpos = 3;
	}
	else if (kpos == 2) {
	  kpos = 2;
	}
	else if (kpos == 3) {
	  kpos = 1;
	}
	else {
	  kpos = 0;
	}
      }
      jpos = lpos*(WORD_SIZE/16) + kpos;
      jpos = jpos*2 + ipos;
      rval = fseek(fp, jpos, 0);
      if (rval != 0) {
	printf("\n+++ERROR SEEKING ON SCRATCH FILE+++\n");
	exit(-1);
      }
      ival = ITEMP[i];
      rval = fwrite(&ival, 2, 1, fp);
      if (rval <= 0) {
	printf("\n+++ERROR WRITING TO SCRATCH FILE+++\n");
	exit(-1);
      }
    }
    
  }
  else if (*NSKIP == 0) { /* y or z level write */
    if (*IBIT != 0) { /* skip for any initial offset */
      skip = *IBIT / 8;
      rval = fseek(fp, skip, 1);
      if (rval != 0) {
	printf("\n+++ERROR SEEKING ON SCRATCH FILE+++\n");
	exit(-1);
      }
      
    }
    rval = fwrite(ITEMP, 2, (*IRP - *ITER), fp);
    if (rval <= 0) {
      printf("\n+++ERROR WRITING TO SCRATCH FILE+++\n");
      exit(-1);
    }
    
  }
  else { /* x level write, big endian */
    if (*IBIT != 0) { /* skip for any initial offset */
      skip = *IBIT / 8;
      rval = fseek(fp, skip, 1);
      if (rval != 0) {
	printf("\n+++ERROR SEEKING ON SCRATCH FILE+++\n");
	exit(-1);
      }
      
    }
    for (i = 0; i < (*IRP - *ITER); i++ ) {
      ival = ITEMP[i] << (WORD_SIZE - 16);
      rval = fwrite(&ival, 2, 1, fp);
      if (rval <= 0) {
	printf("\n+++ERROR WRITING SCRATCH FILE+++\n");
	exit(-1);
      }
      skip = *NSKIP / 8;
      rval = fseek(fp, skip, 1);
      if (rval != 0) {
	printf("\n+++ERROR SEEKING ON SCRATCH FILE+++\n");
	exit(-1);
      }
    }
  }
  
  /* write out extra bytes to fill out NWL amount of space */
  if (*IEXTRA > 0) {
    rval = fwrite(IEXTRA, 2, *IEXTRA, fp);
    if (rval <= 0) {
      printf("\n+++ERROR WRITING TO SCRATCH FILE+++\n");
      exit(-1);
    }
  }
  return;
  
}


/* This function reads from disk whatever part of the edit file
 * isn't in memory.
 */
#if defined (IBMRISC) || defined (HP)
void cget(ITEMP,LOCD,IBIT,NBITS,NSKIP,IRP,ITER,MAXNEED,MINNEED,MEMUSE,IFIXAX,
           NX,NY,LEV,IVOL)
#elif defined (CRAY)
void CGET(ITEMP,LOCD,IBIT,NBITS,NSKIP,IRP,ITER,MAXNEED,MINNEED,MEMUSE,IFIXAX,
           NX,NY,LEV,IVOL)
#else
void cget_(ITEMP,LOCD,IBIT,NBITS,NSKIP,IRP,ITER,MAXNEED,MINNEED,MEMUSE,IFIXAX,
           NX,NY,LEV,IVOL)
#endif
     int ITEMP[], *LOCD, *IBIT, *NBITS, *NSKIP, *IRP, *ITER, *MAXNEED;
     int *MINNEED, *MEMUSE, *IFIXAX, *NX, *NY, *LEV, *IVOL;
{
  
  int ipos, rval, i, ival, kpos, lpos;
  int skip, kval, j, jpos, jval, fval, diff;
  int condx1;
  int nxin, lvin, nxyin, nxy, lvdiff, nxdiff;
  float test, nxfl, lvfl, nxyfl;
  FILE *fp;
  
  /* determine which file pointer to use */
  if (*IVOL == 0) {
    fp = fp_edit;
  }
  else if (*IVOL == 1) {
    fp = fp_remap;
  }

  condx1 = 0;
  if (ENDIAN && *IFIXAX == 1) condx1=1;   /* special steps neces.
					       * because of byte order 
					       */
  /* position the file pointer */
  ipos = (*MINNEED - 1)*WORD_SIZE/8 ;
  rval = fseek(fp, ipos, 0);
  if (rval != 0) {
    printf("\n+++ERROR SEEKING ON SCRATCH FILE+++\n");
    exit(-1);
  }
  
  if (condx1) { /* x level read, little endian */
    ipos = ftell(fp);
    if (ipos < 0) {
      printf("\n+++ERROR FINDING POSITION IN SCRATCH FILE+++\n");
      exit(-1);
    }
    for (i = 0; i < (*IRP - *ITER) ; i++) { /* loop and grab cons. x vals */
      jpos = (*NX) * i + (*LEV);
      kpos = jpos % (int) (WORD_SIZE/16);
      lpos = (int) (jpos-1)/(WORD_SIZE/16.0);
                
 
      if (WORD_SIZE == 64) {
	if (kpos == 1){
	  kpos = 3;
	}
	else if (kpos == 2) {
	  kpos = 2;
	}
	else if (kpos == 3) {
	  kpos = 1;
	}
	else {
	  kpos = 0;
	}
      }
      jpos = lpos*(WORD_SIZE/16) + kpos;
      jpos = jpos*2 + ipos;
      rval = fseek(fp, jpos, 0);
      if (rval != 0) {
	printf("\n+++ERROR SEEKING ON SCRATCH FILE+++\n");
	exit(-1);
      }
      ival = 0;
      rval = fread(&ival, 2, 1, fp);
      if (rval <= 0) {
	printf("\n+++ERROR READING FROM SCRATCH FILE+++\n");
	exit(-1);
      }
      ITEMP[i] = ival;
    }
    
  }
  
  else if (*NSKIP == 0) { /* normal z level or y level read */
    if (*IBIT != 0) { /* skip for any initial offset */
      skip = *IBIT / 8;
      rval = fseek(fp, skip, 1);
      if (rval != 0) {
	printf("\n+++ERROR SEEKING ON SCRATCH FILE+++\n");
	exit(-1);
      }
    }
    rval = fread(ITEMP, 2, (*IRP - *ITER), fp);
    if (rval <= 0) {
      printf("\n+++ERROR READING FROM SCRATCH FILE+++\n");
      exit(-1);
    }
  }
  else { /* x level read, big endian */
    if (*IBIT != 0) { /* skip for any initial offset */
      skip = *IBIT / 8;
      rval = fseek(fp, skip, 1);
      if (rval != 0) {
	printf("\n+++ERROR SEEKING ON SCRATCH FILE+++\n");
	exit(-1);
      }
    }
    for (i = 0; i < (*IRP - *ITER); i++ ) {
      rval = fread(&ival, 2, 1, fp);
      if (rval <= 0) {
	printf("\n+++ERROR READING FROM SCRATCH FILE+++\n");
	exit(-1);
      }
      skip = *NSKIP / 8;
      rval = fseek(fp, skip, 1);
      if (rval != 0) {
	printf("\n+++ERROR SEEKING ON SCRATCH FILE+++\n");
	exit(-1);
      }
      ITEMP[i] = ival >> (WORD_SIZE - 16);
    }
    
  }
  
  return;
  
}

/* the following function swaps the .cededit and .cedremap file pointers */
#if defined (IBMRISC) || defined (HP)
void cflswap()
#elif defined (CRAY)
void CFLSWAP()
#else
void cflswap_()
#endif
{
  FILE *ftemp;
  ftemp=fp_edit;
  fp_edit=fp_remap;
  fp_remap=ftemp;

  return;
}







