/* workstation version */

#include <stdio.h>
#include "cedric.h" 

FILE *fp1;   /* pointer to output file */

/* the following function initializes the output file for writing;
 * "initialization" includes opening the file, if necessary, and
 * positioning it as requested
 */
void FORTRAN_NAME(cout)(L1,L2,IPOS,ISKP,IBUF,NST)
     int *L1, *L2, *NST;
     int *IPOS, *ISKP;
     int IBUF[1];
{
  extern struct files *open_files, *head;
  char filename[8];
  char *ced = CED;
  char inp[5];
  int byte_order = ENDIAN;
  int start = FIRST_VOL;
  int zero = 0 , swap, ival;
  int byte, i, jval, count, ipos, jpos, inunit;
  
  *NST = 0;
  inp[4] = '\0';
  byte_order = byte_order << (WORD_SIZE - 32);
  start = start << (WORD_SIZE - 32); /* shift the integer */
  inunit = *L1*10 + *L2;
  printf("+++COUT:  WORD_SIZE %d +++\n",WORD_SIZE);  
  printf("+++COUT: byte_order %d +++\n",byte_order);  
  printf("+++COUT:      start %d +++\n",start);
  printf("+++COUT:     inunit %d +++\n",inunit); 
  
  /* construct filename */  
  filename[0] = 'f';
  filename[1] = 'o';
  filename[2] = 'r';
  filename[3] = 't';
  filename[4] = '.';
  filename[5] = *L1 + 48;    /* convert to ascii */
  filename[6] = *L2 + 48;    /* convert to ascii */
  filename[7] = '\0';
   


  if (head == NULL) { /* create a list */
    head = (struct files*)malloc(sizeof(struct files));
    if (head == NULL) {
      printf("\n+++COUT-ERROR: OUT OF MEMORY+++\n");
      exit(-1);
    }
    head->next = NULL;
  }
  if (*IPOS == 1) {  /* create for writing */
    open_files = head;
    while (open_files->next != NULL  &&  open_files->unit != inunit) {
      open_files = open_files->next;
    }
    if (open_files->unit == inunit) {
      fclose(open_files->fps);
    }
    else { /* add a new entry to the linked list */
      open_files->next = (struct files*)malloc(sizeof(struct files));
      if (open_files->next == NULL) {
	printf("\n+++COUT-ERROR: OUT OF MEMORY+++\n");
	exit(-1);
      }
      open_files = open_files->next;
      open_files->unit = inunit;
      open_files->next = NULL;
    }      
    
    fp1 = fopen(filename,"w+");
    if (fp1 == NULL)  {
      printf("\n+++COUT-ERROR: OPENING %s FOR WRITING+++\n",filename);
      *NST = 1;
      return;
    }
    
    ival = fwrite(ced, 1, 4, fp1);
    if (ival <= 0) {
      printf("\n+++COUT-ERROR: WRITING TO %s +++\n",filename);
      *NST = 1;
      return;
    }
    
    ival = fwrite(&byte_order, 1, 4, fp1);  /* write out byte_order flag */
    if (ival <= 0) {
      printf("\n+++COUT-ERROR: WRITING TO %s +++\n",filename);
      *NST = 1;
      return;
    }
    
    fseek(fp1,8,1);  /* skip past length information */
    
    ival = fwrite(&start, 1, 4, fp1);  /* write out starting byte location
					  of first cedric volume */
    if (ival <= 0) {
      printf("\n+++COUT-ERROR: WRITING TO %s +++\n",filename);
      *NST = 1;
      return;
    }
    
    for (i = 0; i < 24; i++) {  /* set other fields to zero */
      ival = fwrite(&zero, 1, 4, fp1);
      if (ival <= 0) {
	printf("\n+++COUT-ERROR: WRITING TO %s +++\n",filename);
	*NST = 1;
	return;
      }
      
    }
    ival = fwrite(IBUF, 1, 56, fp1);  /* write out descriptive string */
    if (ival <= 0) {
      printf("\n+++COUT-ERROR: WRITING TO %s +++\n",filename);
      *NST = 1;
      return;
    }

    for (i = 0 ; i < (24*14); i++) { /* zero out other string positions */
      ival = fwrite(&zero, 1, 4, fp1);
      if (ival <= 0) {
	printf("\n+++COUT-ERROR: WRITING TO %s +++\n",filename);
	*NST = 1;
	return;
      }
    }      

    for (i = 0; i < 6; i++) {  /* zero out reserved fields */
      ival = fwrite(&zero, 1, 4, fp1);
      if (ival <= 0) {
	printf("\n+++COUT-ERROR: WRITING TO %s +++\n",filename);
	*NST = 1;
	return;
      }
    }
    
    /* update linked list with new info */
    open_files->fps = fp1;
    open_files->curr_vol = 1;
    open_files->swap = FALSE;
    
  }
  else if (*IPOS == 2) {  /* add to end of existing file */
    open_files = head;
    while (open_files->next != NULL  &&  open_files->unit != inunit) {
      open_files = open_files->next;
    }
    if (open_files->unit != inunit) {
      fp1 = fopen(filename,"r+");
      if (fp1 == NULL)  {
	printf("\n+++COUT-ERROR: OPENING %s FOR APPENDING+++\n",filename);
	*NST = 1;
	return;
      }
      open_files->next = (struct files *)malloc(sizeof(struct files));
      if (open_files->next == NULL) {
	printf("\n+++COUT-ERROR: OUT OF MEMORY+++\n");
	exit(-1);
      }
      open_files = open_files->next;
      open_files->unit = inunit;
      open_files->fps = fp1;
      open_files->next = NULL;
    }
    else {
      fp1 = open_files->fps;
      ival = fseek(fp1,0,0);
      if (ival != 0) {
	printf("\n+++COUT-ERROR: SEEKING ON UNIT %d +++\n",inunit);
	*NST = 1;
	return;
      }
    }
    
    ival = fread(inp, 1, 4, fp1);
    if (ival <= 0) {
      printf("\n+++COUT-ERROR: READING CEDRIC FILE %s +++\n",filename);
      *NST = 1;
      return;
    }
    
    if (strcmp(inp, CED) != 0) {
      printf("\n+++COUT-ERROR: INPUT FILE FORMAT NOT RECOGNIZED ON %s +++\n",filename);
      *NST = 1;
      return;
    }
    
    ival = fread(&byte, 1, 4, fp1);
    byte = byte >> (WORD_SIZE - 32);
    if (byte != ENDIAN) {   /* byte swapping needs to be done */
      printf("\n+++COUT-ERROR: CANNOT HAVE TWO DIFFERENT BYTE ORDERINGS IN SAME FILE. WRITE TO A DIFFERENT OUTPUT FILE+++\n");
      *NST = 1;
      return;
    }
    
    fseek(fp1,8,1);  /* skip past length info */
    
    /* find next available slot in output file */
    
    ival = fread(&jval, 4, 1, fp1);
    jval = jval >> (WORD_SIZE - 32);   
    count = 1;
    
    while (jval != 0) {
      ival = fread(&jval, 4, 1, fp1);
      if (ival <= 0) {
	printf("\n+++COUT-ERROR: READING FROM CEDRIC FILE %s+++\n",filename);
	exit(-1);
      }
      jval = jval >> (WORD_SIZE - 32);   
      count = count +1;
      if (count > MAXVOL) { /* external file is too big */
	printf("\n+++COUT-ERROR: OUTPUT FILE CANNOT CONTAIN MORE THAN %d CEDRIC VOLUMES+++\n",MAXVOL);
	exit(-1);
      }
    }
    
    ipos = ftell(fp1);
    ipos = ipos - 4;
    
    fseek(fp1,0,2);
    jpos = ftell(fp1) + 1;
    fseek(fp1,ipos,0);
    jpos = jpos << (WORD_SIZE - 32);   /* shift to upper 32 bits */
    ival = fwrite(&jpos, 4, 1, fp1);

    /* now output the descriptive character string for this volume */
    for (i = 0 ; i < (25 - count); i++) {
      fseek(fp1,4,1);
    }

    for (i = 0; i < (count - 1); i++) {
      fseek(fp1,56,1);
    }

    ival = fwrite(IBUF,56,1,fp1);
    if (ival <= 0) {
      printf("\n+++COUT-ERROR: WRITING TO CEDRIC FILE %s +++\n", filename);
      *NST = 1;
      return;
    }
    
    jpos = jpos >> (WORD_SIZE - 32);
    fseek(fp1,jpos,0); /* go to end of file for adding additional volume*/
    jpos = ftell(fp1);
    
    open_files->curr_vol = count;
  }    
  return;
}

/* this function writes 16 bit int values to disk */
void FORTRAN_NAME(cwrite)(IARRAY,NVAL,NST)
     int IARRAY[1];
     int *NVAL, *NST;
{
  int ival, jval, fval;
  float test;
  int diff;
  
  *NST = 0;
  fval = 0;
  test = (float) *NVAL/(WORD_SIZE/16.0);
  jval = (int) *NVAL/(WORD_SIZE/16);
  diff = (test - (float)jval)*(WORD_SIZE/16);

  if (((float)jval != test) && ENDIAN) {
  
    ival = fwrite(IARRAY, 2, *NVAL-diff, fp1);
    if (ival <= 0) {
      printf("\n+++COUT-ERROR: WRITING CEDRIC FILE TO DISK+++\n");
      *NST = 1;
      return;
    }
    fval = IARRAY[jval] >> (WORD_SIZE - 16*diff);
    ival = fwrite(&fval, 2, diff, fp1);
    if (ival <= 0) {
      printf("\n+++COUT-ERROR: WRITING CEDRIC FILE TO DISK+++\n");
      *NST = 1;
      return;
    }
  } 
  else {

    ival = fwrite(IARRAY, 2, *NVAL, fp1);
    if (ival <= 0) {
      printf("\n+++COUT-ERROR: WRITING CEDRIC FILE TO DISK+++\n");
      *NST = 1;
      return;
    }

  }
  return;
}

/* this function updates the length field for the file */
void clen_()
{
  int ival;
  int jpos;

  jpos = ftell(fp1);
  jpos = jpos << (WORD_SIZE - 32);
  fseek(fp1,12,0);
  ival = fwrite(&jpos,1,4,fp1);
  jpos = jpos >> (WORD_SIZE - 32);
  fseek(fp1,jpos,0);

  return;
}

/* this function returns the byte ordering of the machine */
void FORTRAN_NAME(cbyte)(MBYTE)
     int *MBYTE;
{
  *MBYTE = ENDIAN;

  return;
}

/* this function closes all open files */
void FORTRAN_NAME(cclose)()
{
  int ret;
  extern struct files *open_files, *head;
  
  while (open_files != NULL) {
    ret = fclose(open_files->fps);
/*    if (ret != 0) {
      printf("\n+++COUT-ERROR: CLOSING OUTPUT FILE+++\n");
      exit(-1);
    }
*/
    open_files = open_files->next;
  }
  
  return;
}
