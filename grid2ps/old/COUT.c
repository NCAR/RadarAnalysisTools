/* workstation version */

#include <stdio.h>
#include "cedric.h" 

FILE *fp1;   /* pointer to output file */

void cout_(L1,L2,IPOS,ISKP,IBUF)
     int *L1, *L2;
     int *IPOS, *ISKP;
     int IBUF[1];
{
  extern struct files *open_files, *head;
  char filename[8];
  char *ced = CED;
  char inp[5];
  int byte_order = BYTE_ORDER;
  int start = FIRST_VOL;
  int zero = 0 , swap, ival;
  int byte, i, jval, count, ipos, jpos, inunit;
  
  inp[4] = '\0';
  byte_order = byte_order << (WORD_SIZE - 32);
  start = start << (WORD_SIZE - 32); /* shift the integer */
  inunit = *L1*10 + *L2;
  
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
      printf("+++OUT OF MEMORY IN COUT\n");
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
	printf("+++OUT OF MEMORY IN COUT\n");
	exit(-1);
      }
      open_files = open_files->next;
      open_files->unit = inunit;
      open_files->next = NULL;
    }      
    
    fp1 = fopen(filename,"w+");
    if (fp1 == NULL)  {
      printf("+++ERROR OPENING %s FOR WRITING+++\n",filename);
      exit(-1);
    }
    
    ival = fwrite(ced, 1, 4, fp1);
    if (ival <= 0) {
      printf("+++ERROR WRITING TO %s +++\n",filename);
      exit(-1);
    }
    
    ival = fwrite(&byte_order, 1, 4, fp1);  /* write out byte_order flag */
    if (ival <= 0) {
      printf("+++ERROR WRITING TO %s +++\n",filename);
      exit(-1);
    }
    
    fseek(fp1,8,1);  /* skip past length information */
    
    ival = fwrite(&start, 1, 4, fp1);  /* write out starting byte location
					  of first cedric volume */
    if (ival <= 0) {
      printf("+++ERROR WRITING TO %s +++\n",filename);
      exit(-1);
    }
    
    for (i = 0; i < 24; i++) {  /* set other fields to zero */
      ival = fwrite(&zero, 1, 4, fp1);
      if (ival <= 0) {
	printf("+++ERROR WRITING TO %s +++\n",filename);
	exit(-1);
      }
      
    }
    ival = fwrite(IBUF, 1, 56, fp1);  /* write out descriptive string */
    if (ival <= 0) {
      printf("+++ERROR WRITING TO %s +++\n",filename);
      exit(-1);
    }

    for (i = 0 ; i < (24*14); i++) { /* zero out other string positions */
      ival = fwrite(&zero, 1, 4, fp1);
      if (ival <= 0) {
	printf("+++ERROR WRITING TO %s +++\n",filename);
	exit(-1);
      }
    }      

    for (i = 0; i < 6; i++) {  /* zero out reserved fields */
      ival = fwrite(&zero, 1, 4, fp1);
      if (ival <= 0) {
	printf("+++ERROR WRITING TO %s +++\n",filename);
	exit(-1);
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
	printf("+++ERROR OPENING %s FOR APPENDING+++\n",filename);
	exit(-1);
      }
      open_files->next = (struct files *)malloc(sizeof(struct files));
      if (open_files->next == NULL) {
	printf("+++OUT OF MEMORY IN COUT\n");
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
	printf("ERROR SEEKING ON OUTPUT FILE\n");
	exit(-1);
      }
    }
    
    ival = fread(inp, 1, 4, fp1);
    if (ival <= 0) {
      printf("+++ERROR READING CEDRIC FILE %s IN COUT+++\n",filename);
      exit(-1);
    }
    
    if (strcmp(inp, CED) != 0) {
      printf("+++INPUT FILE FORMAT NOT RECOGNIZED ON %s +++\n",filename);
      exit(-1);
    }
    
    ival = fread(&byte, 1, 4, fp1);
    byte = byte >> (WORD_SIZE - 32);
    if (byte != BYTE_ORDER) {   /* byte swapping needs to be done */
      printf("+++ERROR: CANNOT HAVE TWO DIFFERENT BYTE ORDERINGS IN SAME FILE. WRITE TO A DIFFERENT OUTPUT FILE+++\n");
      exit(-1);
    }
    
    fseek(fp1,8,1);  /* skip past length info */
    
    /* find next available slot in output file */
    
    ival = fread(&jval, 4, 1, fp1);
    jval = jval >> (WORD_SIZE - 32);   
    count = 1;
    
    while (jval != 0) {
      ival = fread(&jval, 4, 1, fp1);
      if (ival <= 0) {
	printf("+++ERROR READING CEDRIC FILE 2+++\n",filename);
	exit(-1);
      }
      jval = jval >> (WORD_SIZE - 32);   
      count = count +1;
      if (count > MAXVOL) { /* external file is too big */
	printf("Error: output file cannot contain more than",
		"MAXVOL cedric volumes\n");
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
      printf("+++ERROR WRITING TO CEDRIC FILE+++\n");
      exit(-1);
    }
    
    jpos = jpos >> (WORD_SIZE - 32);
    fseek(fp1,jpos,0); /* go to end of file for adding additional volume*/
    jpos = ftell(fp1);
    
    open_files->curr_vol = count;
  }    
  return;
}


/* this function writes 16 bit int values to disk */
void cwrite_(IARRAY,NVAL)
     int IARRAY[1];
     int *NVAL;
{
  int ival, jval, fval;
  float test;
  int diff;
  
  fval = 0;
  test = (float) *NVAL/(WORD_SIZE/16.0);
  jval = (int) *NVAL/(WORD_SIZE/16);
  diff = (test - (float)jval)*(WORD_SIZE/16);

  if (((float)jval != test) && BYTE_ORDER) {
  
    ival = fwrite(IARRAY, 2, *NVAL-diff, fp1);
    if (ival <= 0) {
      printf("+++ERROR WRITING CEDRIC FILE TO DISK+++\n");
      exit(-1);
    }
    fval = IARRAY[jval] >> (WORD_SIZE - 16*diff);
    ival = fwrite(&fval, 2, diff, fp1);
    if (ival <= 0) {
      printf("+++ERROR WRITING CEDRIC FILE TO DISK+++\n");
      exit(-1);
    }
  } 
  else {

    ival = fwrite(IARRAY, 2, *NVAL, fp1);
    if (ival <= 0) {
      printf("+++ERROR WRITING CEDRIC FILE TO DISK+++\n");
      exit(-1);
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

void cbyte_(MBYTE)
     int *MBYTE;
{
  *MBYTE = BYTE_ORDER;

  return;
}




/* this function closes the file */
void cclose_()
{
  int ret;
  
  ret = fclose(fp1);
  if (ret != 0) {
    printf("+++ERROR CLOSING OUTPUT FILE+++\n");
    exit(-1);
  }
  
  return;
}
