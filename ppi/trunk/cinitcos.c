#include <stdio.h>
#include "cedric.h"
#include "op_sys.h"

struct files *open_files;  /* linked list of open file information */
struct files *head;        /* head of linked list */
FILE *fp,*wp;
extern int cos_size;


#if defined (IBMRISC) || defined (HP)
     void init_cos(IUN, IREW)
#elif defined (CRAY)
     void INIT_COS(IUN, IREW)
#elif defined (LINUX)
     void init_cos__(IUN, IREW)
#else
     void init_cos_(IUN, IREW) 
#endif
     int *IUN;       /* unit number of file to open */
     int *IREW;      /* rewind flag */
{
  
  char filename[9];
  int num1, num2, num3, num4, rval;
  void init_cbs();
  static FILE *lastfp = NULL;
  
  if (*IUN < 10 || *IUN > 999) {
    printf("\n +++ CINITCOS: Invalid unit number %d +++\n",*IUN);
    exit(1);
  }
  num1 = *IUN / 10;
  num2 = *IUN % 10;
  num3 = num1 / 10;
  num4 = num1 % 10;

  if(*IUN == 0) return;
  /* construct filename */
  filename[0] = 'f';
  filename[1] = 'o';
  filename[2] = 'r';
  filename[3] = 't';
  filename[4] = '.';
  if (*IUN <= 99) {
    filename[5] = num1 + 48;  /* convert to ascii */
    filename[6] = num2 + 48;  /* convert to ascii */
    filename[7] = '\0';
      }
  else {
    filename[5] = num3 + 48;  /* convert to ascii */
    filename[6] = num4 + 48;  /* convert to ascii */
    filename[7] = num2 + 48;  /* convert to ascii */
    filename[8] = '\0';
  }
  /*printf("iun=%d  num1=%d  num2=%d  num3=%d  num4=%d  filename=%s \n",*IUN,num1,num2,num3,num4,filename);*/
  
  if (head == NULL) {  /* start linked list of open files */
    open_files = (struct files *)malloc(sizeof(struct files));
    if (open_files == NULL) { /* error getting address */
      printf("\n+++OUT OF MEMORY IN CINITCOS+++\n");
      exit(-1);
    }
    head = open_files;
    open_files->next = NULL;
    open_files->unit = -99;
  } 
  open_files = head;
  while (open_files->next != NULL  &&  open_files->unit != *IUN) {
    open_files = open_files->next;
  }
  
  if (open_files->unit != *IUN) { /* file is not already in list;open it*/
    if(lastfp !=NULL) {
      /*      printf("+++ In CINITCOS: closing unit %d \n",lastfp);*/
      fclose(fp);
    }
    if (*IREW == 1) *IREW = 0;
    fp = fopen(filename,"r");
    /*    printf("+++ In CINITCOS: opening %s for reading fd= %d \n",filename,fp);*/
    if (fp == NULL) {
      printf("\n+++ERROR OPENING %s FOR READING+++ \n",filename);
      exit(-1);
    }
    lastfp = fp;
    cos_size = 0;  /* reset buffer position used in rdcosrec */
    init_cbs();
    if (open_files->unit > 0) {
      /* add new open file to list of open files */
      open_files->next = (struct files *) malloc(sizeof(struct files));
      if (open_files->next == NULL) {
	printf("\n+++UNABLE TO OPEN ANY MORE FILES+++\n");
	exit(-1);
      }
      open_files = open_files->next;
    }
    open_files->unit = *IUN;
    open_files->fps  = fp;
    open_files->next = NULL;
  }
  else {
    fp = open_files->fps;
    if (*IREW == 1) {
      rval = fseek(fp, 0, 0);  /* go to first volume */
      if (rval != 0) {
	printf("\n+++ERROR SEEKING ON %s +++\n",filename);
	exit(-1);
      }
      cos_size = 0;  /* reset buffer position used in rdcosrec */
      init_cbs();
    }
  }
  
  return;
}

/**************************************************************/
#if defined (IBMRISC) || defined (HP)
     void init_cosw(IUN)
#elif defined (CRAY)
     void INIT_COSW(IUN)
#elif defined (LINUX)
     void init_cosw__(IUN)
#else
     void init_cosw_(IUN) 
#endif
     int *IUN;       /* unit number of file to open */

{
  
  char filename[8];
  int num1, num2, rval;
  void init_cbsw();
  
  num1 = *IUN / 10;
  num2 = *IUN % 10;
  
  /* construct filename */
  filename[0] = 'f';
  filename[1] = 'o';
  filename[2] = 'r';
  filename[3] = 't';
  filename[4] = '.';
  filename[5] = num1 + 48;  /* convert to ascii */
  filename[6] = num2 + 48;  /* convert to ascii */
  filename[7] = '\0';

  wp = fopen(filename,"w");
  if (wp == NULL) {
    printf("\n+++ERROR OPENING %s FOR READING+++ \n",filename);
    exit(-1);
  }
  printf("\n opened %s",filename);
  cos_size = 0;  /* reset buffer position used in rdcosrec */
  init_cbsw();
  return;
}

/**************************************************************/
/* perform byte swapping for 32 bit words */
#if defined (IBMRISC) || defined (HP)
     void swap32(INJ, NUM)
#elif defined (CRAY)
     void SWAP32(INJ, NUM)
#elif defined (LINUX)
     void swap32_(INJ, NUM)
#else
     void swap32_(INJ,NUM)
#endif
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

/**************************************************************/
/* perform byte swapping for 64 bit words*/
#if defined (IBMRISC) || defined (HP)
     void swap64(INJ, NUM)
#elif defined (CRAY)
     void SWAP64(INJ, NUM)
#elif defined (LINUX)
     void swap64_(INJ, NUM)
#else
     void swap64_(INJ, NUM)
#endif
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

/**************************************************************/
/* swap characters for little Endian machines*/
#if defined (IBMRISC) || defined (HP)
     void swapchar(INJ, OUJ, NUM)
#elif defined (CRAY)
     void SWAPCHAR(INJ, OUJ, NUM)
#elif defined (LINUX)
     void swapchar_(INJ, OUJ, NUM)
#else
     void swapchar_(INJ, OUJ, NUM)
#endif
     int INJ[1], OUJ[1];
     int *NUM;
{
  int t1,t2,t3,t4;
  int c1,c2,c3,c4;
  int k,i;
  
  c1 = 0xff;
  c2 = c1 << 8;
  
  for (i = 0; i < *NUM; i++) {
    t1 = ((INJ[i] & c1)  & c1) << 8;
    t2 = (((INJ[i] & c2) >> 8)  & c1);
    OUJ[i] = t1 | t2 ;
  }
  
}
