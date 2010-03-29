#include <stdio.h>
#include <string.h>
#include "dorade.h"
#include <sys/types.h>
#include <unistd.h>


extern char arr_ryib[MXBYTES];
extern char arr_asib[MXBYTES];
extern char arr_frad[MXBYTES]; 
extern char arr_rdat[MXBYTES];
extern char arr_volhead[MXBYTES];
extern int  len_ryib;
extern int  len_asib;
extern int  len_frad;
extern int  len_rdat;
extern int  len_volhead;
extern int  skipping;
extern struct radd_blk *radds[MXRAD];
extern struct cspd_blk *cspds[MXRAD];
extern struct metadata *data_info[MXRAD];
extern int blknum;

/*
 * The following function reads in blocks, determines the type (name) and
 * calls a function for further processing, depending on the name. This
 * function assumes that the input stream is positioned at the start of
 * a dorade block.
 */
void read_block(fd, fpw, vold, radd, cfac, parm, cspd, celv, swib, ryib, 
                asib,frad, rdat, iprinth, iprintr, blknam, time, reqtime, 
                flddat, istat, iunpck, cfldnam, write_to_file,swapping,
                bytes_read)
     FILE *fpw;
     int fd;
     struct vold_blk *vold;
     struct radd_blk *radd;
     struct cfac_blk *cfac;
     struct parm_blk *parm;
     struct cspd_blk *cspd;
     struct celv_blk *celv;
     struct swib_blk *swib;
     struct ryib_blk *ryib;
     struct asib_blk *asib;
     struct frad_blk *frad;
     struct rdat_blk *rdat;
     int iprinth, iprintr;
     char blknam[5];
     float *time, *reqtime;
     float flddat[MXFLD][MXGAT];
     int *istat;
     int iunpck;
     char cfldnam[MXFLD][9];
     int *write_to_file;
     int *swapping;
     int *bytes_read;
{
  
  int rval, len, iread, ival, ij,slen;
  static int radd_cnt = 0;
  static int radd_cur = 0;
  static int parm_cnt = 0;
  static int blocking = 0;
  static int sweep_files = 0;
  static int first_vold = 0;
  static int ray_cnt  = 0;
  static int previous_descriptor;
  int it1,it2,it3,it4;
  int i,swap;
  int descriptor_found;
  char array[MXBYTES];
  char hold[4];
  char ctemp[8];
  struct vold_blk read_vold();
  struct radd_blk read_radd();
  struct cfac_blk read_cfac();
  struct parm_blk read_parm();
  struct cspd_blk read_cspd();
  struct celv_blk read_celv();
  struct swib_blk read_swib();
  struct ryib_blk read_ryib();
  struct asib_blk read_asib();
  struct frad_blk read_frad();
  struct rdat_blk read_rdat();
  void   read_sswb();
  long cswap32();
  unsigned short cswap16();
  int    bytes;
  double spacing;


  blknam[4]='\0';

  
  rval = myread(array, 8, fd);
  if (rval <= 0) {
    rval = myread(array, 8, fd);
    if (rval <= 0) {
      *istat = 3;
      return;
    }
  } 
  descriptor_found = check_for_descriptor(array,fd,blknam);
  if(descriptor_found == 0){
         printf("END OF DATA FILE\n");
         *istat = 3;
         return;
  }
  else if(descriptor_found == -1){
         printf("ERROR READING DATA FILE\n");
         exit(-1);
  }

  memcpy(ctemp, array, 8);
  /*printf("%s\n",blknam);*/


  if (strcmp(blknam, "VOLD") == 0) {
    *vold = read_vold(fd, array, *vold, iprinth, istat,&swap);
    /* turn on flag to create a new dorade output volume
     * if following conditions are met; used to extract a subset
     * of a larger dorade dataset
     */
    if (*istat == 2 && skipping == 0 && fpw) *write_to_file = 1;
    if (*istat > 2) return;

    if(swap == 0 && *swapping == 1) *swapping = 0;
    if(swap == 1 && *swapping == 0) *swapping = 1;
    /* reset counters so that radar params will be saved correctly */
    radd_cnt = -1;
    parm_cnt = -1;
    for (i = 0; i < MXRAD; i++) {
      data_info[i]->numflds = 0;
    }
    cfac->azim = 0.0;
    cfac->elev = 0.0;
    if((first_vold) == 1 && (sweep_files == 1)) *istat = 2;
    if(first_vold == 0) first_vold = 1;
    if (*istat == -2) return;
    if (*write_to_file) {
      len_volhead = 0;
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      memcpy(&(arr_volhead[len_volhead]), ctemp, 8);
      len_volhead += 8;
      memcpy(&(arr_volhead[len_volhead]), array, (len - 8));
      len_volhead += (len - 8);
    }
  }

  else if (strcmp(blknam, "RADD") == 0) {
    *radd = read_radd(fd, array, *radd, iprinth, istat);
    if (*istat > 2) return;
    /* save radar parms for this radar */
    radd_cnt++;
    if ((radd_cnt + 1) > MXRAD) {
      printf("\n  +++Too many radars in volume. Max. allowed = %d\n",
	     MXRAD);
      *istat = 4;
      return;
    }
    parm_cnt = -1;
    strcpy(radds[radd_cnt]->radnam, radd->radnam);
    radds[radd_cnt]->radcon    = radd->radcon;
    radds[radd_cnt]->scan_mode = radd->scan_mode;
    radds[radd_cnt]->rad_type  = radd->rad_type;
    radds[radd_cnt]->data_comp = radd->data_comp;
    radds[radd_cnt]->scan_rate = radd->scan_rate;
    radds[radd_cnt]->tot_par   = radd->tot_par;
    radds[radd_cnt]->rad_long  = radd->rad_long;
    radds[radd_cnt]->rad_lat   = radd->rad_lat;
    radds[radd_cnt]->rad_alt   = radd->rad_alt;
    radds[radd_cnt]->nyq       = radd->nyq;
    radds[radd_cnt]->max_range = radd->max_range;
    radds[radd_cnt]->num_freq  = radd->num_freq;
    radds[radd_cnt]->num_ip    = radd->num_ip;
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      memcpy(&(arr_volhead[len_volhead]), ctemp, 8);
      len_volhead += 8;
      memcpy(&(arr_volhead[len_volhead]), array, (len - 8));
      len_volhead += (len - 8);
    }
  }

  else if (strcmp(blknam, "PARM") == 0) {
    *parm = read_parm(fd, array, *parm, iprinth, istat);
    if (*istat > 2) return;
    /* save field info for this radar */
    parm_cnt++;
    if ((parm_cnt + 1) > MXFLD) {
      printf("\n   +++Error. Too many fields in dataset. Max. allowed = %d\n",
	     MXFLD);
      exit(1);
    }
    strcpy(data_info[radd_cnt]->fields[parm_cnt], parm->name);
    data_info[radd_cnt]->fld_typ[parm_cnt] = parm->parm_type;
    data_info[radd_cnt]->scale[parm_cnt]   = parm->scale;
    data_info[radd_cnt]->offset[parm_cnt]  = parm->offset;
    data_info[radd_cnt]->bad[parm_cnt]     = parm->bad;
    data_info[radd_cnt]->numflds++;
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      memcpy(&(arr_volhead[len_volhead]), ctemp, 8);
      len_volhead += 8;
      memcpy(&(arr_volhead[len_volhead]), array, (len - 8));
      len_volhead += (len - 8);
    }
  }
  else if (strcmp(blknam, "CELV") == 0) {
    swap = *swapping;
    *celv = read_celv(fd, array, *celv, iprinth, istat,swap,&spacing);
    if (*istat > 2) return;
    data_info[radd_cnt]->ngates  = 0;   /* will be set in read_rdat */
    data_info[radd_cnt]->frstgat = (float)celv->dist_to_fir;
    data_info[radd_cnt]->gatspac = (float)spacing;
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      memcpy(&(arr_volhead[len_volhead]), ctemp, 8);
      len_volhead += 8;
      memcpy(&(arr_volhead[len_volhead]), array, (len - 8));
      len_volhead += (len - 8);
    }
  }
  else if (strcmp(blknam, "CFAC") == 0) {
    len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
    if(swap == 1){
       slen = cswap32((long)len);
       len = slen;
    }
    *cfac = read_cfac(fd, array, *cfac, iprinth, istat, swap);
    if (*istat > 2) return;
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      memcpy(&(arr_volhead[len_volhead]), ctemp, 8);
      len_volhead += 8;
      memcpy(&(arr_volhead[len_volhead]), array, (len - 8));
      len_volhead += (len - 8);
    }
  }
  else if (strcmp(blknam, "CSPD") == 0) {
    len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
    if(swap == 1){
       slen = cswap32((long)len);
       len = slen;
    }    
    *cspd = read_cspd(fd, array, *cspd, iprinth, istat, swap);
    if (*istat > 2) return;
    data_info[radd_cnt]->ngates = 0;
    for (i = 0; i < cspd->num_seg; i++) {
      data_info[radd_cnt]->ngates += cspd->num_cell[i];
      if (i > 0 && cspd->num_cell[i] > 0 && cspd->spacing[i] != 
	  cspd->spacing[i-1] ) {
	printf("\n     +++GATE SPACING NOT CONSTANT IN A GIVEN BEAM+++\n");
	exit(1);
      }
    }
    data_info[radd_cnt]->frstgat = cspd->dist_to_fir;
    data_info[radd_cnt]->gatspac = cspd->spacing[0];
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      memcpy(&(arr_volhead[len_volhead]), ctemp, 8);
      len_volhead += 8;
      memcpy(&(arr_volhead[len_volhead]), array, (len - 8));
      len_volhead += (len - 8);
    }
  }
/****************************************************************************/
  else if (strcmp(blknam, "SSWB") == 0) {
    read_sswb(fd, array,iprinth, istat, &bytes);
    sweep_files = 1;    
    if (*istat > 2) return;
  }
/****************************************************************************/
  else if (strcmp(blknam, "SWIB") == 0) {
    *swib = read_swib(fd, array, *swib, iprinth, istat);
    radd_cur = -1;
    ray_cnt = 0;
    for (i = 0; i <= radd_cnt; i++) {
      if (strncmp(swib->radname, radds[i]->radnam,4) == 0) {
	radd_cur = i;
	break;
      }
    }
    if (radd_cur == -1) {
      printf("\n  +++Error finding radar %s in list.+++\n", swib->radname);
      exit(1);
    }
    if (*istat > 2) return;
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
    } 
  }
/****************************************************************************/
  else if (strcmp(blknam, "RYIB") == 0) {
    ray_cnt++;
    iread = 0;
    *ryib = read_ryib(fd, array, iread, *ryib, iprintr, istat, &bytes);
    if (*istat > 2) return;
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
    }/**write_to_file*/
  }
/****************************************************************************/
  else if (strcmp(blknam, "ASIB") == 0) {
    *asib = read_asib(fd, array, *asib, iprintr, istat ,&bytes);
    if (*istat > 2) return;
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
/*      memcpy(arr_asib, ctemp, 8);
      memcpy(&(arr_asib[8]), array, (len - 8));
      len_asib = len;
*/
    }
  }
  else if (strcmp(blknam, "FRAD") == 0) {
    len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
    if(swap == 1){
       slen = cswap32((long)len);
       len = slen;
    }
    /* in order to speed up for skipping purposes, only unpack data if
     * we are in requested time window 
     */
    *time = ryib->hour*10000. + ryib->min*100 + ryib->sec + ryib->msec/1000. ;
    if (*time < *reqtime) iunpck = 0;
    *frad = read_frad(fd, array, *frad, iprintr, iunpck, flddat, istat, swap);
    if (*istat > 2) return;
    if (*write_to_file) {
      memcpy(arr_frad, ctemp, 8);
      memcpy(&(arr_frad[8]), array, (len - 8));
      len_frad = len;
    }
  }
/**********************************************************************/
  else if (strcmp(blknam, "RDAT") == 0) {
     swap = *swapping;
    /* in order to speed up for skipping purposes, only unpack data if
     * we are in requested time window 
     */
    *time = ryib->hour*10000. + ryib->min*100 + ryib->sec + ryib->msec/1000. ;
    if (*time < *reqtime) iunpck = 0;

    *rdat = read_rdat(fd, array, *rdat, iprintr, iunpck, flddat, istat,
		      radd_cur,swap,&bytes);
    *bytes_read = bytes;
    if (*istat > 2) return;
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
    }
  }    
/**********************************************************************/
  else if (strcmp(blknam, "RYIB") == 0) {
    len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
    if(swap == 1){
       slen = cswap32((long)len);
       len = slen;
    } 
    iread = 1;
    *ryib = read_ryib(fd, array, iread, *ryib, iprintr, istat);
    if (*istat > 2) return;
    iread = 0;
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
    }
  }
  else if (strcmp(blknam, "COMM") == 0) {
    len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
    iread = 1;
    read_comm(fd, array, iprintr, istat, blocking);
    if (*istat > 2) return;
    iread = 0;
    sweep_files = 0;
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
    }
  }

  /*
   *The follwing descriptors are in the ELDORA field tape only.
   */
  else if (strcmp(blknam, "WAVE") == 0) {
    len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
    iread = 1;
    read_wave(fd, array, iread, iprintr, istat);
    if (*istat > 2) return;
    iread = 0;
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
    }
  }

 /************************Navigation descriptor***********************/
  else if (strcmp(blknam, "NDDS") == 0){
    iread = 1;
    read_navigation(fd, array, iread, iprintr, istat);
    if (*istat > 2) return;
    iread = 0;
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
    }
  }
 /**RKTB record signaling an end of sweep for sweep files****************/
 else if (strcmp(blknam, "RKTB") == 0){  
     read_rktb(fd, array, istat);
     *istat = -3;  /*the end of a sweep file*/
     sweep_files = 0;
     return;
 }
 /**NULL record signaling an end of sweep for sweep files****************/
 else if (strcmp(blknam, "NULL") == 0){  
     lseek(fd,0,SEEK_END);
     *istat = -3;  /*the end of a sweep file*/
     sweep_files = 0;
     return;
 }
     
  /************************************************************************/
 else {
    len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
    if(swap == 1){
       slen = cswap32((long)len);
       len = slen;
    } 
    if (len <= 0 || len > 1000000) {/* kludge to deal with record filler in data */
      len = find_ryib(fd, blknam, array, len, istat, ctemp);
      if (*istat > 2) return;
      blknam[0]='R';
      blknam[1]='Y';
      blknam[2]='I';
      blknam[3]='B';
      iread = 1;
      *ryib = read_ryib(fd, array, iread, *ryib, iprintr, istat);
      if (*istat > 2) return;
      iread = 0;
      if (*write_to_file) {
	ival = fwrite(ctemp, 1, 8, fpw);
	if (ival <= 0) {
	  printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	  exit(-1);
	}
	ival = fwrite(array, 1, (len - 8), fpw);
	if (ival <= 0) {
	  printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	  exit(-1);
	}
      }
    }
    else {
      rval = myread(array, (len-8), fd);
      if (rval <= 0) {
	rval = myread(array, (len-8), fd);
	if (rval <= 0) {
	  *istat = 4;
	  return;
	}
      }
      blknum++;
      if (iprinth || iprintr)
      printf("\n%s descriptor found, but not dumped. length=%d bytes\n",blknam,len); 
    }
  }    
    return;
}
/************************************************************************/
int check_for_descriptor(char array[MXBYTES],int fd, char blknam[5])
{

  /*If the data file was made by the RDSS translators then there may
    be four bytes at the beginning and end of a group of descriptors.
    This subroutine checks to make sure we are at a descriptor.  If
    there have been more than two reads then there is an error.  For 
    example:
            bbbbCOMMbbbbbbbbVOLD RADD PARAM'S CELV CFACbbbbbbbbSWIB
  */

  int good;  /*1 yes we have a descriptor, 0 no we don't have a descriptor*/
  int count,rval,ii;
  char hold[8];

  /*Check for the descriptor in the first array element*/
    if((array[0] == 'V') || (array[0] == 'S') || (array[0] == 'R') ||
       (array[0] == 'F') || (array[0] == 'P') || (array[0] == 'C') ||
       (array[0] == 'A') || (array[0] == 'W') || (array[0] == 'M') ||
       (array[0] == 'I') || (array[0] == 'L') || (array[0] == 'N') ||
       (array[0] == 'T')){/*no blocking was present*/
                         blknam[0] = array[0];
                         blknam[1] = array[1]; 
                         blknam[2] = array[2];
                         blknam[3] = array[3];
                         good = 1;
                         return(good);
    }


    /*Check for four byte blocking.  If blocking is present then
     *load the blknam array and read the next four bytes to get the
     *descriptor lenght information.  Load array to contain the correct
     *information in the correct order.
     */  
    else if((array[4] == 'V') || (array[4] == 'S') || (array[4] == 'R') ||
            (array[4] == 'F') || (array[4] == 'P') || (array[4] == 'C') ||
            (array[4] == 'A') || (array[4] == 'W') || (array[4] == 'M') ||
            (array[4] == 'I') || (array[4] == 'L') || (array[4] == 'N') ||
            (array[4] == 'T')){ 
               blknam[0] = array[4];
               blknam[1] = array[5]; 
               blknam[2] = array[6];
               blknam[3] = array[7]; 
               rval = myread(hold, 4, fd);
               if (rval <= 0) {
                   rval = myread(hold , 4, fd);
                   if (rval == 0) {
                       good = 0;
                       return(good);
                   }
                   if(rval == -1){
                       good = -1;
                       return(good);
		   }
               } 
               array[0] = blknam[0];
               array[1] = blknam[1];  
               array[2] = blknam[2];
               array[3] = blknam[3];
               array[4] = hold[0];
               array[5] = hold[1];  
               array[6] = hold[2];
               array[7] = hold[3]; 
               good = 1;
               return(good);
     }

    /*
     *In some cases there are eight bytes between the descriptors.
     *Read until we find a descriptor. 
     */
    else{
         count = 0;
         for(;;){
              rval = myread(array, 1, fd);
              if(rval == 0){
                 good = 0;
                 return(good);
	      }
	      if(rval < 0){
                 good = -1;
                 return(good);
	      }            

             if((array[0] == 'V') || (array[0] == 'S') || (array[0] == 'R') ||
                (array[0] == 'F') || (array[0] == 'P') || (array[0] == 'C') ||
                (array[0] == 'A') || (array[0] == 'W') || (array[0] == 'M') ||
                (array[0] == 'I') || (array[0] == 'L') || (array[0] == 'N') ||
                (array[0] == 'T')){/*we have found a descriptor*/
                    blknam[0] = array[0];
                    rval = myread(hold,7,fd);
                    if(rval == 0){
                       good = 0;
                       return(good);
		     }
		    if(rval < 0){
                       good = -1;
                       return(good);
		    }
                     array[1] = hold[0]; 
                     array[2] = hold[1];
                     array[3] = hold[2];
                     array[4] = hold[3];
                     array[5] = hold[4]; 
                     array[6] = hold[5];
                     array[7] = hold[6];
                     blknam[1] = array[1];
                     blknam[2] = array[2];
                     blknam[3] = array[3];
                     good = 1;
                     return(good);
	     }
             /*
              *In theory there should only be eight bytes between descriptrs
              *at max but read 20 and then bail out.
              */
             count++;
             if(count > 20){
                good = 0;
                return(good);
	     }
	 }/*for(;;)*/
    }

}/*check_for_descriptor*/


/***************************************************************/
