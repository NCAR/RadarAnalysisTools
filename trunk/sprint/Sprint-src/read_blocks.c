#include <stdio.h>
#include <string.h>
#include "dorade.h"
#define MAX(a,b)  ( ((a) > (b)) ? (a) : (b) )

#define  offs_ndx 0
#define  id_ndx 1
#define  rpt_ndx 4
#define  skp_ndx 5
#define  ndx_count 7
#define  sparc_ndx 6
#define  type_long  3
#define  type_double  5
#define  type_char  1
#define  type_float  4
#define  type_short  2
#define  type_int  6

int blknum = 0;
extern struct metadata *data_info[MXRAD];
extern struct radd_blk *radds[MXRAD];
extern struct cspd_blk *cspds[MXRAD];

/* reads in an actual data block */
struct rdat_blk read_rdat(fd, array, rdat, iprint, iunpck, flddat, istat,
			  rnum,swap,bytes) 
     int fd;
     char array[MXBYTES];
     struct rdat_blk rdat;
     int iprint, iunpck;
     float flddat[MXFLD][MXGAT];
     int *istat, rnum,swap,*bytes;
{
  int rval, len, i, j,bad;
  int ngates, numflds, fnum, num_bytes, isize, it1;
  static int rdatcnt = 0;
  short  sdata[MXGAT];
  char   cdata[MXBYTES];
  int    final_bad_count = 0;
  int    n,slen;

  if(swap == 0)
     len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
  else 
     len = four_bytes_2_int(array[7],array[6],array[5],array[4]); 

  *bytes = *bytes + len;
  rval = myread(array, (len-8), fd);
  if (rval <= 0) {
    rval = myread(array, (len-8), fd);
    if (rval <= 0) {
      *istat = 3;
      return rdat;
    }
  }

  for(i = 0; i < len-8; i++){
      cdata[i] = array[i + 8];
  }

  /* do some unpacking */
  strncpy(rdat.par_name, &(array[0]), 8);
  rdat.par_name[8] = '\0';
  numflds = data_info[rnum]->numflds;
  bad     = (int)data_info[rnum]->bad[0];
  
  fnum = -1;
  for (i = 0; i < numflds; i++) {
    if (strcmp(data_info[rnum]->fields[i], rdat.par_name) == 0) {
      fnum = i;
      break;
    }
  }
  if (fnum == -1) {
    printf("\n   +++Error. Could not find field %s in list+++\n",
	   rdat.par_name);
    exit(1);
  }
  switch (data_info[rnum]->fld_typ[fnum])
    {
    case 1:
      num_bytes = 1;
      break;
    case 2:
      num_bytes = 2;
      break;
    case 3:
      num_bytes = 3;
      break;
    case 4:
      num_bytes = 4;
      break;
    default:
      printf("\n   Error: unrecognized field length. type = %s\n", 
	     data_info[rnum]->fld_typ[fnum]);
      exit(-1);
      break;
    }
   ngates = (len-16)/num_bytes;

  rdatcnt++;
  if (rdatcnt <= numflds){
    data_info[rnum]->ngates = MAX(data_info[rnum]->ngates, ngates);
  }
  else {
    rdatcnt = 1;
    data_info[rnum]->ngates = ngates;
  }

  /* initialize array to bad */
  for (j = 0; j < MXGAT; j++) {
    flddat[fnum][j] = (float) bad;
  }

  /*If the data compression flag is turned on then we need to uncompress
   *the data. sdata is a short array which will contain the uncompressed data.
   *cdata is the character array that contains the original compressed data.
   */

  if(radds[rnum]->data_comp == 1){
    if(swap == 0){
        n = uncompress((short *)cdata,sdata,bad,&final_bad_count,
                                  480);
    } 
    else{
        n = LEuncompress((short *)cdata,sdata,bad,&final_bad_count,
                                    480);
    }
     data_info[rnum]->ngates = n;
     ngates = n;
  }  

  if(data_info[rnum]->ngates > MXGAT){
     printf("THE NUMBER OF GATES PRESENT IN THE DATA FILE %d IS GREATER THAN SPRINT WILL ALLOW %d\n",
            data_info[rnum]->ngates,MXGAT);
     exit(-1);
  } 
               
  /* read in the data */
  blknum++;
  if (iunpck) {
    if (iunpck == 1) { /* leave in scaled format */
      for (j = 0; j < ngates; j++) {
        if(radds[rnum]->data_comp != 1){
	   flddat[fnum][j] = (float) two_bytes_2_int(array[8+j*2], array[9+j*2]);
        }
        else{
           flddat[fnum][j] = sdata[j];
	}
      }
    }
    else { /* convert to meteorological units */
      for (j = 0; j < ngates; j++) {
        if(radds[rnum]->data_comp != 1){ 
	   it1 = two_bytes_2_int(array[8+j*2], array[9+j*2]);
        }
        else{
           it1 = sdata[j];
	}
	if (data_info[rnum]->scale[fnum] != 0.0 && it1 != bad) 
	  flddat[fnum][j] = ((float)it1 - data_info[rnum]->offset[fnum])/
	    data_info[rnum]->scale[fnum];
      }
    }

  }

  if (iprint) { 
    printf("\nRadar Data Block (dorade block # %d):\n",blknum);
    printf("\n\n    field=%8s    scale=%8.3f   offset=%8.3f      bad=%8.3f\n",data_info[rnum]->fields[fnum], data_info[rnum]->scale[fnum],data_info[rnum]->offset[fnum],data_info[rnum]->bad[fnum]);
      printf("data values = \n");
      for (j = 0 ; j < ngates; j++) {
	printf("%5.0f ",flddat[fnum][j]);
	if (((j+1) % 20) == 0) printf("\n");   /* new line every 20 datums */
      }
    }
  /*printf("\n");*/
  return rdat;
      
}

/***********************************************************************************/
/* The following function reads in the platform (aircraft/ship) info
 * block and prints it out.
 */
struct asib_blk read_asib(fd, array, asib, iprint, istat, bytes)
     int fd;
     char array[MXBYTES];
     struct asib_blk asib;
     int iprint;
     int *istat;
     int *bytes;
{
  int rval, ival, it1, it2, it3, it4, i;
  float four_bytes_2_flt();
  char printstr[1000];
  int len,swap;

  printstr[0] = '\0';
  swap = 0;
/* figure out length from four bytes already read in */  
   len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
   if(len > 1000000 || len < 0) {
     len = four_bytes_2_int(array[7],array[6],array[5],array[4]);      
     if (len > 1000000 || len < 0) {
         printf("error: invalid block length in read_asib. len=%d\n",len);
         exit(-1);
     }
     swap = 1;
   }

  *bytes = *bytes + len;
  rval = myread(array, (len-8), fd);
  if (rval <= 0) {
    rval = myread(array, (len-8), fd);
    if (rval <= 0) {
      *istat = 3;
      return asib;
    }
  }

/* unpack array into its parts */
  if(swap == 0){
     asib.lon       = four_bytes_2_flt(array[0],array[1],array[2],array[3]);
     asib.lat       = four_bytes_2_flt(array[4],array[5],array[6],array[7]);
     asib.pres_alt  = four_bytes_2_flt(array[8],array[9],array[10],array[11]);
     asib.alt_gnd   = four_bytes_2_flt(array[12],array[13],array[14],array[15]);
     asib.gndspd_ew = four_bytes_2_flt(array[16],array[17],array[18],array[19]);
     asib.gndspd_ns = four_bytes_2_flt(array[20],array[21],array[22],array[23]);
     asib.ver_vel   = four_bytes_2_flt(array[24],array[25],array[26],array[27]);
     asib.heading   = four_bytes_2_flt(array[28],array[29],array[30],array[31]);
     asib.roll      = four_bytes_2_flt(array[32],array[33],array[34],array[35]);
     asib.pitch     = four_bytes_2_flt(array[36],array[37],array[38],array[39]);
     asib.drift     = four_bytes_2_flt(array[40],array[41],array[42],array[43]);
     asib.rot_ang   = four_bytes_2_flt(array[44],array[45],array[46],array[47]);
     asib.tilt_ang  = four_bytes_2_flt(array[48],array[49],array[50],array[51]);
     asib.wndspd_ew = four_bytes_2_flt(array[52],array[53],array[54],array[55]);
     asib.wndspd_ns = four_bytes_2_flt(array[56],array[57],array[58],array[59]);
     asib.verspd    = four_bytes_2_flt(array[60],array[61],array[62],array[63]);
     asib.head_rate = four_bytes_2_flt(array[64],array[65],array[66],array[67]);
     asib.ptch_rate = four_bytes_2_flt(array[68],array[69],array[70],array[71]);
  }
  else{
     asib.lon       = four_bytes_2_flt(array[3],array[2],array[1],array[0]);
     asib.lat       = four_bytes_2_flt(array[7],array[6],array[5],array[4]);
     asib.pres_alt  = four_bytes_2_flt(array[11],array[10],array[9],array[8]);
     asib.alt_gnd   = four_bytes_2_flt(array[15],array[14],array[13],array[12]);
     asib.gndspd_ew = four_bytes_2_flt(array[19],array[18],array[17],array[16]);
     asib.gndspd_ns = four_bytes_2_flt(array[23],array[22],array[21],array[20]);
     asib.ver_vel   = four_bytes_2_flt(array[27],array[26],array[25],array[24]);
     asib.heading   = four_bytes_2_flt(array[31],array[30],array[29],array[28]);
     asib.roll      = four_bytes_2_flt(array[35],array[34],array[33],array[32]);
     asib.pitch     = four_bytes_2_flt(array[39],array[38],array[37],array[36]);
     asib.drift     = four_bytes_2_flt(array[43],array[42],array[41],array[40]);
     asib.rot_ang   = four_bytes_2_flt(array[47],array[46],array[45],array[44]);
     asib.tilt_ang  = four_bytes_2_flt(array[51],array[50],array[49],array[48]);
     asib.wndspd_ew = four_bytes_2_flt(array[55],array[54],array[53],array[52]);
     asib.wndspd_ns = four_bytes_2_flt(array[59],array[58],array[57],array[56]);
     asib.verspd    = four_bytes_2_flt(array[63],array[62],array[61],array[60]);
     asib.head_rate = four_bytes_2_flt(array[64],array[65],array[66],array[67]);
     asib.ptch_rate = four_bytes_2_flt(array[71],array[70],array[69],array[68]); 
  }


  blknum++;
  if (iprint) { /* print out info */
    printf("\nPlatform Info Block (dorade block # %d):\n",blknum);
    strcat(printstr,"     long=%8.3f      lat=%8.3f pres_alt=%8.3f  alt_gnd=%8.3f");
    strcat(printstr," gndsp_ew=%8.3f gndsp_ns=%8.3f  ver_vel=%8.3f\n");
    strcat(printstr,"  heading=%8.3f     roll=%8.3f    pitch=%8.3f    drift=%8.3f");
    strcat(printstr,"  rot_ang=%8.3f tilt_ang=%8.3f wndsp_ew=%8.3f\n");
    strcat(printstr," wndsp_ns=%8.3f   verspd=%8.3f  headchg=%8.3f");
    strcat(printstr," pitchchg=%8.3f\n");

    printf(printstr, asib.lon, asib.lat, asib.pres_alt, asib.alt_gnd,
           asib.gndspd_ew, asib.gndspd_ns, asib.ver_vel, asib.heading, asib.roll,
           asib.pitch, asib.drift, asib.rot_ang, asib.tilt_ang, asib.wndspd_ew,
           asib.wndspd_ns, asib.verspd, asib.head_rate, asib.ptch_rate);
  }

  return asib;
}

/******************************************************************************************/
/* The following function reads in the correction factor descriptor
 * block and prints out what it read in.
 */

struct cfac_blk read_cfac(fd, array, cfac, iprint, istat, swap)
     int fd;
     char array[MXBYTES];
     struct cfac_blk cfac;
     int iprint;
     int *istat;
     int swap;
{
  int rval, ival, it1, it2, it3, it4, i, len, slen;
  int index;
  char printstr[1000];
  float four_bytes_2_flt();
  float swap_floats();

  printstr[0]='\0';
/* get length of block as a two's complement integer from four bytes 
 * already read in.
 */
  len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
  if(swap == 1){
       slen = cswap32((long)len);
       len = slen;
  }

  if (len > 1000000 || len < 0) {
    printf("error: invalid block length in read_cfac. len=%d\n",len);
    exit(-1);
  }


  rval = myread(array, (len-8), fd);
  if (rval <= 0) {
    rval = myread(array, (len-8), fd);
    if (rval <= 0) {
      *istat = 3;
      return cfac;
    }
  }
  if(swap == 0){
     cfac.azim    = four_bytes_2_flt(array[0],array[1],array[2],array[3]);
     cfac.elev    = four_bytes_2_flt(array[4],array[5],array[6],array[7]);
     cfac.rng_del = four_bytes_2_flt(array[8],array[9],array[10],array[11]);
     cfac.lon     = four_bytes_2_flt(array[12],array[13],array[14],array[15]);
     cfac.lat     = four_bytes_2_flt(array[16],array[17],array[18],array[19]);
     cfac.prs_alt = four_bytes_2_flt(array[20],array[21],array[22],array[23]);
     cfac.alt     = four_bytes_2_flt(array[24],array[25],array[26],array[27]);
     cfac.gspd_ew = four_bytes_2_flt(array[28],array[29],array[30],array[31]);
     cfac.gspd_ns = four_bytes_2_flt(array[32],array[33],array[34],array[35]);
     cfac.ver_vel = four_bytes_2_flt(array[36],array[37],array[38],array[39]);
     cfac.heading = four_bytes_2_flt(array[40],array[41],array[42],array[43]);
     cfac.roll    = four_bytes_2_flt(array[44],array[45],array[46],array[47]);
     cfac.pitch   = four_bytes_2_flt(array[48],array[49],array[50],array[51]);
     cfac.drift   = four_bytes_2_flt(array[52],array[53],array[54],array[55]);
     cfac.rot_ang = four_bytes_2_flt(array[56],array[57],array[58],array[59]);
     cfac.tilt    = four_bytes_2_flt(array[60],array[61],array[62],array[63]);
  }
  else{

     cfac.azim    = four_bytes_2_flt(array[3],array[2],array[1],array[0]);;
     cfac.elev    = four_bytes_2_flt(array[7],array[6],array[5],array[4]);;
     cfac.rng_del = four_bytes_2_flt(array[11],array[10],array[9],array[8]);;
     cfac.lon     = four_bytes_2_flt(array[15],array[14],array[13],array[12]);
     cfac.lat     = four_bytes_2_flt(array[19],array[18],array[17],array[16]);
     cfac.prs_alt = four_bytes_2_flt(array[23],array[22],array[21],array[20]);
     cfac.gspd_ew = four_bytes_2_flt(array[31],array[30],array[29],array[28]);
     cfac.gspd_ns = four_bytes_2_flt(array[35],array[34],array[33],array[32]);
     cfac.ver_vel = four_bytes_2_flt(array[39],array[38],array[37],array[36]);
     cfac.heading = four_bytes_2_flt(array[43],array[42],array[41],array[40]);
     cfac.roll    = four_bytes_2_flt(array[47],array[46],array[45],array[44]);
     cfac.pitch   = four_bytes_2_flt(array[51],array[50],array[49],array[48]);
     cfac.drift   = four_bytes_2_flt(array[55],array[54],array[53],array[52]);
     cfac.rot_ang = four_bytes_2_flt(array[59],array[58],array[57],array[56]);
     cfac.tilt    = four_bytes_2_flt(array[63],array[62],array[61],array[60]);
}


  blknum++;
  if (iprint) {    /* now print out the info */
    printf("\nCorrection Factor Block (dorade block # %d):\n",blknum);
    strcat(printstr,"azi = %8.3f, elev = %8.3f, rng_delay = %8.3f, long = %8.3f,");
    strcat(printstr,"          lat = %8.3f, pres_alt = %8.3f, alt = %8.3f,");
    strcat(printstr," gspd_ew = %8.3f,          gspd_ns = %8.3f,");
    strcat(printstr," vert_vel = %8.3f, heading = %8.3f, roll = %8.3f,");
    strcat(printstr,"          pitch = %8.3f, drift = %8.3f, rot_ang = %8.3f,");
    strcat(printstr," tilt = %8.3f\n");

   printf(printstr, cfac.azim, cfac.elev, cfac.rng_del, cfac.lon, cfac.lat,
          cfac.prs_alt, cfac.alt, cfac.gspd_ew, cfac.gspd_ns, cfac.ver_vel, 
          cfac.heading, cfac.roll, cfac.pitch, cfac.drift, cfac.rot_ang, cfac.tilt);
  }


  return cfac;
}


/* The following function reads in a dorade comment block and prints it
 * out if requested.
 */
void read_comm(fd, array, iprint, istat, blocking)
     int fd;
     char array[MXBYTES];
     int iprint;
     int *istat;
     int blocking;
{
  int rval, ival;
  int len,bytes_to_read,count;

  /* convert four bytes of length to a two's complement integer */
  len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
  if (len > 1000000 || len < 0) {
      len = four_bytes_2_int(array[7],array[6],array[4],array[4]);
      if (len > 1000000 || len < 0) {      
	 printf("INVALID COMMENT DESCRIPTOR LENGTH %d\n",len);
         exit(-1);
      }
  }

  bytes_to_read = len - 8;
  rval = myread(array, bytes_to_read, fd);
  if (rval <= 0) {
    rval = myread(array, bytes_to_read, fd);
    if (rval <= 0) {
      *istat = 3;
      return;
    }
  }     
  array[len-8] = '\0';
  blknum++;
  if (iprint) printf("comment=%s\n",array);
  return;
}
  
/********************************************************************************/
struct celv_blk read_celv(fd, array, celv, iprint, istat,swap,spacing)
     int fd;
     char array[MXBYTES];
     struct celv_blk celv;
     int iprint;
     int *istat;
     int swap;
     double *spacing;
{
  int rval, len, i, slen;
  char printstr[1000];
  float dist;
  float four_bytes_2_flt();

  printstr[0]='\0';
/* get length of block as a two's complement integer from four bytes 
 * already read in.
 */
  if(swap == 0)
     len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
  else
     len = four_bytes_2_int(array[7],array[6],array[5],array[4]);

  rval = myread(array, (len-8), fd);
  if (rval <= 0) {
    rval = myread(array, (len-8), fd);
    if (rval <= 0) {
      *istat = 3;
      return celv;
    }
  }

  /* now unpack the elements of this block into the structure */
  if(swap == 0){
     celv.num_cells   = four_bytes_2_int(array[0],array[1],array[2],array[3]);
     celv.dist_to_fir = four_bytes_2_flt(array[4],array[5],array[6],array[7]);
     dist             = four_bytes_2_flt(array[8],array[9],array[10],
                        array[11]);
     *spacing     = dist - celv.dist_to_fir;
  }
  else{
     celv.num_cells   = four_bytes_2_int(array[3],array[2],array[1],array[0]);
     celv.dist_to_fir = four_bytes_2_flt(array[7],array[6],array[5],array[4]);
     dist             = four_bytes_2_flt(array[11],array[10],array[9],
                        array[8]);
     *spacing     = dist - celv.dist_to_fir;
  }     
  /* print out the info now */

  blknum++;
  if (iprint) {
    printf("\nCell Range Vector Block (dorade block # %d):\n",blknum);
    strcat(printstr,"  num_cells=%9d dist2fir=%f spacing=%f\n");
    printf(printstr, celv.num_cells, celv.dist_to_fir,*spacing);
  }
  return celv;
}

/* the following function reads in the dorade cell spacing descriptor.
 * This descriptor is not part of the dorade format itself, but is
 * used in the eldora field format.
 */
struct cspd_blk read_cspd(fd, array, cspd, iprint, istat, swap)
     int fd;
     char array[MXBYTES];
     struct cspd_blk cspd;
     int iprint;
     int *istat;
     int swap;
{
  int rval, len, i, slen;
  char printstr[1000];

  printstr[0]='\0';
/* get length of block as a two's complement integer from four bytes 
 * already read in.
 */
  len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
  if(swap == 1){
       slen = cswap32((long)len);
       len = slen;
  }
  if (len > 1000000 || len < 0) {
    printf("error: invalid block length in read_cspd. len=%d\n",len);
    exit(-1);
  }


  rval = myread(array, (len-8), fd);
  if (rval <= 0) {
    rval = myread(array, (len-8), fd);
    if (rval <= 0) {
      *istat = 3;
      return cspd;
    }
  }

/* now unpack all the elements of this block into the structure */

  if(swap == 1){
     cspd.num_seg = two_bytes_2_int(array[0],array[1]);
     cspd.dist_to_fir = two_bytes_2_int(array[2],array[3]);
  
     for (i = 0; i < 6; i++) {
       cspd.spacing[i] = two_bytes_2_int(array[4+(i*2)],array[5+(i*2)]);
     }

     for (i = 0; i < 6; i++) {
       cspd.num_cell[i] = two_bytes_2_int(array[16+(i*2)],array[17+(i*2)]);
     }
  }
  else{
     cspd.num_seg = two_bytes_2_int(array[1],array[0]);
     cspd.dist_to_fir = two_bytes_2_int(array[3],array[2]);     
     for (i = 0; i < 6; i++) {
       cspd.spacing[i] = two_bytes_2_int(array[5+(i*2)],array[4+(i*2)]);
     }

     for (i = 0; i < 6; i++) {
       cspd.num_cell[i] = two_bytes_2_int(array[17+(i*2)],array[16+(i*2)]);
     }
  }
  
/* print out the info now */
  
  blknum++;
  if (iprint) {
    printf("\nCell Spacing Block (dorade block # %d):\n",blknum);

    strcat(printstr,"  num_seg=%8d dist2fir=%8d    spac1=%8d    spac2=%8d");
    strcat(printstr,"    spac3=%8d    spac4=%8d    spac5=%8d\n    spac6=%8d");
    strcat(printstr,"     num1=%8d     num2=%8d     num3=%8d     num4=%8d");
    strcat(printstr,"     num5=%8d     num6=%8d\n");

    printf(printstr, cspd.num_seg, cspd.dist_to_fir, cspd.spacing[0], 
           cspd.spacing[1], cspd.spacing[2], cspd.spacing[3], cspd.spacing[4],
           cspd.spacing[5], cspd.num_cell[0], cspd.num_cell[1], cspd.num_cell[2],
           cspd.num_cell[3], cspd.num_cell[4], cspd.num_cell[5]);
  }

  return cspd;
}

/********************************************************************************/
/* The following function reads in the dorade parameter descriptor
 * block and prints out what it read in.
 */
struct parm_blk read_parm(fd, array, parm, iprint, istat)
     int fd;
     char array[MXBYTES];
     struct parm_blk parm;
     int iprint;
     int *istat;
{
  int rval, len, i, numflds,swap;
  float four_bytes_2_flt();
  char printstr[1000];

  printstr[0]='\0';

/* get length of block as a two's complement integer from four bytes 
 * already read in.
 */
  swap = 0;
  len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
  if(len < 0 || len > 1000){
     len = four_bytes_2_int(array[7],array[6],array[5],array[4]);
     if(len < 0 || len > 1000){
        printf("INVALID PARAMETER BLOCK LEN %d\n",len);
        exit(-1);
     }
     swap = 1;
  }

  rval = myread(array, (len-8), fd);
  if (rval <= 0) {
    rval = myread(array, (len-8), fd);
    if (rval <= 0) {
      *istat = 3;
      return parm;
    }
  }

/* now unpack all the elements of this block into the structure */

  parm.name[8]='\0';
  for (i = 0; i < 8; i++) {
    parm.name[i] = array[i];
  }
  
  parm.desc[8]='\0';
  for (i = 0; i < 8; i++) {
    parm.desc[i] = array[8+i];
  }

  parm.units[8] = '\0';
  for (i = 0; i < 8; i++) {
    parm.units[i] = array[48+i];
  }

  parm.thr_field[8] = '\0';
  for (i = 0; i < 8; i++) {
    parm.thr_field[i] = array[72+i];
  }

  if(swap == 0){
     parm.ipp       = two_bytes_2_int(array[56],array[57]);
     parm.tran_freq = two_bytes_2_int(array[58],array[59]);
     parm.rec_band  = four_bytes_2_flt(array[60],array[61],array[62],
                                       array[63]);
     parm.pul_wid   = two_bytes_2_int(array[64],array[65]);
     parm.polar     = two_bytes_2_int(array[66],array[67]);
     parm.num_sam   = two_bytes_2_int(array[68],array[69]);
     parm.parm_type = two_bytes_2_int(array[70],array[71]);
     parm.thr_value = four_bytes_2_flt(array[80],array[81],array[82],
                                       array[83]);
     parm.scale     = four_bytes_2_flt(array[84],array[85],array[86],
                                       array[87]);
     parm.offset    = four_bytes_2_flt(array[88],array[89],array[90],
                                       array[91]);
     parm.bad       = four_bytes_2_int(array[92],array[93],array[94],
                                       array[95]);
  }
  else{
     parm.ipp       = two_bytes_2_int(array[57],array[56]);
     parm.tran_freq = two_bytes_2_int(array[59],array[58]);
     parm.rec_band  = four_bytes_2_flt(array[63],array[62],array[61],
                                       array[60]);
     parm.pul_wid   = two_bytes_2_int(array[65],array[64]);
     parm.polar     = two_bytes_2_int(array[67],array[66]);
     parm.num_sam   = two_bytes_2_int(array[69],array[68]);
     parm.parm_type = two_bytes_2_int(array[71],array[70]);
     parm.thr_value = four_bytes_2_flt(array[83],array[82],array[81],
                                       array[80]);
     parm.scale     = four_bytes_2_flt(array[87],array[86],array[85],
                                       array[84]);
     parm.offset    = four_bytes_2_flt(array[91],array[90],array[89],
                                       array[88]);
     parm.bad       = four_bytes_2_int(array[95],array[94],array[93],
                                       array[92]); 
  }    
     

/* now print the information out */  

  blknum++;
  if (iprint) {
    printf("\nParameter Descriptor Block (dorade block # %d):\n",blknum);
    strcat(printstr,"     name=%8s     desc=%8s    units=%8s      ipp=%8d");
    strcat(printstr," tranfreq=%8d rec_band=%8.3f  pul_wid=%8d\n    polar=%8d");
    strcat(printstr,"  num_sam=%8d  parmtyp=%8d  thr_fld=%8s  thr_val=%8.3f");
    strcat(printstr,"    scale=%8.3f   offset=%8.3f\n   badval=%8d\n");

    printf(printstr ,parm.name, parm.desc, parm.units, parm.ipp, parm.tran_freq,
           parm.rec_band, parm.pul_wid, parm.polar, parm.num_sam, parm.parm_type,
           parm.thr_field, parm.thr_value, parm.scale, parm.offset, parm.bad);
  }

  return parm;
}
/********************************************************************************/

/* The following function reads in the radar descriptor block and prints
 * out what it read in.
 */
struct radd_blk read_radd(fd, array, radd, iprint, istat)
     int fd;
     char array[MXBYTES];
     struct radd_blk radd;
     int iprint;
     int *istat;
{
  int rval, ival, it1, it2, it3, it4, i,index,aindex;
  float four_bytes_2_flt();
  char printstr[1000];
  int len,swap;
  unsigned short radd_value;
  extern unsigned short cswap16();
  unsigned char  temp[MXBYTES];
  long  tlong;


  printstr[0]='\0';
  swap = 0;
  len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
  if(len < 0 || len > 10000){
     len = four_bytes_2_int(array[7],array[6],array[5],array[4]);
     if(len < 0 || len > 10000){
        printf("Invalid lenght for RADD descriptor block %d\n",len);
        exit(-1);
     }
     swap = 1;
  }


  rval = myread(array, (len-8), fd);
  if (rval <= 0) {
    rval = myread(array, (len-8), fd);
    if (rval <= 0) {
      *istat = 3;
      return radd;
    }
  }


/* unpack array into its discrete parts (variables) */
  radd.radnam[8] = '\0';
  for (i = 0; i < 8; i++) {
    radd.radnam[i] = ' ';
    radd.radnam[i] = array[i];
  }
  if(swap == 0){
     radd.radcon    = four_bytes_2_flt(array[8],array[9],array[10],array[11]);
     radd.nompkpw   = four_bytes_2_flt(array[12],array[13],array[14],array[15]);
     radd.nomnspw   = four_bytes_2_flt(array[16],array[17],array[18],array[19]);
     radd.rcvgain   = four_bytes_2_flt(array[20],array[21],array[22],array[23]);
     radd.antgain   = four_bytes_2_flt(array[24],array[25],array[26],array[27]);
     radd.radgain   = four_bytes_2_flt(array[28],array[29],array[30],array[31]);
     radd.horbmwth  = four_bytes_2_flt(array[32],array[33],array[34],array[35]);
     radd.verbmwth  = four_bytes_2_flt(array[36],array[37],array[38],array[39]);
     radd.rad_type  = two_bytes_2_int(array[40],array[41]);
     radd.scan_mode = two_bytes_2_int(array[42],array[43]);
     radd.scan_rate = four_bytes_2_flt(array[44],array[45],array[46],array[47]);
     radd.start_ang = four_bytes_2_flt(array[48],array[49],array[50],array[51]);
     radd.stop_ang  = four_bytes_2_flt(array[52],array[53],array[54],array[55]);
     radd.tot_par   = two_bytes_2_int(array[56],array[57]);
     radd.tot_des   = two_bytes_2_int(array[58],array[59]);
     radd.data_comp = two_bytes_2_int(array[60],array[61]);
     radd.data_red  = two_bytes_2_int(array[62],array[63]);
     radd.data_red1 = four_bytes_2_flt(array[64],array[65],array[66],array[67]);
     radd.data_red2 = four_bytes_2_flt(array[68],array[69],array[70],array[71]);
     radd.rad_long  = four_bytes_2_flt(array[72],array[73],array[74],array[75]);
     radd.rad_lat   = four_bytes_2_flt(array[76],array[77],array[78],array[79]);
     radd.rad_alt   = four_bytes_2_flt(array[80],array[81],array[82],array[83]);
     radd.nyq       = four_bytes_2_flt(array[84],array[85],array[86],array[87]);
     radd.max_range = four_bytes_2_flt(array[88],array[89],array[90],array[91]);
     radd.num_freq  = two_bytes_2_int(array[92],array[93]);
     radd.num_ip    = two_bytes_2_int(array[94],array[95]);
     radd.freq1 = four_bytes_2_flt(array[96],array[97],array[98],array[99]);
     radd.freq2 = four_bytes_2_flt(array[100],array[101],array[102],array[103]);
     radd.freq3 = four_bytes_2_flt(array[104],array[105],array[106],array[107]);
     radd.freq4 = four_bytes_2_flt(array[108],array[109],array[110],array[111]);
     radd.freq5 = four_bytes_2_flt(array[112],array[113],array[114],array[115]);
     radd.ipp1  = four_bytes_2_flt(array[116],array[117],array[118],array[119]);
     radd.ipp2  = four_bytes_2_flt(array[120],array[121],array[122],array[123]);
     radd.ipp3  = four_bytes_2_flt(array[124],array[125],array[126],array[127]);
     radd.ipp4  = four_bytes_2_flt(array[128],array[129],array[130],array[131]);
     radd.ipp5  = four_bytes_2_flt(array[132],array[133],array[134],array[135]);
  }
  else{
     radd.radcon    = four_bytes_2_flt(array[11],array[10],array[9],array[8]);
     radd.nompkpw   = four_bytes_2_flt(array[15],array[14],array[13],array[12]);
     radd.nomnspw   = four_bytes_2_flt(array[19],array[18],array[17],array[16]);
     radd.rcvgain   = four_bytes_2_flt(array[23],array[22],array[21],array[20]);
     radd.antgain   = four_bytes_2_flt(array[27],array[26],array[25],array[24]);
     radd.radgain   = four_bytes_2_flt(array[31],array[30],array[29],array[28]);
     radd.horbmwth  = four_bytes_2_flt(array[35],array[34],array[33],array[32]);
     radd.verbmwth  = four_bytes_2_flt(array[39],array[38],array[37],array[36]);
     radd.rad_type  = two_bytes_2_int(array[41],array[40]);
     radd.scan_mode = two_bytes_2_int(array[43],array[42]); 
     radd.scan_rate = four_bytes_2_flt(array[47],array[46],array[45],array[44]);
     radd.start_ang = four_bytes_2_flt(array[51],array[50],array[49],array[48]);
     radd.stop_ang  = four_bytes_2_flt(array[55],array[54],array[53],array[52]);
     radd.tot_par   = two_bytes_2_int(array[57],array[56]);
     radd.tot_des   = two_bytes_2_int(array[59],array[58]);
     radd.data_comp = two_bytes_2_int(array[61],array[60]);
     radd.data_red  = two_bytes_2_int(array[63],array[62]);
     radd.data_red1 = four_bytes_2_flt(array[67],array[66],array[65],array[64]);
     radd.data_red2 = four_bytes_2_flt(array[71],array[70],array[69],array[68]);
     radd.rad_long  = four_bytes_2_flt(array[75],array[74],array[73],array[72]);
     radd.rad_lat   = four_bytes_2_flt(array[79],array[78],array[77],array[76]);
     radd.rad_alt   = four_bytes_2_flt(array[83],array[82],array[81],array[80]);
     radd.nyq       = four_bytes_2_flt(array[87],array[86],array[85],array[84]);
     radd.max_range = four_bytes_2_flt(array[91],array[90],array[89],array[88]);
     radd.num_freq  = two_bytes_2_int(array[93],array[92]);
     radd.num_ip    = two_bytes_2_int(array[95],array[94]);
     radd.freq1     = four_bytes_2_flt(array[99],array[98],array[97],array[96]);
     radd.freq2     = four_bytes_2_flt(array[103],array[102],array[101],array[100]);
     radd.freq3     = four_bytes_2_flt(array[107],array[106],array[105],array[104]);
     radd.freq4     = four_bytes_2_flt(array[111],array[110],array[109],array[108]);
     radd.freq5     = four_bytes_2_flt(array[115],array[114],array[113],array[112]);
     radd.ipp1      = four_bytes_2_flt(array[119],array[118],array[117],array[116]);
     radd.ipp2      = four_bytes_2_flt(array[123],array[122],array[121],array[120]);
     radd.ipp3      = four_bytes_2_flt(array[127],array[126],array[125],array[124]);
     radd.ipp4      = four_bytes_2_flt(array[131],array[130],array[129],array[128]);
     radd.ipp5     = four_bytes_2_flt(array[135],array[134],array[133],array[132]);
  }

if(radd.tot_par > MXFLD){
   printf("MAX NUMBER OF SPRINT ALLOWED PARAMETERS IS %d.\n",MXFLD);
   printf("NUMBER OF PARAMETERS ACCORDING TO RADD DESCRIPTOR IS %d\n",radd.tot_par);
   exit(0);
}

  blknum++;
  if (iprint) { /* print out info */  
    printf("\nRadar Descriptor Block (dorade block # %d):\n", blknum);
    strcat(printstr,"   radnam=%8s   radcon=%8.3f  nompkpw=%8.3f  nomnspw=%8.3f");
    strcat(printstr,"  rcvgain=%8.3f  antgain=%8.3f  radgain=%8.3f\n ");
    strcat(printstr,"horbmwth=%8.3f verbmwth=%8.3f rad_type=%8d scanmode=%8d");
    strcat(printstr," scanrate=%8.3f startang=%8.3f  stopang=%8.3f\n  tot_par=%8d");
    strcat(printstr,"  tot_des=%8d datacomp=%8d data_red=%8d datared1=%8.3f");
    strcat(printstr," datared2=%8.3f rad_long=%8.3f\n  rad_lat=%8.3f");
    strcat(printstr,"  rad_alt=%8.3f      nyq=%8.3f   maxrng=%8.3f num_freq=%8d");
    strcat(printstr,"   numipp=%8d    freq1=%8.3f\n    freq2=%8.3f    freq3=%8.3f");
    strcat(printstr,"    freq4=%8.3f    freq5=%8.3f     ipp1=%8.3f     ipp2=%8.3f");
    strcat(printstr,"     ipp3=%8.3f\n     ipp4=%8.3f     ipp5=%8.3f\n");

    printf(printstr, radd.radnam, radd.radcon, radd.nompkpw, radd.nomnspw, 
           radd.rcvgain, radd.antgain, radd.radgain, radd.horbmwth, radd.verbmwth,
           radd.rad_type, radd.scan_mode, radd.scan_rate, radd.start_ang, 
           radd.stop_ang, radd.tot_par, radd.tot_des, radd.data_comp, radd.data_red,
           radd.data_red1, radd.data_red2, radd.rad_long, radd.rad_lat, 
           radd.rad_alt, radd.nyq, radd.max_range, radd.num_freq, radd.num_ip,
           radd.freq1, radd.freq2, radd.freq3, radd.freq4, radd.freq5, radd.ipp1,
           radd.ipp2, radd.ipp3, radd.ipp4, radd.ipp5);
  }

  return radd;

}
/**********************************************************************************/ 
void read_rktb(fd, array,istat)
     int fd;
     char array[MXBYTES];
     int *istat;
{
  int rval, ival, it1, it2, it3, it4, i;
  float four_bytes_2_flt();
  char printstr[1000];
  int len,slen;
  
  printstr[0]='\0';
/* figure out length from four bytes already read in */  
  len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
  rval = myread(array, (len-8), fd);
    if (rval <= 0) {
      rval = myread(array, (len-8), fd);
      if (rval <= 0) {
	*istat = 3;
      }
    }
}/*read_rktb*/
/**********************************************************************************/  
/* the following function reads in the ray info block and prints out
 * what it read in.
 */
struct ryib_blk read_ryib(fd, array, iread, ryib, iprint, istat, bytes)
     int fd;
     char array[MXBYTES];
     int iread;
     struct ryib_blk ryib;
     int iprint;
     int *istat;
     int *bytes;
{
  int rval, ival, it1, it2, it3, it4, i;
  float four_bytes_2_flt();
  char printstr[1000];
  int len,swap;
  

  printstr[0]='\0';
  swap = 0;
/* figure out length from four bytes already read in */  
  len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
  if (len > 1000000 || len < 0) {
     len = four_bytes_2_int(array[7],array[6],array[5],array[4]);
     if (len > 1000000 || len < 0) {
         printf("error: invalid block length in read_ryib. len=%d\n",len);
         exit(-1);
      }
      swap = 1;
  }
  *bytes = len;  
  
  if (iread != 1) { /* read in rest of the desc. */
    rval = myread(array, (len-8), fd);
    if (rval <= 0) {
      rval = myread(array, (len-8), fd);
      if (rval <= 0) {
	*istat = 3;
	return ryib;
      }
    }
  }


/* unpack array into its discrete parts (variables) */

  if(swap == 0){
     ryib.swpnum = four_bytes_2_int(array[0],array[1],array[2],array[3]);
     ryib.day    = four_bytes_2_int(array[4],array[5],array[6],array[7]);
     ryib.hour   = two_bytes_2_int(array[8],array[9]);
     ryib.min    = two_bytes_2_int(array[10],array[11]);
     ryib.sec    = two_bytes_2_int(array[12],array[13]);
     ryib.msec   = two_bytes_2_int(array[14],array[15]);
     ryib.az     = four_bytes_2_flt(array[16],array[17],array[18],array[19]);
     ryib.elev   = four_bytes_2_flt(array[20],array[21],array[22],array[23]);
     ryib.pkpwr  = four_bytes_2_flt(array[24],array[25],array[26],array[27]);
     ryib.rate   = four_bytes_2_flt(array[28],array[29],array[30],array[31]);
     ryib.status = four_bytes_2_int(array[32],array[33],array[34],array[35]);
  }
  else{
     ryib.swpnum = four_bytes_2_int(array[3],array[2],array[1],array[0]);
     ryib.day    = four_bytes_2_int(array[7],array[6],array[5],array[4]);
     ryib.hour   = two_bytes_2_int(array[9],array[8]);
     ryib.min    = two_bytes_2_int(array[11],array[10]);
     ryib.sec    = two_bytes_2_int(array[13],array[12]);
     ryib.msec   = two_bytes_2_int(array[15],array[14]);
     ryib.az     = four_bytes_2_flt(array[19],array[18],array[17],array[16]);
     ryib.elev   = four_bytes_2_flt(array[23],array[22],array[21],array[20]);
     ryib.pkpwr  = four_bytes_2_flt(array[27],array[26],array[25],array[24]);
     ryib.rate   = four_bytes_2_flt(array[31],array[30],array[29],array[28]);
     ryib.status = four_bytes_2_int(array[35],array[34],array[33],array[32]);
  }

  blknum++;
  if (iprint) { /* print out the info */
    printf("\nRay Info Block (dorade block # %d):\n",blknum);

    strcat(printstr,"   swpnum=%8d      day=%8d     hour=%8d      min=%8d");
    strcat(printstr,"      sec=%8d     msec=%8d       az=%8.3f\n     elev=%8.3f");
    strcat(printstr,"    pkpwr=%8.3f     true scan rate=%8.3f   ray status=%8d\n");

    printf(printstr ,ryib.swpnum, ryib.day, ryib.hour, ryib.min, ryib.sec,
           ryib.msec, ryib.az, ryib.elev, ryib.pkpwr, ryib.rate, ryib.status);
  }
  return ryib;
}

/**********************************************************************************/
/* The following function reads in the sswbinfo block and prints
 * it out. The sswb is located only in sweep files.
 */
void read_sswb(fd, array, iprint, istat)
     int fd;
     char array[MXBYTES];
     int iprint;
     int *istat;
{
  int   rval;
  int   len,slen,swap;
  float four_bytes_2_flt();
  int   num_parameters;


/* unpack length into a two's complement integer */
  swap = 0;
  len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
  if(len < 0 || len > 1000){
     len = four_bytes_2_int(array[7],array[6],array[5],array[4]);
     swap = 1;
  }
   rval = myread(array, (len-8), fd);
   if (rval <= 0) {
      *istat = 3;
   }

  *istat = 2;
  blknum++;
}/*sswb*/
     
/**********************************************************************************/
/* The following function reads in the sweep info block and prints
 * it out.
 */
struct swib_blk read_swib(fd, array, swib, iprint, istat)
     int fd;
     char array[MXBYTES];
     struct swib_blk swib;
     int iprint;
     int *istat;
{

  int rval, ival, it1, it2, it3, it4, i;
  float four_bytes_2_flt();
  char printstr[1000];
  int len,swap;

  if (*istat < 1) *istat = 1;  /* set stat flag to indicate new sweep */

  swap = 0;
  printstr[0] = '\0';
/* unpack length into a two's complement integer */
  len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
  if (len > 1000000 || len < 0) {
     len = four_bytes_2_int(array[7],array[6],array[5],array[4]);
     if (len > 1000000 || len < 0) {
        printf("error: invalid block length in read_swib. len=%d\n",len);
        exit(-1);
     }
     swap = 1;
  }


  rval = myread(array, (len-8), fd);
  if (rval <= 0) {
    rval = myread(array, (len-8), fd);
    if (rval <= 0) {
      *istat = 3;
      return swib;
    }
  }

/* unpack array into its discrete parts (variables) */

  swib.radname[8]='\0';
  for (i = 0; i < 8; i++) {
    swib.radname[i] = array[i];
  }

  if(swap == 0){ 
     swib.swpnum    = four_bytes_2_int(array[8],array[9],array[10],array[11]);
     swib.nray      = four_bytes_2_int(array[12],array[13],array[14],array[15]);
     swib.start_ang = four_bytes_2_flt(array[16],array[17],array[18],array[19]);
     swib.stop_ang  = four_bytes_2_flt(array[20],array[21],array[22],array[23]);
     swib.fix_ang   = four_bytes_2_flt(array[24],array[25],array[26],array[27]);
     swib.filt      = four_bytes_2_int(array[28],array[29],array[30],array[31]);
  }
  else{
     swib.swpnum    = four_bytes_2_int(array[11],array[10],array[9],array[8]);
     swib.nray      = four_bytes_2_int(array[15],array[14],array[13],array[12]);
     swib.start_ang = four_bytes_2_flt(array[19],array[18],array[17],array[16]);
     swib.stop_ang  = four_bytes_2_flt(array[23],array[22],array[21],array[20]);
     swib.fix_ang   = four_bytes_2_flt(array[27],array[26],array[25],array[24]);
     swib.filt      = four_bytes_2_int(array[31],array[30],array[29],array[28]);
  } 


  blknum++;
  if (iprint) { /* print out info */
    printf("\nSweep Info Block (dorade block # %d):\n",blknum);
    
    strcat(printstr,"radname=%s swpnum=%d nray=%d start_ang=%f stop_ang=%f");
    strcat(printstr," fix_ang=%f, filt=%d\n");

    printf(printstr, swib.radname, swib.swpnum, swib.nray, swib.start_ang, swib.stop_ang,
           swib.fix_ang, swib.filt);
  }
  return swib;
}

/************************************************************************************/
/* The following function reads in the rest of a volume descriptor
 * and unpacks it into a structure.
 */
struct vold_blk read_vold(fd, array, vold, iprint, istat,swap)
     int fd;
     char array[MXBYTES];
     struct vold_blk vold;
     int iprint;
     int *istat;
     int *swap;
{

  int rval, ival, it1, it2, it3, it4, i;
  static int volcnt = 0;
  char printstr[1000];
  int len,size,slen;
  unsigned short vold_value;
  extern unsigned short cswap16();

  printstr[0]='\0';
  volcnt++;

  /* set stat. flag to indicate new volume ; only every other volume header should
   * turn on flag since each volume has a header at the begin. and an indentical
   * one at the end.
   */
  if ((volcnt % 2) == 1) *istat = 2; 
  else *istat = -2;

  *swap = 0;
/* convert four bytes of length to a 2's complement integer */
  len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
  if (len > 1000000 || len < 0) {
    len = four_bytes_2_int(array[7],array[6],array[5],array[4]);
    if (len > 1000000 || len < 0) {
        printf("error: invalid block length in read_vold. len=%d\n",len);
        exit(-1);
    }
    *swap = 1;
  }

  rval = myread(array, (len-8), fd);
/*  rval = fread(array, 1, (len-8), fp);  */ /* read in rest of vol. desc. */
  if (rval <= 0) {
    rval = myread(array, (len-8), fd);
    if (rval <= 0) {
      *istat = 3;
      return vold;
    }
  }

/* now unpack volume descriptor into discrete parts */

  vold.proj_name[20] = '\0';
  for (i = 0 ; i < 20 ; i++) {
    vold.proj_name[i] = array[8+i];
  }

  vold.flt_number[8] = '\0';
  for (i = 0; i < 8; i++) {
    vold.flt_number[i] = array[40+i];
  }

  vold.gen_fac[8] = '\0';
  for (i = 0; i < 8; i++) {
    vold.gen_fac[i] = array[48+i];
  }

  if(*swap == 0){
    vold.ver      = two_bytes_2_int(array[0],array[1]);
    vold.volnum   = two_bytes_2_int(array[2],array[3]);
    vold.maxbyt   = four_bytes_2_int(array[4],array[5],array[6],array[7]);
    vold.year     = two_bytes_2_int(array[28],array[29]);
    vold.month    = two_bytes_2_int(array[30],array[31]);
    vold.day      = two_bytes_2_int(array[32],array[33]);
    vold.hour     = two_bytes_2_int(array[34],array[35]);
    vold.min      = two_bytes_2_int(array[36],array[37]);
    vold.sec      = two_bytes_2_int(array[38],array[39]);
    vold.gen_yr   = two_bytes_2_int(array[56],array[57]);
    vold.gen_mon  = two_bytes_2_int(array[58],array[59]);
    vold.gen_day  = two_bytes_2_int(array[60],array[61]);
    vold.num_sens = two_bytes_2_int(array[62],array[63]);
  }
  else{
    vold.year     = two_bytes_2_int(array[29],array[28]);
    vold.month    = two_bytes_2_int(array[31],array[30]);
    vold.day      = two_bytes_2_int(array[33],array[32]);
    vold.hour     = two_bytes_2_int(array[35],array[34]);
    vold.min      = two_bytes_2_int(array[37],array[36]);
    vold.sec      = two_bytes_2_int(array[39],array[38]);
    vold.gen_yr   = two_bytes_2_int(array[57],array[56]);
    vold.gen_mon  = two_bytes_2_int(array[59],array[58]);
    vold.gen_day  = two_bytes_2_int(array[61],array[60]);
    vold.num_sens = two_bytes_2_int(array[63],array[62]);
  }    

  blknum++;
  if (iprint) { /* all the unpacking is done; now print out, if desired */
    printf("\n\n                              +++  VOLUME HEADER  +++\n");
    printf("\nVolume descriptor block(dorade block # %d):\n",blknum);
    strcat(printstr,"  version=%8d   volnum=%8d   maxbyt=%8d projname=%8s");
    strcat(printstr,"     year=%8d    month=%8d      day=%8d\n     hour=%8d");
    strcat(printstr,"      min=%8d      sec=%8d   fltnum=%8s  gen_fac=%8s");
    strcat(printstr,"   gen_yr=%8d  gen_mon=%8d\n  gen_day=%8d num_sens=%8d\n");

    printf(printstr, vold.ver, vold.volnum, vold.maxbyt, vold.proj_name, vold.year,
           vold.month, vold.day, vold.hour, vold.min, vold.sec, vold.flt_number,
           vold.gen_fac, vold.gen_yr, vold.gen_mon, vold.gen_day, vold.num_sens); 
  }

  return vold;
}

/******************************WAVEFORM********************************/
void read_wave(fd, array, iread, iprint, istat)
     int fd;
     char array[MXBYTES];
     int iread;
     int iprint;
     int *istat;
{

  int rval, ival, it1, it2, it3, it4, i;
  float four_bytes_2_flt();
  char printstr[1000];
  float fvar;
  short svar;
  int len,ii,aindex,count;
  
  printstr[0]='\0';
  blknum++;
  printf("\nWaveform Block (dorade block # %d):\n",blknum);
/* convert four bytes of length to a 2's complement integer */
  len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
  if (len > 1000000 || len < 0) {
    printf("error: invalid block length in read_wave. len=%d\n",len);
    exit(-1);
  }
  
  rval = myread(array, (len-8), fd);
  if (rval <= 0) {
    rval = myread(array, (len-8), fd);
    if (rval <= 0) {
      *istat = 3;
      return;
    }
  }

  printf("ps_file_name ");
  for(i = 0; i < 16; i++)
      printf("%c",array[i]);
  printf("\n");
 
  aindex = 16;
  count  = 1; 
  for(ii = 0; ii < 6; ii++){  
     svar = two_bytes_2_int(array[aindex],array[aindex+1]);
     printf("num_chips in a repeat sequence for frequency %d:  %d\n",
             count,svar);
     aindex += 2;
     count++;
  }

  aindex = 28;
  printf("The Blanding RAM sequence\n");
  for(ii = 0; ii < 256; ii++){
     printf("%c",array[aindex]);
     aindex++;
  }
  printf("\n");

  fvar  = four_bytes_2_flt(array[284],array[285],array[286],array[287]);
  printf("Repeat sequence: %f\n",fvar);
  svar = two_bytes_2_int(array[288],array[289]);
  printf("Repeat sequence dwel: %d\n",svar);
  svar = two_bytes_2_int(array[290],array[291]);
  printf("Total PCP: %d\n",svar);

  aindex = 292;
  printf("Chip Offsets 1 - 6:  ");
  for(ii = 0; ii < 6; ii++){  
     svar = two_bytes_2_int(array[aindex],array[aindex+1]);
     printf(" %d",svar);
     aindex += 2;
  }


  printf("\n");
  aindex = 304;
  printf("Chip Width   1 - 6:  ");
  for(ii = 0; ii < 6; ii++){  
     svar = two_bytes_2_int(array[aindex],array[aindex+1]);
     printf(" %d",svar);
     aindex += 2;
  }
  printf("\n");

  fvar  = four_bytes_2_flt(array[316],array[317],array[318],array[319]);
  printf("Unabmiguous range PCP   : %f\n",fvar);
  fvar  = four_bytes_2_flt(array[320],array[321],array[322],array[323]);
  printf("Unabmiguous velocity PCP: %f\n",fvar);

  aindex = 324;
  printf("Number of gates sampled : ");
  for(ii = 0; ii < 6; ii++){  
     svar = two_bytes_2_int(array[aindex],array[aindex+1]);
     printf(" %d",svar);
     aindex += 2;
  }

  printf("\n");
  aindex = 336;
  count  = 1;
  for(ii = 0; ii < 6; ii++){  
     svar = two_bytes_2_int(array[aindex],array[aindex+1]);
     printf("gate distance%d[2] %d\n",count,svar);
     aindex += 2;
     count++;
  }  
}/*waveform*/

/*********************************NDDS*************************************/
void read_navigation(fd, array, iread, iprint, istat)
     int fd;
     char array[MXBYTES];
     int iread;
     int iprint;
     int *istat;
{

  int rval, ival, it1, it2, it3, it4, i;
  float four_bytes_2_flt();
  char printstr[1000];
  short svar;
  int len;
  
  printstr[0]='\0';
  blknum++;
  printf("\nNavigation  Block (dorade block # %d):\n",blknum);
/* convert four bytes of length to a 2's complement integer */
  len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
  if (len > 1000000 || len < 0) {
    printf("error: invalid block length in read_wave. len=%d\n",len);
    exit(-1);
  }
  
  rval = myread(array, (len-8), fd);
  if (rval <= 0) {
    rval = myread(array, (len-8), fd);
    if (rval <= 0) {
      *istat = 3;
      return;
    }
  }

  svar = two_bytes_2_int(array[0],array[1]);
  printf("Ins flag : %d",svar);
  svar = two_bytes_2_int(array[0],array[1]);
  printf("      Gps flag  : %d",svar); 
  svar = two_bytes_2_int(array[0],array[1]);
  printf("      Minirims flag : %d",svar); 
  svar = two_bytes_2_int(array[0],array[1]);
  printf("      Kalman flag : %d\n",svar);
}

/*********************************FRAD*************************************/
  
/* The following function reads in the dorade field radar data
 * block and prints out what it read in.
 */
struct frad_blk read_frad(fd, array, frad, iprint, iunpck, flddat, istat, 
                          swap)
     int fd;
     char array[MXBYTES];
     struct frad_blk frad;
     int iprint, iunpck;
     float flddat[MXFLD][MXGAT];
     int *istat;
     int swap;
{
  int rval, len, i, j, first_gate, last_gate, isize;
  int it1, it2, rnum, slen;
  int num_bytes[MXFLD];
  int ngates, numflds, ray_count;
  char radname[9];
  float four_bytes_2_flt();




/* get length of block as a two's complement integer from four bytes 
 * already read in.
 */
  len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
  if(swap == 1){
     slen = cswap32((long)len);
     len = slen;
  }    
  if (len > MXBYTES || len < 0) {
    printf("error: invalid block length in read_frad. len=%d\n",len);
    exit(-1);
  }

  rval = myread(array, (len-8), fd);
/*  rval = fread(array, 1, (len-8), fp);  */ /* read in rest of the desc. */
  if (rval <= 0) {
    rval = myread(array, (len-8), fd);
    if (rval <= 0) {
      *istat = 3;
      return frad;
    }
  }

/* unpack some of the housekeeping words. we don't need them all. 
 * data will be unpacked later in this function.
 */

  frad.radname[8] = '\0';
  for (i = 0; i < 8; i++) {
    frad.radname[i] = ' ';
    frad.radname[i] = array[4+i];
  }

  rnum = -1;
  for (i = 0; i < MXRAD; i++) {
    if (strncmp(frad.radname, radds[i]->radnam,4) == 0) rnum = i;
  }
  if (rnum == -1) {
    printf("\n    +++Error. Could not find radar %s in radar list.\n",
	   frad.radname);
    exit(1);
  }
  ngates  = data_info[rnum]->ngates;
  numflds = data_info[rnum]->numflds;

  if(swap == 0){
    frad.ray_count  = four_bytes_2_int(array[36],array[37],array[38],
                                       array[39]);
    frad.firs_gate  = two_bytes_2_int(array[40],array[41]);
    frad.last_gate  = two_bytes_2_int(array[42],array[43]);
  }
  else{
    frad.ray_count  = four_bytes_2_int(array[39],array[38],array[37],
                                       array[36]);
    frad.firs_gate  = two_bytes_2_int(array[41],array[40]);
    frad.last_gate  = two_bytes_2_int(array[43],array[42]);
  }  
  
/* check for internal consistency between the size of the block in bytes and
 * the number of gates and fields according to the housekeeping.
 */  
  isize = 52;
  for (i = 0; i < numflds; i++) {
    switch (data_info[rnum]->fld_typ[i])
      {
      case 1:
	num_bytes[i] = 1;
	break;
      case 2:
	num_bytes[i] = 2;
	break;
      case 3:
	num_bytes[i] = 4;
	break;
      case 4:
	num_bytes[i] = 4;
	break;
      default:
	printf("\n    error: incorrect field data length. type=%d\n",data_info[rnum]->fld_typ[i]);
	exit(-1);
	break;
      }
    isize = isize + ngates*num_bytes[i];
  }

  if (isize != len) {
    printf("error: length of frad block and number of gates and fields are not consistent.\n len=%d ngates=%d, numflds=%d\n",len, data_info[rnum]->ngates, data_info[rnum]->numflds);
    exit(-1);
  }

  blknum++;
  if (iunpck) {    /* transfer the data to arrays now */
    if (iunpck == 1) { /* leave in scaled format */
      for (i = 0; i < numflds; i++) {
	for (j = 0; j < ngates; j++) {
	  it2 = array[45+(j*numflds*2)+2*i];
	  if (it2 < 0) it2 = it2 + 256;
	  flddat[i][j] = (float)it2;
	}
      }
    }
    else { /* convert to meteorological units */
      for (i = 0; i < numflds; i++) {
	for (j = 0; j < ngates; j++) {
	  it2 = array[45+(j*numflds*2)+2*i];
	  if (it2 < 0) it2 = it2 + 256;
	  if (data_info[rnum]->scale[i] != 0.0) 
	    flddat[i][j] = ((float)it2 - data_info[rnum]->offset[i])/data_info[rnum]->scale[i];
	}
      }
    }
  }

  if (iprint) {  /* print the info out */

    printf("\nField Radar Block (dorade block # %d):\n",blknum);
    printf("  radname=%8s   raycnt=%8d firstgat=%8d  lastgat=%8d",frad.radname, frad.ray_count,frad.firs_gate,frad.last_gate);
    for (i = 0; i < numflds; i++) {
      printf("\n\n    field=%8s    scale=%8.3f   offset=%8.3f      bad=%8.3f\n",data_info[rnum]->fields[i], data_info[rnum]->scale[i],data_info[rnum]->offset[i],data_info[rnum]->bad[i]);
      printf("data values = \n");
      for (j = 0 ; j < ngates; j++) {
	printf("%5.0f ",flddat[i][j]);
	if (((j+1) % 20) == 0) printf("\n");   /* new line every 20 datums */
      }
    }
    printf("\n");
  }

  return frad;

}

/*********************************************************************************/
int uncompress( ss, dd, flag, empty_run, wmax )
  short *ss, dd[MXGAT];
  int flag, *empty_run, wmax;
{
    /*Author Dick Oye. NCAR/RDP
     * routine to unpacks actual data assuming MIT/HRD compression where:
     * ss points to the first 16-bit run-length code for the compressed data
     * dd points to the destination for the unpacked data
     * flag is the missing data flag for this dataset that is inserted
     *     to fill runs of missing data.
     * empty_run pointer into which the number of missing 16-bit words
     *    can be stored. This will be 0 if the last run contained data.
     # wmax indicate the maximum number of 16-bit words the routine can
     *    unpack. This should stop runaways.
     */
    int i, j, k, n, mark, wcount=0;
    short value;

    k = 0;
    while(*ss != 1) {		/* 1 is an end of compression flag */
	n = *ss & 0x7fff;	/* nab the 16-bit word count */
	if(wcount+n > wmax) {
	    printf("Uncompress failure %d %d %d\n"
		   , wcount, n, wmax);
	    mark = 0;
	    break;
	}
	else {
	    wcount += n;		/* keep a running tally */
	}
	if( *ss & 0x8000 ) {	/* high order bit set implies data! */
	    *empty_run = 0;
	    ss++;
            k = 0;
	    for(; n--;) {
		dd[k] = *ss++;
                k++;
	    }
	}	
	else {			/* otherwise fill with flags */
	    *empty_run = n;
	    ss++;
	    for(; n--;) {
		dd[k] = flag;
                k++;
	    }
	}
    }
    return(wcount);
}
/* c------------------------------------------------------------------------ */

int LEuncompress( ss, dd, flag, empty_run, wmax )
  unsigned short *ss, *dd;
  int flag, *empty_run, wmax;
{
    /*
     * This routine is called when byte swapping needs to be done.
     * routine to unpack actual data assuming MIT/HRD compression where:
     * ss points to the first 16-bit run-length code for the compressed data
     * dd points to the destination for the unpacked data
     * flag is the missing data flag for this dataset that is inserted
     *     to fill runs of missing data.
     * empty_run pointer into which the number of missing 16-bit words
     *    can be stored. This will be 0 if the last run contained data.
     # wmax indicate the maximum number of 16-bit words the routine can
     *    unpack. This should stop runaways.
     */
    int i, j, k, n, mark, wcount=0;
    unsigned short rlcw;
    unsigned char *aa, *bb;
    void swack_short();
    

    aa = (unsigned char *)&rlcw;
    i = 0;
    for(;;) {	
       bb = (unsigned char *)ss;
       *aa = *(bb+1);
       *(aa+1) = *bb;		/* set run length code word "rlcw" */
       if(rlcw == 1) { break; }	/* 1 is the end of compression flag */

       n = rlcw & 0x7fff;	/* nab the 16-bit word count */
       if(wcount+n > wmax) {
	  printf("Uncompress failure %d %d %d\n"
		 , wcount, n, wmax);
	  mark = 0;
	  break;
       }
       else {
	  wcount += n;		/* keep a running tally */
       }
       if( rlcw & 0x8000 ) {	/* high order bit set implies data! */
	  *empty_run = 0;
	  ss++;
	  swack_short(ss, dd, n);
	  ss += n;
	  dd += n;
	  
       }	
       else {			/* otherwise fill with flags */
	  *empty_run = n;
	  ss++;
	  for(; n--;) {
	     *dd++ = flag;
	  }
       }
    }
    return(wcount);
}
/* c------------------------------------------------------------------------ */
void swack_short(ss, tt, nn)
  char *ss, *tt;
  int nn;
{
   for(; nn--;) {
      *tt++ = *(ss+1);
      *tt++ = *ss;
      ss += 2;
   }
}



