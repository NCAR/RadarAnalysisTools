#include <stdio.h>
#include "dorade.h"

extern struct metadata *data_info[MXRAD];

/* The following function reads a beams worth of data from a dorade or
 * an eldora field format dataset. Note that a beam is several dorade
 * blocks so this function will read the number of blocks necessary
 * for a whole beam. This function assumes that a beam is read in when
 * we have read in a RYIB (Ray info block), an ASIB, and an FRAD
 * consecutively in that order. This assumption may have to be different
 * when reading eldora field format vs. "true" dorade.
 *
 *Airborne Dorade:
 *   This function assumes that a beam is read in when
 *   we have read in a RYIB (Ray info block), an ASIB, and an FRAD
 *   consecutively in that order. 
 *
 *Ground based Dorade:
 *   This function assumes that a beam is read in when
 *   we have read in a RYIB (Ray info block) and an FRAD
 *   consecutively in that order.
 *
 * The radar type airborne or groundbased is read in from the RADD
 * descriptor.  As outlined in the Dorade documentation The rad_type
 * field of the RADD gives the radar type.
 * 0 - ground based  
 * 1 - airborne forward
 * 2 - airborne aft
 * 3 - airborne tail
 * 4 - airborne LF
 * 5 - shipborne
 */


void assemble_beam(fd, fpw, vold, radd, cfac, parm, cspd, celv, swib,ryib,
                   asib, frad, rdat, iprinth, iprintr, time, reqtime, 
                   flddat, istat, iunpck, cfldnam, write_to_file,swapping,
                   platformb_found,sweep_files,bytes_beam)
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
     float *time;
     float *reqtime;
     float flddat[MXFLD][MXGAT];
     int *istat, iunpck;
     char cfldnam[MXFLD][9];
     int *write_to_file;
     int *swapping;
     int *platformb_found;
     int *sweep_files;
     int *bytes_beam;
{
  int i, j, k, ij;
  int have_asib = 0;
  int beam_start = 0;
  int found_data_block = 0;
  int read_full_beam = 0;
  int bytes_read = 0;
  static int beams_in_sweep;
  static int beams_read;
  char blknam[5];
  char block[3][5];
  void read_block();
  static int radar_type = -1;


  blknam[4] = '\0';
/* start reading in blocks. Read in enough blocks to have a beam defined. */
  i = 0;
  while ( i < MXBLOCKS && ryib) {
        read_block(fd, fpw, vold, radd, cfac, parm, cspd, celv, swib, ryib, 
               asib,frad, rdat, iprinth, iprintr, blknam, time, reqtime, 
	       flddat, istat, iunpck, cfldnam, write_to_file,swapping,
               &bytes_read);
               if (*istat > 2) return;
               if (*istat < 0) return;
/* we have a ray when we have read in a RYIB, an ASIB, and several RDAT 
 * descriptors in that order. Thus, we keep searching for such consecutive 
 * blocks.
 */
    if(strncmp(blknam,"SSWB",4) == 0){
       *sweep_files = 1;
    } 

    if(strncmp(blknam,"SWIB",4) == 0){
       beams_in_sweep = swib->nray;
       beams_read = 0;
    }   

    if(strncmp(blknam,"RADD",4) == 0){
       radar_type = radd->rad_type;
    }  
  
    if(strncmp(blknam,"ASIB",4) == 0){
     /*
      *The platform information block may or may not be present. 
      *It is mandatory for shipborne and airborne radars. 
      */
       *platformb_found = 1;
    }

    if(strncmp(blknam,"RYIB",4) == 0){
      /*
       *We have read a RYIB this "always" starts a beam of data.
       *We now need a RDAT in order start reading the data.
       */
       beams_read++;
       beam_start = 1;
    }

    if(strncmp(blknam,"RDAT",4) == 0){
      /*
       *We have found a RDAT so we are now reading a data block.
       */
       found_data_block = 1;
    }

    if(found_data_block == 1 && beam_start == 1){
      /*
       *We can now start reading the data. We have a RYIB and an RDAT.
       */
       read_full_beam = 1;
     }

    if(read_full_beam == 1){
       for ( j = 1; j < data_info[0]->numflds; j++) {
	   read_block(fd, fpw, vold, radd, cfac, parm, cspd, celv, swib, 
		   ryib, asib, frad, rdat, iprinth, iprintr, blknam, time,
		   reqtime,flddat, istat, iunpck, cfldnam, write_to_file,
                   swapping,&bytes_read);
	   if (*istat > 2) return;
	}/*the end for the for j loop*/
       /*
        *We now have a full beams worth of data since we have read in the 
        *fields so we can return to rdbeam2.
        */
        *bytes_beam = bytes_read;
        return;
    }/*the end for if read full beam*/

    i++;
  }/*while*/

  printf("\n +++ Error: could not find a sequence of dorade blocks that define");
  printf(" a beam.\n");
  *istat = 4;
  return;
}

    
  
