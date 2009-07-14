#include <stdio.h>
#include <string.h>
#include  <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/mtio.h>
#include <sys/ioctl.h>
#include "nexrad.h"
#include "nxshort.h"
#include "cedric.h"
int  nex_skipping = 0;       /* 1 -> in process of skipping; used when writing
			 * out new volumes.
			 */

static  struct nexrad_site_info *nsi;
static  struct nexrad_volume_scan_info *nvsi;
static  struct nexrad_data_info *ndi;
static  struct nexrad_message_header *nmh;
static  struct digital_radar_data_header *drdh;
static  struct rda_status_info *rda_si;
static  struct nexrad_VCP_header *first_nvh=NULL, *current_nvh;
static  struct nexrad_VCP_items *nvi;
typedef short int Int2b;
static  int nex_unamb_rngs[5][8];	/* 8 ranges, 5 deltas */
static  int current_VCP = 0;
static  int number_of_rays = 0;
static  int tape_device = NO;
static  char field[2];
int     current_delta;
static  int     Blocking;
static  int     TapeHeaderPresent = NO;
static  int     volume_number = -1;
static  int position,bytesUsed = 0;
static  int  blocks;
static  FILE *savefd;
static  FILE *fp; /*File pointer for disk file*/
static  int  fpd; /*File pointer for tape device*/

/*
 *These arrays are for the volume coverage pattern number.  They are 
 *filled in the routine get_VCP_info.
*/
static int vcp11[68];
static int vcp12[68];
static int vcp21[48];
static int vcp31[36];
static int vcp32[32];
static int vcp300[20];
static int vcp121[70];


#if defined (IBMRISC) || defined (HP)
void nexrad_rdbeam(int *nufst,int *endtime, int *inunit, int *irewnd, int *istat,
                    int *ivol, int *iswp, int *iyr, int *mon, int *day,
	            int *hr, int *min, int *sec, int *msec, int *julday,
                    float *az, float *el, float *rotang, float *fixed_angle, 
                    float *nyquist, float *unamb_range,
                    float flddat[MXFLD][MXGAT],char fldnam[MXFLD][8],
                    char requested[MXFLD][8],int *num_req_fields,int *nfields, 
                    int *nrng,float *rng_first_gate, float *gatspac, int *num_rays,
                    int *swapping,char tape_unit[8])
#elif defined (CRAY)
void NEXRAD_RDBEAM((int *nufst,int *endtime, int *inunit, int *irewnd, int *istat,
                    int *ivol, int *iswp, int *iyr, int *mon, int *day,
	            int *hr, int *min, int *sec, int *msec, int *julday,
                    float *az, float *el, float *rotang, float *fixed_angle, 
                    float *nyquist, float *unamb_range,
                    float flddat[MXFLD][MXGAT],char fldnam[MXFLD][8],
                    char requested[MXFLD][8],int *num_req_fields,int *nfields, 
                    int *nrng,float *rng_first_gate, float *gatspac, int *num_rays,
                    int *swapping,char tape_unit[8])
#elif defined (linux)
void nexrad_rdbeam__(int *nufst,int *endtime, int *inunit, int *irewnd, int *istat,
                    int *ivol, int *iswp, int *iyr, int *mon, int *day,
	            int *hr, int *min, int *sec, int *msec, int *julday,
                    float *az, float *el, float *rotang, float *fixed_angle, 
                    float *nyquist, float *unamb_range,
                    float flddat[MXFLD][MXGAT],char fldnam[MXFLD][8],
                    char requested[MXFLD][8],int *num_req_fields,int *nfields, 
                    int *nrng,float *rng_first_gate, float *gatspac, int *num_rays,
                    int *swapping,char tape_unit[8])
#else
void nexrad_rdbeam_(int *nufst,int *endtime, int *inunit, int *irewnd, int *istat,
                    int *ivol, int *iswp, int *iyr, int *mon, int *day,
	            int *hr, int *min, int *sec, int *msec, int *julday,
                    float *az, float *el, float *rotang, float *fixed_angle, 
                    float *nyquist, float *unamb_range,
                    float flddat[MXFLD][MXGAT],char fldnam[MXFLD][8],
                    char requested[MXFLD][8],int *num_req_fields,int *nfields, 
                    int *nrng,float *rng_first_gate, float *gatspac, int *num_rays,
                    int *swapping,char tape_unit[8])
#endif

/* The following C function initiates the reading of a beam from a
 * Nexrad format data file. 
 *
 *
 * 
 * Output of istat variable:
 *     ISTAT=0  --> START OF NEW SCAN OR SWEEP
 *     ISTAT=1  --> INTERMEDIATE BEAM
 *     ISTAT=2  --> END OF AN ELEVATION SCAN
 *     ISTAT=3  --> BEGINNING OF A NEW VOLUME SCAN
 *     ISTAT=4  --> END OF A VOLUME SCAN
 *     ISTAT=5  --> STATUS DATA CONTAINS NO BEAM DATA
 *     ISTAT=6  --> END OF DATA
 *     ISTAT=7  --> READ ERROR
 *
 * The order of fields in the flddat array is always:
 * flddat[first][MXGAT]  = DZ
 * flddat[second][MXGAT] = VE
 * flddat[third][MXGAT]  = SW
 *
 */

/******************************************************************************/


     
{
  int i1,i2,i3,i4,ii,jj;
  int read_status;
  int number_of_fields;
  static int fd;
  static int end_time_reached = 0;
  int rval, rnum;
  int iprinth = 0, iprintr = 0;
  int beam_time;
  int index,swap;
  int gate_sp,numgates;
  int range_to_first_gate;
  int number_req_fields,datasize;
  int current_time;
  int subtract;
  int length,test;
  char filename[9], radname[9];
  char cfldnam[MXFLD][9];
  char tape_drive_name[12],*cmd1;
  float bad = -327.68;
  extern long cswap32();
  struct mtop op;


  /* linked list of open file info*/
  static struct files *open_files = NULL;
  static struct files *head = NULL;  /* head of linked list */
  static int lastunit = -1;
  swap = NO;

  if(*swapping == YES) swap = YES;
  /*--------------------------------------------------------------------------*/
  /* 
   *Initialize the nexrad structures.  These structures are located in nexrad.h
   */
  if(volume_number == -1){
     init_nexrad_structures();
  }

  /*--------------------------------------------------------------------------*/
  /*
   *Open the file.
   */
  if(*inunit != 0){
    if (lastunit != *inunit) { /* get file pointer for requested unit number */
      lastunit = *inunit;
    
      if (*inunit < 10 || *inunit > 999) {
         printf("\n +++ Invalid unit number %d +++\n",*inunit);
         exit(1);
      }
      i1 = *inunit / 10 ;
      i2 = *inunit % 10 ;
      i3 = i1 / 10 ;
      i4 = i1 % 10 ;
    
    /* construct filename from input fortran unit number*/    
      filename[0] = 'f';
      filename[1] = 'o';
      filename[2] = 'r';
      filename[3] = 't';
      filename[4] = '.';
      if (*inunit <= 99) {
        filename[5] = i1 + 48;  /* convert to ascii */
        filename[6] = i2 + 48;  /* convert to ascii */
        filename[7] = '\0';
      }
      else {
        filename[5] = i3 + 48;  /* convert to ascii */
        filename[6] = i4 + 48;  /* convert to ascii */
        filename[7] = i2 + 48;  /* convert to ascii */
        filename[8] = '\0';
      }
    printf("inunit=%d  i1=%d  i2=%d  i3=%d  i4=%d  filename=%s \n",*inunit,i1,i2,i3,i4,filename);
    }
    
    if (head == NULL) {  /* start linked list of open files */
      open_files = (struct files *)malloc(sizeof(struct files));
      if (open_files == NULL) { /* error getting address */
	printf("\n+++ OUT OF MEMORY IN RDBEAM2+++\n");
	exit(-1);
      }
      head = open_files;
      open_files->next = NULL;
      open_files->unit = -99;
    } 

    open_files = head;
    while (open_files->next != NULL  &&  open_files->unit != *inunit) {
      open_files = open_files->next;
    }
    if (open_files->unit != *inunit) { /* file is not already in list;open it*/
      
      fp = fopen(filename,"r");
      if(!fp){
         printf("file: %s was not found\n",filename);
         exit(0);
      }
      if (open_files->unit > 0) {
	/* add new open file to list of open files */
	open_files->next = (struct files *) malloc(sizeof(struct files));
	if (open_files->next == NULL) {
	  printf("\n+++OUT OF MEMORY IN NEXRAD.c+++\n");
	  exit(-1);
	}
	open_files = open_files->next;
      }
      open_files->unit = *inunit;
      open_files->fps  = fp;
      open_files->next = NULL;
    }
    else {
      fp = open_files->fps;
    }

   }/*if(lastunit != inunit*/
  else{
    if(tape_device == NO){ /*Open the tape device unit*/
      strcpy(tape_drive_name,"/dev");
      strcat(tape_drive_name,tape_unit);
      length = strlen(tape_drive_name);
      tape_drive_name[length+1] = '\0';
      fpd = open(tape_drive_name,O_RDONLY);
      tape_device = YES;
      if(fpd <= 0){
         printf("UNABLE TO OPEN TAPE DEVICE UNIT: %s\n",tape_drive_name);
         exit(0);
      }
      else{
         printf("TAPE DRIVE UNIT %s IS OPEN UNIT :%d\n",tape_drive_name,fpd);
      }
    }
  }
  /*--------------------------------------------------------------------------*/

  /*
   *Read the file we will know what type of record we have by the read status.
   */

  /*
   *Initialize the data array.
   */
   for(ii = 0; ii < MXFLD; ii++){
     for(jj = 0; jj < MXGAT; jj++){
         flddat[ii][jj] = bad;
     }
   } 
  nvsi->processing = 1;
  number_req_fields = *num_req_fields; /*The number of fields requested by Sprint*/
  read_status = read_nexrad_ray(flddat,cfldnam,requested,number_req_fields,
                                &number_of_fields,&gate_sp,&numgates,
                                &range_to_first_gate,swap);
  switch(read_status){
     case FILE_READ_ERROR:{
          *istat = 7;
          break;
     }
     case END_OF_FILE:{
          *istat = 6;
          *nufst = 2;
          fseek(fp,0,0);
          break;
     }
     case END_OF_TAPE:{
          *istat = 6;
          *nufst = 2;
          break;
     }          
     case TAPE_HEADER:{
          *istat = 0;
          *iyr   = 0;
          *mon   = 0;
          *day   = 0;
          *hr    = 0;
          *min   = 0;
          *sec   = 0;
          *msec  = 0;
          ndi->bytesBuffer = 0;
          break;
     }
     case FILE_HEADER:{ /*A file header has volume time information in it.*/
          *istat = 5;
          subtract = nvsi->year/100;
          subtract = subtract * 100;
          *iyr     = nvsi->year - subtract;
          *mon     = nvsi->month;
          *day     = nvsi->day;
          *hr      = nvsi->hour;
          *min     = nvsi->minute;
          *sec     = nvsi->second;
          *msec    = 0;
          *nrng    = 0;
          *nfields = 0;
          *gatspac = 0.0;
          *rng_first_gate = 0.0;
          *nfields = 0;
          *el    = 0.0;
          *az    = 0.0;
          *fixed_angle = 0.0;
          *nyquist     = 0.0;
          *unamb_range = 0.0;
          break;
     }
  case DATA_RECORD:{ /*A message header has ray time information in it*/
	 /*
          *If we did not read a digital radar data block or the requested 
          *field or flields were not found in this sweep then just return.
          */
          if((drdh->ref_num_gates == 0) && (drdh->vel_num_gates == 0)){
             *istat = 5;
             *nufst = 0;
             current_time = 10000 * nvsi->hour + 100 * nvsi->minute + nvsi->second;
             if(current_time >= *endtime){
                end_time_reached = 1;
	     }
             *iyr     = 0;
             *mon     = 0;
             *day     = 0;
             *hr      = nvsi->hour;
             *min     = nvsi->minute;
             *sec     = nvsi->second;
             *msec    = nvsi->millisecond;
             return;
          }
          subtract = nvsi->year/100;
          subtract = subtract * 100;
          *iyr     = nvsi->year - subtract;
          *mon     = nvsi->month;
          *day     = nvsi->day;
          *hr      = nvsi->hour;
          *min     = nvsi->minute;
          *sec     = nvsi->second;
          *msec    = nvsi->millisecond;
          *nrng    = numgates;
          *nfields = number_of_fields;
          *gatspac = (float)gate_sp;
          *rng_first_gate = (float)range_to_first_gate;
          *nfields = nvsi->num_flds_in_sweep;
          *el    = (float)drdh->elevation * (180./(8.*4096.));
          *az    = (float)drdh->azimuth * (180./(8.*4096.));
          *fixed_angle = nvsi->fixed_angle;
          *nyquist     = (float)drdh->nyquist_vel_x100/100.;
          *unamb_range = drdh->unamb_range_x10/10.;
          strncpy(fldnam[0],cfldnam[0],3);
          strncpy(fldnam[1],cfldnam[1],3);
          strncpy(fldnam[2],cfldnam[2],3);
          current_time = 10000 * nvsi->hour + 100 * nvsi->minute + nvsi->second;
          if(current_time >= *endtime){
             end_time_reached = 1;
	  }
	  if(drdh->radial_status == 0){ /*First ray of new elevation sweep*/
	     *istat = 0; 
             *num_rays = 1;
             number_of_rays = 1;
	  }
	  else if(drdh->radial_status == 1){/* An intermediate ray*/
	     *istat  = 1;  /*intermediate sweep*/
             *num_rays = number_of_rays;
          }
          else if(drdh->radial_status == 2){ /*End of elevation sweep.*/
             *istat = 2;
             *num_rays = number_of_rays;
	  }
	  else if(drdh->radial_status == 3){ /*Beginning of new Volume*/
             *istat = 3;
             *num_rays = 1; 
             number_of_rays = 1;
	  }  
	  else if(drdh->radial_status == 4){ /*End of volume*/
                 *istat = 4;
                 ndi->bytesBuffer = 0;
                 ndi->buffer_position = 0;
                 ndi->bufferPtr =0;
                 *num_rays = number_of_rays; 
                 
                 if(end_time_reached == 1){
                    *nufst = 3;
                    if(tape_device == NO){
                        fseek(fp,0,0);
		    }
                    else{
                      printf("REPOSITIONING THE TAPE DEVICE\n");
	              op.mt_op = MTREW;
	              op.mt_count = 1;	/* Only rewind once */
                      if (ioctl(fpd, MTIOCTOP, &op) < 0){
                         printf("UNABLE TO REPOSITION TAPE DRIVE\n");
                         *nufst = 2;
		      }
		    }
		    /* printf("END TIME REACHED %d %d %d\n",*hr,*min,*sec);*/
                    end_time_reached = 0;
                    return;
                 }/*end of if for end time reached*/
                 else
		 {
		   if(tape_device == YES){/*Skip over the end of volume marker*/
	              op.mt_op = MTFSF;
	              op.mt_count = 1;	/* SKIP ONE FILE MARKER*/
                      if (ioctl(fpd, MTIOCTOP, &op) < 0){
                         printf("UNABLE TO REPOSITION TAPE DRIVE\n");
                         *nufst = 2;
                         return;
		      }
		   }
		 }
	  }/*end of the if for status = 4*/
     *nufst = 0;
     }/*case DATA RECORD*/
  }/*switch*/
  return;
  
}/*end of nexrad_vol*/


/**********************************************************************/  
int read_nexrad_ray(float flddat[MXFLD][MXGAT],char cfldnam[MXFLD][9],
                    char requested [MXFLD][8],int number_req_fields,
                    int *number_of_fields,int *gate_sp,int *numgates,
                    int *range_to_first_gate,int swap)
{
char   fields_available[MXFLD][8];
int    blockType;
extern int position,bytesUsed;
extern int  blocks;
int    CTM_size,rda_size,nmh_size;
int    index,field_index,i,the_first_gate,range;
int    gate_index;
int    number_of_gates,start_of_data;
int    junk,found;
double *databuff;
double  offset;
unsigned char  unsignedByte;
extern int Blocking,number_of_rays;
extern struct nexrad_data_info *ndi;
extern struct rda_status_info *rda_si;
extern struct digital_radar_data_header *drdh;

     CTM_size = sizeof(struct CTM_info);
     nmh_size = sizeof(struct nexrad_message_header);
     rda_size = sizeof(struct rda_status_info);
 
     if(ndi->bytesBuffer <= 0){  
        ndi->bufferPtr = read_nexrad_record(ndi->fileBuffer,&ndi->bytesBuffer,swap);
        blocks = 1; 
        position = 0;
        ndi->buffer_position = 0;
     }
     else{
        position = ndi->buffer_position;
     }

     /*
      *Determine what type of record we have by its length.
      */
      if(ndi->bufferPtr == BYTES_FILE_HEADER){
           get_file_header(ndi->fileBuffer,swap);     
           ndi->bytesBuffer = 0;
           return(FILE_HEADER);
      }
      
      else if(ndi->bufferPtr == BYTES_TAPE_HEADER){
         get_tape_header(ndi->fileBuffer); 
         ndi->bytesBuffer = 0;
         return(TAPE_HEADER);
      }
      else if(ndi->bufferPtr == BYTES_DATA_BLOCK){
           if(Blocking == YES){
              position = position + CTM_size + 4;
	   }
           else{
              position = position + CTM_size;
	   }
           get_message_header(ndi->fileBuffer,&blockType,position);
           position  = position + nmh_size;
           if(blockType == DIGITAL_RADAR_DATA){
               get_digital_radar_data_header(ndi->fileBuffer,position,
                                                fields_available,swap);
               *number_of_fields = nvsi->num_flds_in_sweep;
               found = NO;
               for(field_index = 0; field_index < nvsi->num_flds_in_sweep; field_index++){
                   if((requested[field_index][0] == fields_available[0][0]) ||
                      (requested[field_index][0] == fields_available[1][0]) ||
                      (requested[field_index][0] == fields_available[2][0]))
                                 found = YES;
               }  
               if(found == NO){ /*The requested field is not in this sweep.*/
                     number_of_gates = 0;
                     nvsi->num_flds_in_sweep = 0;
                     drdh->ref_num_gates = 0;
                     drdh->vel_num_gates = 0;
               }
               else{
                    for(field_index = 0; field_index < number_req_fields; field_index++){ 
                        if(requested[field_index][0] == 'D'){
                           number_of_gates = drdh->ref_num_gates;
                           start_of_data   = drdh->ref_ptr;
                           *gate_sp        = drdh->ref_gate_width;
                           *numgates       = drdh->ref_num_gates;
                           *range_to_first_gate = drdh->ref_gate1;
                           cfldnam[field_index][0] = 'D';
                           cfldnam[field_index][1] = 'Z';
                           cfldnam[field_index][2] = '\0';
                           offset = 32.;
                           the_first_gate = 0;
                           range = drdh->ref_gate1;
                           i = 1;
                           if(drdh->ref_gate1 < 0.0) {
			     for(;;){
                                  range = drdh->vel_gate1 + (i - 1) * drdh->vel_gate_width;
                                  if(range > 0){
                                      *range_to_first_gate = range;
                                      the_first_gate = i - 1;
                                      break;
				  }
                                  i++;
			     }
			   }
                           *numgates = *numgates - the_first_gate;
                            number_of_gates = drdh->ref_num_gates;
                                
                         }/*end reflectivity*/
                         else if(requested[field_index][0] == 'V'){
                           number_of_gates = drdh->vel_num_gates;
                           start_of_data   = drdh->vel_ptr;
                           *gate_sp        = drdh->vel_gate_width;
                           *numgates       = drdh->vel_num_gates;
                           /*In some cases the range to the first velocity gate is a negative
                            *number as documented in the Nexrad Level II documentation from NCDC.
                            *Sprint does not like negative ranges to the first gate when the 
                            *RESET command is used so we use only positive ranges by throwing
                            *out the rage gates that have a negative range.
                            */
                           the_first_gate = 0;
                           range = drdh->vel_gate1;
                           i = 1;
                           if(drdh->vel_gate1 < 0.0) {
			     for(;;){
                                  range = drdh->vel_gate1 + (i - 1) * drdh->vel_gate_width;
                                  if(range > 0){
                                      *range_to_first_gate = range;
                                      the_first_gate = i - 1;
                                      break;
				  }
                                  i++;
			     }
			   }
                           *numgates = *numgates - the_first_gate;
                           number_of_gates = drdh->vel_num_gates;
                           cfldnam[field_index][0] = 'V';
                           cfldnam[field_index][1] = 'E';
                           cfldnam[field_index][2] = '\0';
                           offset = 63.5;
			   /*printf("velocity");*/
                         }
                         else if(requested[field_index][0] == 'S'){
			 /*The number of sw gates equal the number of velocity gates.*/
                            number_of_gates = drdh->vel_num_gates;
                            start_of_data   = drdh->sw_ptr; 
                            *gate_sp        = drdh->vel_gate_width;
                            *numgates       = drdh->vel_num_gates;
                            *range_to_first_gate = drdh->vel_gate1;
                            cfldnam[field_index][0] = 'S';
                            cfldnam[field_index][1] = 'W';
                            cfldnam[field_index][2] = '\0';
                            offset = 63.5;
                         }
                         else{
                            printf("UNKNOWN NEXRAD LEVEL II FIELD REQUESTED: EXITING\n");
                            exit(-1);
                         }

                      /*
                       *Malloc space for the data buffer.
                       */
                       databuff = (double *)malloc(sizeof(double) * number_of_gates); 
                       if(!databuff){
                          printf("UNABLE TO MALLOC MEMORY FOR DATA\n");
                          exit(-1);
                       }
                       gate_index = 0;
		       if((number_of_gates > 0) && (nvsi->processing > 0)){
			   if(field_index == 0) number_of_rays++;
                           for(index = 0; index < number_of_gates; index++){
                               unsignedByte = ndi->fileBuffer[position+ start_of_data + index];
                               if((unsignedByte != 0) && (unsignedByte != 1)){
                                   databuff[index] = (double)((unsignedByte - 2) * 0.5) - offset;
                                   flddat[field_index][gate_index] = (float)databuff[index];
		               }
                               if(index > the_first_gate) gate_index++;
		            }/*for loop for data*/
		       }
                      free(databuff);
		    }/*for loop for field_index*/
  
	          } /*else*/
                  position  = blocks * BYTES_DATA_BLOCK;
		  }
                  else if(blockType ==  RDA_STATUS_DATA){
                      drdh->vel_num_gates  = 0;
                      drdh->ref_num_gates  = 0;
                      memcpy(rda_si,&ndi->fileBuffer[position],rda_size);
                      position  = blocks * BYTES_DATA_BLOCK;
	          }
                  else if(blockType == PERFORMANCE_MAINTENANCE_DATA){
                      printf("PERFORMANCE/MAINTENANCE DATA\n");
                      position  = blocks * BYTES_DATA_BLOCK;
                      drdh->vel_num_gates  = 0;
                      drdh->ref_num_gates  = 0;
	          }
                  else if(blockType == VOLUME_COVERAGE_PATTERN){
                      printf("VOLUME COVERAGE PATTERN\n");
                      position  = blocks * BYTES_DATA_BLOCK;
                      drdh->vel_num_gates  = 0;
                      drdh->ref_num_gates  = 0;
	          } 
                  blocks++;
                  bytesUsed = position;
                  ndi->buffer_position = position;
                  if(bytesUsed >= ndi->bytesBuffer){
                     ndi->bytesBuffer = 0;
                     ndi->buffer_position = 0;
		  }
                  return(DATA_RECORD);
     }
      else if(ndi->bufferPtr == END_OF_FILE){
          printf("back in read nexrad ray\n");
          return(END_OF_FILE);  
     }
     else{
         printf("ERROR READING NEXRAD DATA SOURCE\n");
          return(FILE_READ_ERROR);
     }    

}/*read_nexrad_ray*/

/**********************************************************************/ 
int  read_nexrad_record(char buffer[READ_BUFFER_MAX],int *bytes_in_buffer,
                        int swap)
{
  int  firstBytes,extraBytes; 
  char fileBuffer[READ_BUFFER_MAX];
  int  BytestoRead;
  static int bookkeeping = NO;
  static int read_end_of_file = NO;
  int recLen,rectype;
  int lenBook;
  int endLenBook;
  int lenblocking;
  int ii;
  extern int TapeHeaderPresent;
  extern int Blocking;
  extern FILE *fp; 
  extern int  fpd; 
  extern long cswap32();
  struct mtop op;
/*-----------------------------------------------------------------------*/
  /*This routine reads a Nexrad record.  It checks for the three main types
    of records.  A tape header and file header or a data record.  There may
    or may not be a tape header on the tape and there may or may not be 
    Fortran blocking on the tape.*/
  extraBytes = 0;
  if(*bytes_in_buffer == 0){
     firstBytes = 13;  /* 4 + 9(ARCHIVE2.)*/
     if(tape_device == NO){ /*We have a disk file*/
        recLen = fread (buffer, 1,firstBytes, fp);
        if (recLen != firstBytes)  return(END_OF_FILE);
     }
     else{/*We have a tape drive*/
        recLen = read(fpd,(void *)buffer,READ_BUFFER_MAX);
        if(recLen == 0){
              return(END_OF_TAPE);
        }
        if(recLen == -1){
           printf("BAD READ\n");
           op.mt_op = MTFSR;
	   op.mt_count = 1;	/* SKIP ONE RECORD*/
           if (ioctl(fpd, MTIOCTOP, &op) < 0){
                printf("UNABLE TO RECOVER FROM BAD READ\n");
                return(FILE_READ_ERROR);
	   }
	 }
        if(recLen == BYTES_TAPE_HEADER ){/*Tape header may be followed by an EOF*/
           recLen = read(fpd,(void *)buffer,READ_BUFFER_MAX);
           if(recLen == -1) return(FILE_READ_ERROR);
	}
     }
/*-------------------------------------------------------------------------*/
     if (strncmp("ARCHIVE2",buffer,8) == 0){
         bookkeeping = NO;
         Blocking = NO;
         if (strncmp("ARCHIVE2.",buffer,9) == 0)
         {
             if(TapeHeaderPresent == NO) printf("NO TAPE HEADER PRESENT\n");
             rectype     = FILE_HEADER;
             BytestoRead = BYTES_FILE_HEADER;
         }
         else
         {
             TapeHeaderPresent = YES;
             rectype     = TAPE_HEADER;
             BytestoRead = BYTES_TAPE_HEADER;  
         }
      }  
/*-------------------------------------------------------------------------*/
      else if (strncmp("ARCHIVE2",buffer+4,8) == 0){
         bookkeeping = YES;
         Blocking = YES;
         if (strncmp("ARCHIVE2.",buffer+4,9) == 0)
          {
             rectype     = FILE_HEADER;
             BytestoRead = BYTES_FILE_HEADER;
          }
	  else
          {
             rectype     = TAPE_HEADER;
             BytestoRead = BYTES_TAPE_HEADER;  
             TapeHeaderPresent = YES;
	  }
       }/*else if*/
/*-------------------------------------------------------------------------*/
       else{
            memcpy((void *)&lenBook,buffer,4);
            memcpy((void *)&endLenBook,buffer,4);
            if ((bookkeeping) && (lenBook == 0) && (endLenBook == 0))
                {
                extraBytes = 2 * 4;
                recLen = fread (buffer + firstBytes, 1,extraBytes, fp);
                if (recLen != extraBytes)
                    return END_OF_FILE;
                if (strncmp("ARCHIVE2",buffer+extraBytes+4,8) == 0)
                {
                    if (strncmp("ARCHIVE2.",buffer+extraBytes+4,9) == 0)
                         {
                         rectype = FILE_HEADER;
                         BytestoRead = BYTES_FILE_HEADER;
                         }
                    else
                         {
                         rectype = TAPE_HEADER;
                         BytestoRead = BYTES_TAPE_HEADER;
                         }
                }
                else
                {
                    fprintf(stderr,"In module %s 4-byte logical record"
                     " length read = 0 \n"
                     "This is seen as logical flag for end of file,\n"
                     "though it could also mean incorrect"
                     " file format.\n");
                    return END_OF_FILE;
		    }
	      }/*if(bookeeping and lenbook and endlenbook)*/

	    else
		{
                rectype = DATA_RECORD;
                BytestoRead = BYTES_DATA_BLOCK;
		}

	}/*if bytes_in_buffer == 0*/
/*---------------------------------------------------------------------*/
 

        if(tape_device == YES){
           *bytes_in_buffer = recLen;
           return BytestoRead;
        }

        if (! bookkeeping){
	    recLen = fread (buffer + firstBytes, 1,
                     BytestoRead - firstBytes, fp);
	    if (recLen != BytestoRead - firstBytes)
		return END_OF_FILE;
            return BytestoRead;
        }

/*
 * We have "bookkeeping" info, so use it to determine
 * how long the logical record is.
 */
	memcpy((void *)&lenBook,buffer+extraBytes,4);
        if(swap == 1) lenBook = cswap32((long)lenBook);
	if ((lenBook < MIN_ARCH_BYTES) ||
	    (lenBook > READ_BUFFER_MAX))
	{
	    if (lenBook == 0)
		{
		fprintf(stderr,"In module %s 4-byte logical record"
		 " length read = 0 \n"
		 "This is seen as logical flag for end of file,\n"
		 "though it could also mean incorrect"
		 " file format.\n");
		return END_OF_FILE;
		}
	    return INCORRECT_FORMAT;/* error: couldn't get 
				       the recLen "bookkeeping"*/
       }
/*
 * Read into the buffer the rest of the logical record.
 * Remember: the start of the buffer has the 4 bytes
 * which are not part of the logical record.
*/
	recLen = fread (buffer + firstBytes + extraBytes, 1,
		 lenBook - firstBytes + 4, fp);

	if (recLen != lenBook - firstBytes + 4)
	    return END_OF_FILE;
        *bytes_in_buffer = recLen;
/*
 * While we are at it, read past the trailing "bookkeeping"
 * so that the next time we need to read, we'll be in the right
 * position.
 */
        recLen = fread (&endLenBook, 1,4, fp); /* read past the
                                 trailing "record length bookkeeping" */
        if (recLen != 4)
            return END_OF_FILE;

        *bytes_in_buffer += 4;
	return BytestoRead;
      }

  
}/*read_nexrad_record*/


/**********************************************************************/
/*              ROUTINES TO READ THE HEADERS                          */
/**********************************************************************/
void get_tape_header(char fileBuffer[READ_BUFFER_MAX])
{
 char   radarName[5],radar_name[5],city[25],state[4];
 char   line[128];
 char   str[256];
 char   month[3];
 int    ii,site_number,swp_count;
 int    buffer_index,index,jj,junk;
 int    latd,latm,lats,lond,lonm,lons,found;
 float  latitude,longitude,altitude,frequency_mhz,short_pulse_ns,long_pulse_ns;
 extern struct nexrad_site_info *nsi;
 extern int volume_number;
 extern int  Blocking;
 FILE   *fs;

  if((tape_device == NO) && (Blocking == YES)){
    /*skip the ARCHIVE2 characters and the FORTRAN blocking for disk file*/
    buffer_index = 8 + 4; 
  }
  else{
    buffer_index = 8;
  } 

 /*
  *Get the radar name if there is one.
  */
  memcpy((void *)radarName,&fileBuffer[buffer_index],4);
  radarName[4] = '\0';
  if(!(fs = fopen("nexrad_radar_sites.txt", "r"))) {
     printf("unable to open radar sites file\n");
     exit(0);   
  }

  found = NO;
  while (fgets(line,sizeof(line),fs) != NULL) {
      if(*line == '#')
         continue;
      ii = sscanf(line, "%d%s%s%s%d%d%d%d%d%d%f%f%f%f"
		    , &site_number
		    , radar_name
		    , city
		    , state
		    , &latd
		    , &latm
		    , &lats
		    , &lond
		    , &lonm
		    , &lons
		    , &altitude
		    , &frequency_mhz
		    , &short_pulse_ns
		    , &long_pulse_ns
		    );
       if(strncmp(radarName,radar_name,4) == 0){
          found = YES;
          break;
       }
   }/*end while*/
   fclose(fs);

   if(found == NO){
       return; /*The radar name is not on the tape*/
   }
   nsi->site_number = site_number;
   strcpy(nsi->radar_name,radar_name);
   strcpy(nsi->city,city);
   strcpy(nsi->state,state);
   nsi->latd = latd;
   nsi->latm = latm;
   nsi->lats = lats;
   nsi->lond = lond;
   nsi->lonm = lonm;
   nsi->lons = lons;

   nsi->altitude       = altitude;
   nsi->frequency_mhz  = frequency_mhz;
   nsi->short_pulse_ns = short_pulse_ns;
   nsi->long_pulse_ns  = long_pulse_ns;

   /*
    * PRT is determined by searching this array for a match to the
    * unambiguous range derived from
    * drdh->unamb_range_x10 and remembering the row number (delta)
    * then the nvi->is_prf_num becomes the column index for the true
    * unambiguous range which is then used to calculate PRT or PRF.
    * use nvi->id_prf_num for velocity data.
    */

    if(!(fs = fopen("nexrad_unamb_rngs.txt", "r"))) {
         printf("UNABLE TO OPEN UNAMBIGUOUS RANGE TEXT\n");
         exit(0);
    }

    for(jj=0; fgets(line,sizeof(line),fs) != NULL;) {
	if(*line == '#')
	      continue;
	ii = sscanf(line, "%d%d%d%d%d%d%d%d"
		    , &nex_unamb_rngs[jj][0]
		    , &nex_unamb_rngs[jj][1]
		    , &nex_unamb_rngs[jj][2]
		    , &nex_unamb_rngs[jj][3]
		    , &nex_unamb_rngs[jj][4]
		    , &nex_unamb_rngs[jj][5]
		    , &nex_unamb_rngs[jj][6]
		    , &nex_unamb_rngs[jj][7]
		    );
	jj++;
    }

    
/*
 *Get date and time from buffer.
 */
 buffer_index = 26 + 4;
 sscanf(&fileBuffer[buffer_index],"%2d", &nsi->year);

 buffer_index = 22 + 4;
 month[0] = fileBuffer[buffer_index];
 buffer_index++;
 month[1] = fileBuffer[buffer_index];
 buffer_index++;
 month[2] = fileBuffer[buffer_index]; 

 buffer_index = 19 + 4;
 sscanf(&fileBuffer[buffer_index],"%2d", &nsi->day);

 /*printf("TAPE DATE: %c%c%c  %d,  %d\n",month[0],month[1],month[2],
   nsi->day, nsi->year);*/

}/*get_tape_header*/



/**************************************************************************/

#if defined (IBMRISC) || defined (HP)
  void  get_radar_location(float *radar_lat,float *radar_lon,float *radar_alt,
                           char radarName[4])
#elif defined (CRAY)
  void  GET_RADAR_LOCATION(float *radar_lat,float *radar_lon,float *radar_alt,
                           char radarName[4])
#elif defined (linux)
  void  get_radar_location__(float *radar_lat,float *radar_lon,float *radar_alt,
                             char radarName[4])
#else
  void  get_radar_location_(float *radar_lat,float *radar_lon,float *radar_alt,
                            char radarName[4])
#endif
{
 char   radar_name[5],city[25],state[4];
 char   line[128],character;
 int    latd,latm,lats,lond,lonm,lons,found;
 int    site_number,ii,number;
 float  latitude,longitude,altitude,frequency_mhz,short_pulse_ns,long_pulse_ns;
 FILE   *fs;

  if(!(fs = fopen("nexrad_radar_sites.txt", "r"))) {
     printf("unable to open radar sites file\n");
     exit(0);   
  }

  found = NO;
  while (fgets(line,sizeof(line),fs) != NULL) {
      if(*line == '#')
         continue;
      ii = sscanf(line, "%d%s%s%s%d%d%d%d%d%d%f%f%f%f"
		    , &site_number
		    , radar_name
		    , city
		    , state
		    , &latd
		    , &latm
		    , &lats
		    , &lond
		    , &lonm
		    , &lons
		    , &altitude
		    , &frequency_mhz
		    , &short_pulse_ns
		    , &long_pulse_ns
		    );
      if(strncmp(radarName,radar_name,4) == 0){
          found = YES;
          break;
	  }
   }/*end while*/

   fclose(fs);
   *radar_lat = (float)latd + latm/60. + lats/3600.;  
   if(lond < 0){
      lonm = - lonm;
      lons = - lons;
   }
   *radar_lon = (float)lond + lonm/60. + lons/3600.;
   *radar_alt = altitude;

}/*end get_radar_location*/

      
/**************************************************************************/
#if defined (IBMRISC) || defined (HP)
  void  get_radar_name(char name[4])
#elif defined (CRAY)
  void GET_RADAR_NAME((char name[4])
#elif defined (linux)
  void  get_radar_name__(char name[4])
#else
  void  get_radar_name_(char name[4])
#endif
{
  extern struct nexrad_site_info *nsi;
  extern int    TapeHeaderPresent;


  if(TapeHeaderPresent == NO){
      return;
  }

  if(strncmp(name,nsi->radar_name,4) != 0)
     strncpy(name,nsi->radar_name,4);
  
}/*end get_radar_info*/

/**************************************************************************/
  
void get_file_header(char fileBuffer[READ_BUFFER_MAX],int swap)  
{

 int    julianDay;
 int    year,month,day;
 int    hour,minute,second,millisecond;
 int    buffer_index;
 int    jul70,i;
 int    daymillisecs;
 static int have_vcp_info = 0;
 NxDate date;
 NxTime time;
 static char    module[]="getArchFileHeader";
 extern int  Blocking;
 extern int  TapeHeaderPresent;
 extern struct nexrad_volume_scan_info *nvsi;
 extern long cswap32();


 if(Blocking == YES)
    buffer_index = 12 + 4 + 8;
 else
    buffer_index = 12;
 memcpy((void *)&jul70,&fileBuffer[buffer_index],4);
 if(swap == YES) jul70 = cswap32((long)jul70);
 jul70ymd(jul70, &year, &month, &day, &julianDay);
 nvsi->day = day;
 nvsi->year = year;
 nvsi->month = month;
 
 if(Blocking == YES)
    buffer_index = 16 + 4 + 8;
 else
    buffer_index = 16;
 memcpy((void *)&daymillisecs,&fileBuffer[buffer_index],4);
 if(swap == YES) daymillisecs = cswap32((long)daymillisecs);
 millisecsHMSM(daymillisecs, &hour, &minute, &second,&millisecond);
 
 nvsi->hour = hour;
 nvsi->minute = minute;
 nvsi->second = second;
 nvsi->millisecond = millisecond;


}/*get_file_header*/  
 


/**********************************************************************/
/*                       MESSAGE HEADER                               */
/**********************************************************************/
void get_message_header(char fileBuffer[READ_BUFFER_MAX],int *blockType,
                        int position)
{
int      buffer_index,index,i;
int      jul70;
int      year, month, day, julianDay;
int      hour, minute, second, millisec;
int      daymillisecs;
int      CTM_size;
Int2b    tempInt2b;
extern   struct   nexrad_message_header *nmh;
extern struct nexrad_volume_scan_info *nvsi;
extern long cswap32();
int      nmh_size;


 CTM_size = sizeof(struct CTM_info);
 nmh_size = sizeof(struct nexrad_message_header);
 buffer_index = position;

 memcpy((void *)nmh,&fileBuffer[buffer_index],nmh_size);
 *blockType = nmh->message_type; /* = DIGITAL_RADAR_DATA or other */
 if (*blockType != DIGITAL_RADAR_DATA)
    {
      memcpy((void *)(&tempInt2b),&nmh->julian_date,2);
      jul70 = tempInt2b;          
      jul70 = jul70 & 0177777; /* get only bottom 16 bits */
      memcpy((void *)&daymillisecs,&nmh->milliseconds_past_midnight,4);      
    }
  else{
       memcpy((void *)(&tempInt2b),&nmh->julian_date,2);
       jul70 = tempInt2b;          
       jul70 = jul70 & 0177777; /* get only bottom 16 bits */
       memcpy((void *)&daymillisecs,&nmh->milliseconds_past_midnight,4);

  }

  jul70ymd(jul70, &year, &month, &day, &julianDay);
  daymillisecs = cswap32((long)daymillisecs);
  millisecsHMSM(daymillisecs, &hour, &minute, &second,&millisec);

}/*get_message_header*/



/**********************************************************************/
/*                  DIGITAL RADAR DATA HEADER                         */
/**********************************************************************/
void get_digital_radar_data_header(char fileBuffer[READ_BUFFER_MAX],
                                   int position_of_drdh,
                                   char fields_available[MXFLD][8],
                                   int swap)
{
int      buffer_index,index,i;
int      jul70;
int      year, month, day, julianDay;
int      hour, minute, second, millisec;
int      daymillisecs;
int      drdh_size;
int      special_sweep;
int      vcp_info[4];
long     gates;
Int2b    tempInt2b;
float    prf;
float    fixed_angle;
extern   struct digital_radar_data_header *drdh;
extern   struct nexrad_volume_scan_info *nvsi;
extern   struct   nexrad_VCP_items *nvi;
extern   int current_VCP;
extern   int nex_unamb_rngs[5][8];
extern   int TapeHeaderPresent;
extern   long cswap32();
extern   unsigned short cswap16();

 drdh_size = sizeof(struct digital_radar_data_header);
 
 buffer_index =  position_of_drdh;
 memcpy((void *)drdh,&fileBuffer[buffer_index],drdh_size);

 memcpy((void *)(&tempInt2b),&drdh->julian_date,2);
 jul70 = tempInt2b;    
 jul70 = jul70 & 0177777; /* get only bottom 16 bits */
 if(swap == YES) jul70 = cswap16(jul70);      

 memcpy((void *)&daymillisecs,&drdh->milliseconds_past_midnight,4);
 if(swap == YES) daymillisecs = cswap32((long)daymillisecs);

 jul70ymd(jul70, &year, &month, &day, &julianDay);
 millisecsHMSM(daymillisecs, &hour, &minute, &second, &millisec);
 
  nvsi->year        = year;
  nvsi->day         = day;
  nvsi->month       = month;

  nvsi->hour        = hour;
  nvsi->minute      = minute;
  nvsi->second      = second;
  nvsi->millisecond = millisec;

 /*
  *Determine how many fields are in the present sweep.
  */
  
  nvsi->num_flds_in_sweep = 0;
  if(swap == YES){
     drdh->ref_num_gates = cswap16(drdh->ref_num_gates);
     drdh->vel_num_gates = cswap16(drdh->vel_num_gates);
     drdh->elev_num      = cswap16(drdh->elev_num);
     drdh->vol_coverage_pattern = cswap16(drdh->vol_coverage_pattern);
     drdh->ref_ptr       = cswap16(drdh->ref_ptr);
     drdh->vel_ptr       = cswap16(drdh->vel_ptr);
     drdh->sw_ptr        = cswap16(drdh->sw_ptr);
     drdh->ref_gate_width = cswap16(drdh->ref_gate_width);     
     drdh->vel_gate_width = cswap16(drdh->vel_gate_width);
     drdh->ref_gate1      = cswap16(drdh->ref_gate1);
     drdh->vel_gate1      = cswap16(drdh->vel_gate1);
     drdh->nyquist_vel_x100 = cswap16(drdh->nyquist_vel_x100);
     drdh->elevation      = cswap16(drdh->elevation);
     drdh->azimuth        = cswap16(drdh->azimuth);
     drdh->radial_status  = cswap16(drdh->radial_status);
   }


  if(nvsi->processing == 0){
     return;
  }

  if(drdh->ref_num_gates > 0){
     nvsi->num_flds_in_sweep++;
  }
  if(drdh->vel_num_gates > 0){
   /*vel + sw will be on the tape so add 2*/
    nvsi->num_flds_in_sweep = nvsi->num_flds_in_sweep + 2;
  }
  
 /*
  *Load field names. These are the fields that are available on the tape.
  */
  if((drdh->ref_num_gates > 0) && (drdh->vel_num_gates == 0)){
     fields_available[0][0] = 'D';
     fields_available[0][1] = 'Z';
     fields_available[0][2] = '\0';
     fields_available[1][0] = ' ';
     fields_available[1][1] = ' ';
     fields_available[1][2] = '\0';
     fields_available[2][0] = ' ';
     fields_available[2][1] = ' ';
     fields_available[2][2] = '\0';
  }
  else if((drdh->ref_num_gates == 0) && (drdh->vel_num_gates > 0)){
     fields_available[0][0] = 'V';
     fields_available[0][1] = 'E';
     fields_available[0][2] = '\0';
     fields_available[1][0] = 'S';
     fields_available[1][1] = 'W';
     fields_available[1][2] = '\0';
     fields_available[2][0] = ' ';
     fields_available[2][1] = ' ';
  }
  else if((drdh->ref_num_gates > 0) && (drdh->vel_num_gates > 0)){  
     fields_available[0][0] = 'D';
     fields_available[0][1] = 'Z';
     fields_available[0][2] = '\0';
     fields_available[1][0] = 'V';
     fields_available[1][1] = 'E';
     fields_available[1][2] = '\0';
     fields_available[2][0] = 'S';
     fields_available[2][1] = 'W';
     fields_available[2][2] = '\0';
  }
  else{
     fields_available[0][0] = '?';
     fields_available[0][1] = '?';
     fields_available[0][2] = '\0';
     fields_available[1][0] = '?';
     fields_available[1][1] = '?';
     fields_available[1][2] = '\0';
     fields_available[2][0] = '?';
     fields_available[2][1] = '?';
     fields_available[2][2] = '\0';
  }     


 /*
  * Get the fixed angle
  */
  current_VCP = drdh->vol_coverage_pattern;
  get_VCP_info(current_VCP,drdh->elev_num,vcp_info);
  if(vcp_info[0] == 0) fixed_angle = -999.;
  else  fixed_angle = (vcp_info[0]/8.) * (180./4096.);
  nvsi->fixed_angle = fixed_angle;
}

/*******************************************************************************/
void read_vcp_file()
{
 char   str[256];
 char   temp[4],wavel[5];
 char   line[128];
 int    vcpnum,ii,fixed,azrate,numpulses,count;
 FILE   *fs;
 extern int vcp11[68];
 extern int vcp12[68];
 extern int vcp21[48];
 extern int vcp31[36];
 extern int vcp32[32];
 extern int vcp300[20];
 extern int vcp121[70];


  if(!(fs = fopen("nexrad_VCPs.txt", "r"))) {
     printf("unable to open nexrad volume coverage pattern file\n");
     exit(0);   
  }

  while (fgets(line,sizeof(line),fs) != NULL) {
	if(*line == '#')
	      continue;
	if((line[0] == 'V') && (line[1] == 'C') && (line[2] == 'P')){
	    /* this is a new pattern */
            temp[0] = line[3];
            temp[1] = line[4];
            if(line[5] == ' '){
               temp[2] = '\0';
               wavel[0] = line[6];
               wavel[1] = line[7];
               wavel[2] = line[8];
	    }
            else{
                temp[2] = line[5];
                temp[3] =  '\0';
                wavel[0] = line[7];
                wavel[1] = line[8];
                wavel[2] = line[9];
            }
            vcpnum = atoi(temp);

            if(vcpnum == 11){
               vcp11[0] = 11; 
               wavel[3] = '\0';
               vcp11[1] = atoi(wavel);
               count = 2;
            }
            else if(vcpnum == 12){
               vcp12[0] = 12; 
               wavel[3] = '\0';
               vcp12[1] = atoi(wavel);
               count = 2; 
	    }              
            else if(vcpnum == 21){
               vcp21[0] = 21; 
               wavel[3] = '\0';
               vcp21[1] = atoi(wavel);
               count = 2; 
	    } 
            else if(vcpnum == 31){   
               vcp31[0] = 31; 
               wavel[3] = '\0';
               vcp31[1] = atoi(wavel);
               count = 2; 
	    }                
            else if(vcpnum == 32){   
               vcp32[0] = 32; 
               wavel[3] = '\0';
               vcp32[1] = atoi(wavel);
               count = 2; 
	    }   
            else if(vcpnum == 300){   
               vcp300[0] = 300; 
               wavel[3] = '\0';
               vcp300[1] = atoi(wavel);
               count = 2; 
	    } 
            else if(vcpnum == 121){
               vcp121[0] = 121; 
               wavel[3] = '\0';
               vcp121[1] = atoi(wavel);
               count = 2; 
	    }      
            else{
               printf("vcp number was not found\n");
               exit(0);
            }                           
        }
        else{
            ii = sscanf(line, "%d%d%d",&fixed,&numpulses,&azrate);
            if(vcpnum == 11){
               vcp11[count] = fixed;
               count++;
               vcp11[count] = numpulses;
               count++;
               vcp11[count] = azrate;
               count++;
	    }
            else if(vcpnum == 12){
               vcp12[count] = fixed;
               count++;
               vcp12[count] = numpulses;
               count++;
               vcp12[count] = azrate;
               count++;
	    } 
            else if(vcpnum == 21){  
               vcp21[count] = fixed;
               count++;
               vcp21[count] = numpulses;
               count++;
               vcp21[count] = azrate;
               count++;
	    }  
            else if(vcpnum == 31){  
               vcp31[count] = fixed;
               count++;
               vcp31[count] = numpulses;
               count++;
               vcp31[count] = azrate;
               count++;
	    }        
            else if(vcpnum == 32){  
               vcp32[count] = fixed;
               count++;
               vcp32[count] = numpulses;
               count++;
               vcp32[count] = azrate;
               count++;
	    }   

            else if(vcpnum == 300){  
               vcp300[count] = fixed;
               count++;
               vcp300[count] = numpulses;
               count++;
               vcp300[count] = azrate;
               count++;
	    }   
            else if(vcpnum == 121){  
               vcp121[count] = fixed;
               count++;
               vcp121[count] = numpulses;
               count++;
               vcp121[count] = azrate;
               count++;
	    }  
        }
  }

    fclose(fs);
}

/*******************************************************************************/
 void get_VCP_info(int vcp_num,int el_num,int vcp_info[4])
{
	int fix_angle;
	int pulse_cnt;
	int az_rate;
	int pulse_width;
        int index;

	/* case statement to get vcp info */
	switch(vcp_num) {
	case 11:
          index = 3*el_num;
	  fix_angle =   vcp11[(3*el_num)-1];
	  pulse_cnt =   vcp11[(3*el_num)];
	  az_rate =     vcp11[(3*el_num)+1];
	  pulse_width = vcp11[1];
	  break;
	case 12:
          index = 3*el_num;
	  fix_angle =   vcp12[(3*el_num)-1];
	  pulse_cnt =   vcp12[(3*el_num)];
	  az_rate =     vcp12[(3*el_num)+1];
	  pulse_width = vcp12[1];
	  break;
	case 21:
	  fix_angle =   vcp21[(3*el_num)-1];
	  pulse_cnt =   vcp21[(3*el_num)];
	  az_rate =     vcp21[(3*el_num)+1];
	  pulse_width = vcp21[1];
	  break;
	case 31:
	  fix_angle =   vcp31[(3*el_num)-1];
	  pulse_cnt =   vcp31[(3*el_num)];
	  az_rate =     vcp31[(3*el_num)+1];
	  pulse_width = vcp31[1];
	  break;
	case 32:
	  fix_angle =   vcp32[(3*el_num)-1];
	  pulse_cnt =   vcp32[(3*el_num)];
	  az_rate =     vcp32[(3*el_num)+1];
	  pulse_width = vcp32[1];
	  break;
	case 300:
	  fix_angle =   vcp300[(3*el_num)-1];
	  pulse_cnt =   vcp300[(3*el_num)];
	  az_rate =     vcp300[(3*el_num)+1];
	  pulse_width = vcp300[1];
	  break;
	case 121:
	  fix_angle =   vcp121[(3*el_num)-1];
	  pulse_cnt =   vcp121[(3*el_num)];
	  az_rate =     vcp121[(3*el_num)+1];
	  pulse_width = vcp121[1];
	  break;
	default:
          printf("VCP INFORMATION FOR VCP %d WAS NOT FOUND\n",vcp_num);
	  fix_angle  = 0;
	  pulse_cnt  = 0;
	  az_rate    = 0;
	  pulse_width= 0;
	  break;
	}
	
	/* get array for output	*/
	vcp_info[0]=fix_angle;
	vcp_info[1]=pulse_cnt;
	vcp_info[2]=az_rate;
	vcp_info[3]=pulse_width;

}/*void get_VCP_info*/
/*******************************************************************************/
void swap_four_bytes(void *word)
{
 unsigned char *byte;
 unsigned char temp;
 byte = word;
 temp = byte[0];
 byte[0] = byte[1];
 byte[1] = temp;
}

/*******************************************************************************/
void init_nexrad_structures()
{
 
  void read_vcp_file();

  if(!nsi){
     nsi = (struct nexrad_site_info *)
	  malloc(sizeof(struct nexrad_site_info));
     if(!nsi){
         printf("UNABLE TO MALLOC SPACE FOR RADAR SITE STURCURE\n");
         exit(0);
     }
  }

  if(!nvsi){
     nvsi = (struct nexrad_volume_scan_info *)
	  malloc(sizeof(struct nexrad_volume_scan_info));
     if(!nvsi){
        printf("UNABLE TO MALLOC SPACE FOR VOLUME INFORMATION STURCURE\n");
        exit(0);
     }
  }

  if(!ndi){
     ndi = (struct nexrad_data_info *)
	  malloc(sizeof(struct nexrad_data_info));
     if(!ndi){
        printf("UNABLE TO MALLOC SPACE FOR NEXRAD DATA INFO\n");
        exit(0);
     }
  }

  if(!nmh){
     nmh = (struct nexrad_message_header *)
	    malloc(sizeof(struct nexrad_message_header));

     if(!nmh){
        printf("UNABLE TO MALLOC SPACE FOR THE NEXRAD MESSAGE HEADER\n");
        exit(0);
     }
  }
  memset(nmh, 0, sizeof(struct nexrad_message_header));

  if(!drdh){
     drdh = (struct digital_radar_data_header *)
	     malloc(sizeof(struct digital_radar_data_header));

     if(!drdh){
         printf("UNABLE TO MALLOC SPACE FOR THE DIGITAL RADAR DATA HEADER\n");
         exit(0);
     }
  }
  memset(drdh, 0, sizeof(struct digital_radar_data_header));

  if(!rda_si){
     rda_si= (struct rda_status_info *)malloc( sizeof( struct rda_status_info ));
     if(!rda_si){
        printf("UNABLE TO MALLOC SPACE FOR THE RDA STATUS INFO STRUCT\n");
        exit(0);
     }
  }

  memset(rda_si, 0, sizeof( struct rda_status_info ));

  read_vcp_file();

}/*end initialize structures*/
/*******************************************************************************/
#ifndef THEDATE 
#define THEDATE "<30-Oct-1997 21:23:51><UTC>"
#endif

#include "rah.h"

/*
 * jul70ymd
 * Take the jul70 (Julian day from 1-1-1970) and set
 *      year YYYY, month MM, day DD, and normal julian day.
 * Programmer: Robert Hueftle NOAA/NSSL at NCAR/MMM
 *
 * Input conditions:
 *  The jul70 must be >= 1.
 * Exit conditions:
 * jul70ymd returns 1 if success and 0 if failure.
 *    If success, then  
 *          *year, *month, *day will be the year, month, day, and
 *          *julianDay will be the julian day
 *      associated with the date passed.
 *    If failure, then all the return variables are undefined.
 */

int jul70ymd(int jul70, int * year, int * month, int * day, int * julianDay)
{
#define DAYS_N_4_YEARS (365*4 +1)/*days in normal 4 consecutive years: 1 leap */
#define DAYS_400_YEARS (365*400 +100 - 3)/* days in any 400 consecutive years */
/* DAYS_N_100_YEARS:  days in normal 100 consecutive yrs: not including
   any of the 4*100 years, i.e., not including 2000, 2400, 2800, etc. */
#define DAYS_N_100_YEARS (365*100+ 25 -1)/* days in normal 100 consecutive yrs*/
/*  DAYS_AB_100_YEARS: days in "abnormal" 100 consecutive yrs,  including
   any of the 4*100 years, i.e., including 2000, 2400, 2800, etc.*/
#define DAYS_AB_100_YEARS (365*100+25) /* days in abnormal 100 consecutive yrs*/
#define DAYS_FIRST_130_YEARS (365*130 + 25 + 7 ) /* days to 1-1-2100 */
#define DAYS_FIRST_131_YEARS (365*131 + 25 + 8 -1) /* days to 1-1-2101 */

    int   yearsPassed, years4passed, years400passed;

    if (jul70 < 1)
	return 0;


    yearsPassed = 0;  /* we'll increment this as we whittle down jul70 */
    if (jul70 > DAYS_FIRST_130_YEARS)
	{
/*
 * Since 2000 is a leap year (unlike 1900, 2100, 2200, 2300, etc.) the
 * first 130 years behave regularly.  So only 1-1-2100 and onward do
 * we have to deal with the exception years divisible by 4 not being leap years.
 */
	years400passed = (jul70 - 1) / DAYS_400_YEARS; /* integer division */
	yearsPassed += 400 * years400passed;
	jul70 -= DAYS_400_YEARS * years400passed;
/*
 * At this point we are whittled back to somewhere
 * between 1-1-1970 and 12-31-2369.
 */
        while (jul70 > DAYS_FIRST_131_YEARS)
	    {
	    jul70 -= DAYS_N_100_YEARS;
	    yearsPassed += 100;
            }
/*
 * At this point we whittled jul70 back to somewhere
 * between 1-1-1970 and 12-31-2100.  We don't want to stay in the year 2100
 * because it is not a leap year, so do one more test.
 */

        if (jul70 > DAYS_FIRST_130_YEARS)
	    {
	    jul70 -= DAYS_AB_100_YEARS;
	    yearsPassed += 100;
            }
        }
/*
 * At this point we whittled jul70 back to somewhere
 * between 1-1-1970 and 12-31-2099, where every year divisible by 4 is a
 * leap year.
 */
    years4passed = (jul70 - 1) / DAYS_N_4_YEARS; /* integer division */
    yearsPassed += 4 * years4passed;
    jul70 -= DAYS_N_4_YEARS * years4passed;
/*
 * At this point we whittled jul70 back to somewhere
 * between 1-1-1970 and 12-31-1973.
 */
    if (jul70 <= 365)
	{
	*year = 1970 + yearsPassed;
	*julianDay = jul70;
	}
    else if (jul70 <= 2*365)
	{
	*year = 1971 + yearsPassed;
	*julianDay = jul70 - 365;
	}
    else if (jul70 <= (2*365 + 366))
	{
/*
 * Here we have a likely candidate for a leap year, i.e., divisible by 4.
 * Only by looking at the year will we know if we have one or not.
 */
	*year = 1972 + yearsPassed;
	*julianDay = jul70 - 365*2;
	}
    else if (jul70 <= (3*365 + 366))
	{
	*year = 1973 + yearsPassed;
	*julianDay = jul70 - (365*2 + 366);
	}
    else{
        printf("bad date\n");
	exit(-1);
    }

    return julDayMMDD( *julianDay, *year, month, day); /* get month & day */
} /* jul70ymd ends */

/*********************************************************************************/
/*
 * julDayMMDD
 * Take Julian Day and year YYYY and get month MM and day DD.
 * Input conditions:
 *  julDay: julian day 
 *  year: YYYY, must be >= 1800.
 * Exit conditions:
 * julDayMMDD returns 1 if success and 0 if failure.
 *      If success, then  *tMonth, *tDay are the components of the
 *      date associated with the Julian Day and the year.
 *      If failure, then tMonth and tDay are undefined.
 */
int julDayMMDD(int julDay, int year, int *tMonth, int *tDay)
{
    int    leapDay;
    int    jj;
    int    julianDays[12]= {31,59,90,120,151,181,212,243,273,304,334,365};


    if ((year % 4 == 0) && ((year %400 == 0) || (year %100 != 0)))
	leapDay = 1;
    else
	leapDay = 0;
    if ((julDay>365 + leapDay) || (julDay <1) || (year <1800))
	return 0;
    if (julDay <= 31)
	{
	*tMonth = 1;
	*tDay = julDay;
	}
    else
	{
        jj = 1;
        while (julDay > julianDays[jj] + leapDay)
	    jj++;
	*tMonth = jj+1;
	*tDay = julDay - (julianDays[jj-1] + leapDay); /* OK except for Feb */
	if (*tMonth == 2)
	    *tDay += leapDay;
	}
    return 1;
} /* julDayMMDD ends */
/*********************************************************************************/


/*
 * millisecsHMSM
 * Take the millisecs for a day's time and convert to
 *      HH, MM, SS, MSECS.
 * Programmer: Robert Hueftle NOAA/NSSL at NCAR/MMM
 *
 * Input conditions:
 *  The dayMilli must be >= 0 and <= 24*60*60*1000.
 * Exit conditions:
 * millisecsHMSM returns 1 if success and 0 if failure.
 *    If success, then  
 *          *year, *month, *day will be the year, month, day, and
 *      associated with the millisecs passed.
 *    If failure, then all the return variables are undefined.
 */
int millisecsHMSM(int dayMilli, int * hours, int * minutes, int * seconds,
	  int * milliSecs)
{
#define DAY_MILLISECS    (24 *60*60*1000) /* milliseconds in a day */
#define HOUR_MILLISECS   (    60*60*1000) /* milliseconds in a hour */
#define MINUTE_MILLISECS (       60*1000) /* milliseconds in a minute */
#define SECOND_MILLISECS (          1000) /* milliseconds in a second */
    if ( (dayMilli < 0) || (dayMilli > DAY_MILLISECS))
	return 0;

    *hours = dayMilli / HOUR_MILLISECS;
    *minutes = (dayMilli - (*hours * HOUR_MILLISECS)) / MINUTE_MILLISECS;
/*
 * Use modulo to get the lower end.
 */
    *seconds = (dayMilli % MINUTE_MILLISECS) / SECOND_MILLISECS;
    *milliSecs = dayMilli % SECOND_MILLISECS;
    return 1;
} /* int millisecsHMSM ends */
/*********************************************************************************/

#ifndef THEDATE
#define THEDATE "<06-Feb-1995 17:18:30><UTC>"
#endif

/*
 *  Programming: Robert A. Hueftle, NOAA/NSSL, at NCAR/MMM
 */

/*
 ******************* Include files *********************
 */
/*
 *** C include files ***
 */

/* 
 * NxOkTime:
 *   check that the date/time are acceptable.
 *   Years before 1800 are not acceptable.
 *   Return 1 if acceptable, 0 if not.
 */
short int NxOkTime (NxDate date, NxTime time)
{
    
    short int idays[12] = {31,28,31,30,31,30,31,31,30,31,30,31};
    short int days;

    if ((date.year < 1800) || (date.month < 1) || (date.month >12))
	return 0;                     /* out of range!! */
    days = idays[date.month - 1];     /* get number of days in month */
    if ((date.month ==2) && (date.year%4 ==0)&& 
	 ((date.year%100 !=0) || (date.year%400 ==0))) 
	days = 29; /*leap year every 4 y. except centuries not divis by 400 */
    if ((date.day < 1) || ((int) date.day > days) ||
	(time.hour < 0) || (time.hour > 23) ||
	(time.minute < 0) || (time.minute > 59) ||
	(time.second < 0) || (time.second > 59) ||
	(time.milliSecond < 0) || (time.milliSecond > 999))
	return 0;
    return 1;
}
/* NxOkTime ends */

/***************************************************************/
#if defined (IBMRISC) || defined (HP)
  void nexrad_close_file(char tape_unit[8],int *unit)
#elif defined (CRAY)
  void NEXRAD_CLOSE_FILE(char tape_unit[8],int *unit)
#elif defined (linux)
  void nexrad_close_file__(char tape_unit[8],int *unit)
#else
  void nexrad_close_file_(char tape_unit[8],int *unit)
#endif
{

 extern int fpd;
 extern FILE *fp;

  printf("in nexrad close file\n");
  if(*unit == 0){
    if(close(fpd) == -1){
       printf("error closing tape device\n");
    }
  }
  else{
    if(fclose(fp) == -1){   
       printf("error closing data file\n");
    }
  }
}/*close_file*/

/******************************************************************/
#if defined (IBMRISC) || defined (HP)
  void nexrad_skip_volume(int *istat,int *swapping)
#elif defined (CRAY)
  void NEXRAD_SKIP_VOLMUE(int *istat,int *swapping)
#elif defined (linux)
  void nexrad_skip_volume__(int *istat,int *swapping)
#else
  void nexrad_skip_volume_(int *istat,int *swapping)
#endif
{
 int    position,blockType,blocks;
 int    CTM_size,rda_size,nmh_size;
 int    end_of_volume,bytesUsed,swap;
 char   fields_available[MXFLD][8];
 struct mtop op;
 extern int fpd;
 extern int tape_device;
 extern FILE *fp;
 extern struct nexrad_data_info *ndi;
 extern struct nexrad_volume_scan_info *nvsi;
 extern int Blocking;


  CTM_size = sizeof(struct CTM_info);
  nmh_size = sizeof(struct nexrad_message_header);
  rda_size = sizeof(struct rda_status_info);
 
  if(tape_device == YES){ /*we have a tape drive*/
     op.mt_op = MTFSF;
     op.mt_count = 1;	/* SKIP ONE FILE MARKER*/
     if (ioctl(fpd, MTIOCTOP, &op) < 0){
         printf("UNABLE TO REPOSITION TAPE DRIVE\n");
         *istat = 6;
         return;
     }  
     else{
         *istat = 1;
     }
     ndi->bytesBuffer = 0;
     ndi->buffer_position = 0;
  }
  else{/*disk file*/
     swap = *swapping;
     ndi->bytesBuffer = 0;
     end_of_volume = 0;
     bytesUsed = 0;
     while(end_of_volume == 0){
         if(ndi->bytesBuffer <= 0){  
            ndi->bufferPtr = read_nexrad_record(ndi->fileBuffer,
                             &ndi->bytesBuffer,0);
            blocks = 1; 
            position = 0;
            ndi->buffer_position = 0;
         }
         else{
            position = ndi->buffer_position;
         }     
         /*
          *Determine what type of record we have by its length.
          */
          if(ndi->bufferPtr == BYTES_FILE_HEADER){
               get_file_header(ndi->fileBuffer,swap);     
               ndi->bytesBuffer = 0;
          }
          else if(ndi->bufferPtr == BYTES_TAPE_HEADER){
               ndi->bytesBuffer = 0;
          }
          else if(ndi->bufferPtr == BYTES_DATA_BLOCK){
              if(Blocking == YES){
                 position = position + CTM_size + 4;
	      }
              else{
                 position = position + CTM_size;
	      }
              get_message_header(ndi->fileBuffer,&blockType,position);
              position  = position + nmh_size;
              if(blockType == DIGITAL_RADAR_DATA){
                 get_digital_radar_data_header(ndi->fileBuffer,position,
                                               fields_available,swap);
                 if(drdh->radial_status == 4) end_of_volume = 1;
                 position  = blocks * BYTES_DATA_BLOCK;
	      }
              else if(blockType ==  RDA_STATUS_DATA){
                   position  = blocks * BYTES_DATA_BLOCK;
	      }
              else if(blockType == PERFORMANCE_MAINTENANCE_DATA){
                   position  = blocks * BYTES_DATA_BLOCK;
	      }
              else if(blockType == VOLUME_COVERAGE_PATTERN){
                   position  = blocks * BYTES_DATA_BLOCK;
	      } 
              blocks++;
              bytesUsed = position;
              ndi->buffer_position = position;
              if(bytesUsed >= ndi->bytesBuffer){
                 ndi->bytesBuffer = 0;
                 ndi->buffer_position = 0;
	      }
        }/*end if for BYTES_DATA_BLOCK*/
        else if(ndi->bufferPtr == END_OF_FILE){
             *istat = 6;
        }
        else{
             printf("ERROR READING NEXRAD DATA SOURCE\n");
             *istat = 7;
        }    
     }/*end the while*/
   }/*End of the else for disk file*/
}/*nexrad_skip_volume*/
