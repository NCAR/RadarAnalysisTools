#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <memory.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <dirent.h>
#include "cedric.h"
#include "./include/mdv_macros.h"
#include "./include/mdv_handle.h"
#include "./include/mdv_field_handle.h"
#include "./include/port_types.h"
#include "./include/mdv_read.h"
#include "./include/os_config.h"
#include "./include/mdv_print.h"
#include "./include/mdv_file.h"
#include "./include/mdv_user.h"
#include "./include/mdv_write.h"
#include "./include/mdv_utils.h"


#define MDV_READ_SUCCESSFUL             0
#define MDV_READ_OPEN_FAILURE           1
#define MDV_READ_BAD_MASTER_HDR         2
#define MDV_READ_INVALID_FIELD_NUM      3
#define MDV_READ_BAD_FIELD_HDR          4
#define MDV_READ_BAD_VLEVEL_HDR         5
#define MDV_READ_NO_VLEVEL_HDRS         6
#define MDV_READ_INVALID_CHUNK_NUM      7
#define MDV_READ_BAD_CHUNK_HDR          8
#define MDV_READ_DATA_ARRAY_TOO_SMALL   9
#define MDV_READ_DATA_ERROR            10
#define MDVFSI32                       39
#define MDVFFL32                       31
#define MDVFNXIX                        9
#define MDVFNYIX                       10
#define MDVFNZIX                       11
#define MDVFGDX                        12
#define MDVFGDY                        13
#define MDVFGDZ                        14
#define MDVFXMIN                       15
#define MDVFYMIN                       16
#define MDVFZMIN                       17

#define MDVMAXGRID                     25000000   /*5000*5000*/

#define RAP  1
#define HDF  2
#define GRIB 3
#define CEDRIC 3

#ifndef BOOL_STR
#define BOOL_STR(a) (a == FALSE ? "false" : "true")
#endif

/*
 *The following mdv_file_information structure and cdata char pointer are used
 *only in the routines in this file.
 */
struct mdv_file_information{
  FILE   *mdv_in;
  FILE   *mdv_out;
  int    bytes_master;
  int    field_data_offset;
  int    offset_vlevels;
  int    vlevels;
  int    nativev;
  int    projection;
  int    vtype[NFMAX];
  int    num_fields;
  int    in_unit;
  int    time_forecast;
  int    data_type;
  int    swap;
  int    num_points;
  int    numx;
  int    numy;
  int    src_flg;
  int    grids_differ;
  int    nvertlevels[NFMAX];
  int    iscale[NFMAX];
  double vertorder[NFMAX][122];
  double vertspacing[NFMAX];
  double old_grid[6];
  double mdvbias[NFMAX];
  double lat;
  double lon;
  double mdvscale[NFMAX];
  double mdvbad[NFMAX];
  double mdvmiss[NFMAX];
  double minv[NFMAX][122];
  double maxv[NFMAX][122];
  double avev[NFMAX][122];
};
static struct mdv_file_information *mdvptr;

/*
 *cdata is an array for holding the data for 25 fields.
 *It is malloced in rdfieldh and freed in mdvfree.  Mdvfree
 *is called from READVL.f and TRANSF.f.
 */
unsigned char   *cdata[NFMAX];

float *fdata;
char field_names[NFMAX][8];
char radarname[6];

/***********************************************************************/
void init_MDV_struct()
{
   extern struct mdv_file_information *mdvptr;
   int index;
   static int initialized = 0;
   extern char radarname[6];

   if(initialized == 1) return;

 /*
  *Initialize the mdv structure used to write out mdv format.
  */
  mdvptr = (struct mdv_file_information *)malloc(sizeof(struct  mdv_file_information));
  if(!mdvptr){
     printf("UNABLE TO MALLOC SPACE FOR MDV_CDF STRUCTURE\n");
     exit(-1);
  }

  for(index = 0; index < NFMAX; index++) mdvptr->mdvscale[index] = -1;
  initialized = 1;
  mdvptr->nativev = -999;
  strcpy(radarname,"UNKNOW");
}/*init_MDV_struct*/


/*****************************************************************
 * MDV_COMPRESSION2STRING: Convert the compression type integer to an
 * ascii string.  See mdv_macros for the data order declarations.
 */

char * MDV_compression2string(int compression_type)
{
  switch(compression_type)
  {
  case MDV_COMPRESSION_NONE :
    return("MDV_COMPRESSION_NONE");
  case MDV_COMPRESSION_RLE :
    return("MDV_COMPRESSION_RLE");
  case MDV_COMPRESSION_LZO :
    return("MDV_COMPRESSION_LZO");
  case MDV_COMPRESSION_ZLIB :
    return("MDV_COMPRESSION_ZLIB");
  case MDV_COMPRESSION_BZIP :
    return("MDV_COMPRESSION_BZIP");
  case MDV_COMPRESSION_GZIP :
    return("MDV_COMPRESSION_GZIP");
  default:
    return("Unknown compression type");
  }
}


/*****************************************************************
 * MDV_TRANSFORM2STRING: Convert the transform type integer to an
 * ascii string.  See mdv_macros for the data order declarations.
 */

char * MDV_transform2string(int transform_type)
{
  switch(transform_type)
  {
  case MDV_TRANSFORM_NONE :
    return("MDV_TRANSFORM_NONE");
  case MDV_TRANSFORM_LOG :
    return("MDV_TRANSFORM_LOG");
  default:
    return("Unknown transform type");
  }
}


/*****************************************************************
 * MDV_SCALING2STRING: Convert the scaling type integer to an
 * ascii string.  See mdv_macros for the data order declarations.
 */

char * MDV_scaling2string(int scaling_type)
{
  switch(scaling_type)
  {
  case MDV_SCALING_NONE :
    return("MDV_SCALING_NONE");
  case MDV_SCALING_ROUNDED :
    return("MDV_SCALING_ROUNDED");
  case MDV_SCALING_DYNAMIC :
    return("MDV_SCALING_DYNAMIC");
  case MDV_SCALING_INTEGRAL :
    return("MDV_SCALING_INTEGRAL");
  case MDV_SCALING_SPECIFIED :
    return("MDV_SCALING_SPECIFIED");
  default:
    return("Unknown scaling type");
  }
}
/*****************************************************************
 * MDV_ENCODE2STRING: Convert the encoding type integer to ascii 
 * string.  See mdv_macros for the encoding type declarations.
 * --Rachel Ames 1/96
 */

char * MDV_encode2string(int encodeing_type)
{

   switch(encodeing_type)  {
      case MDV_NATIVE :
        return("NATIVE"); 
      case MDV_INT8 :
	return("MDV_INT8 (CHAR/BYTE)"); 
      case MDV_INT16:
	return("MDV_INT16 (SHORT)"); 
      case MDV_FLOAT32 :
	return("MDV_FLOAT32 (FLOAT)"); 
      case MDV_PLANE_RLE8 :
	return("MDV_PLANE_RLE8"); 
      default:
	return("Unknown Encoding Type"); 
   }
}  
 char * MDV_proj2string(int proj_type)
{

   switch(proj_type)  {
      case MDV_PROJ_NATIVE :
        return("Native"); 
      case MDV_PROJ_LATLON :
	return("Latitude/Longitude Grid (units in degrees)"); 
      case MDV_PROJ_ARTCC :
	return("ARTCC"); 
      case MDV_PROJ_STEREOGRAPHIC :
	return("Stereographice"); 
      case MDV_PROJ_LAMBERT_CONF :
	return("Lambert Conformal"); 
      case MDV_PROJ_MERCATOR :
	return("Mercator"); 
      case MDV_PROJ_POLAR_STEREO :
	return("Polar Stereographic"); 
      case MDV_PROJ_POLAR_ST_ELLIP :
	return("Polar Sterographic Equidistant"); 
      case MDV_PROJ_CYL_EQUIDIST :
	return("Cylindrical Equidistant"); 
      case MDV_PROJ_FLAT :
	return("Flat (Cartesian) (units in KM)"); 
      case MDV_PROJ_POLAR_RADAR :
	return("Polar Radar"); 
      case MDV_PROJ_RADIAL :
	return("Radial"); 
      default:
	return("Unknown Projection Type"); 
   }

}  
/******************************************************************************
 * MDV_LOAD_MASTER_HEADER: Load mdv data file header into given area from
 * disk.  Memory for the master header is assumed to be allocated before
 * this routine is called.  The bytes in the header are swapped if necessary
 * to put the data in native format.
 *
 * Inputs: infile - pointer to the input file.  This is assumed to currently
 *                  be open for read.
 *         m_hdr - pointer to the master header to be loaded.
 *
 * Outputs: m_hdr - updated to include the values read in from disk,
 *                  byte swapped as necessary.
 *
 * Returns: MDV_SUCCESS or MDV_FAILURE.
 */

int MDV_load_master_header( FILE *infile, MDV_master_header_t *m_hdr)

{
  
  /* master headers are always at the beginning */
  if (fseek(infile,0,SEEK_SET)) {
    return MDV_FAILURE;
  }

  if((fread(m_hdr,sizeof(MDV_master_header_t),1,infile)) != 1) {
    return MDV_FAILURE;
  }

  MDV_master_header_from_BE(m_hdr);
  
  return MDV_SUCCESS;

}/*MDV_load_master_header*/

/***********************************************************************
*
* RDMASTER: Read the master header from a data file and
* return the information in FORTRAN-usable structures.
*
* Use the following to call the subroutine:
*
*    CHARACTER*8 FNAME
*    INTEGER*4      MASTER_HDR_INTS(MDV_NUM_MASTER_HEADER_SI32)
*    REAL*4         MASTER_HDR_REALS(MDV_NUM_MASTER_HEADER_FL32)
*    CHARACTER*(MDV_INFO_LEN)  DATASET_INFO
*    CHARACTER*(MDV_NAME_LEN)  DATASET_NAME
*    CHARACTER*(MDV_NAME_LEN)  DATASET_SOURCE
*    INTEGER        RETURN_STATUS
*
*   RDMASTER(INUNIT, MASTER_HDR_INTS, MASTER_HDR_REALS,
*                              DATASET_INFO, DATASET_NAME, DATASET_SOURCE,
*		               RETURN_STATUS)
*
* This function will open the file named FNAME, read the master header.
* The header information is returned in the given arrays.
* 
* When finished, RETURN_STATUS can be MDV_READ_SUCCESSFUL,
*                                     MDV_READ_OPEN_FAILURE,
*                                     MDV_READ_BAD_MASTER_HDR
* Author : Nancy Rehak, RAP
* Modified: Sherrie Fredrick,MMM to work with Cedric
*/
void FORTRAN_NAME(rdmaster)(
                 int  *INUNIT,
		 si32 *master_hdr_ints,
		 fl32 *master_hdr_reals,
		 char  *dataset_info,
		 char  *dataset_name,
		 char  *dataset_source,
                 float valnyq[61],
		 int   *return_status,
                 char  volnam[8])
{
  int i,unit,temp,rw;
  int index,jindex,startindex;
  int format = MDVFMT;
  int writeflg = 0;
  int numnyquist;
  char test[11];
  MDV_master_header_t master_hdr;
  void mf_master_hdr_to_fortran();
  FILE * mdv_file_open();
  extern struct mdv_file_information *mdvptr;  
  extern void save_latlonalt();
  extern void set_format_type();
  struct tm *ptr;
  time_t print_time;
  extern void set_global_swap_flag();
  extern char radarname[6]; 


 /*
  *Open the input file.
  */
  init_MDV_struct();
  unit = *INUNIT;
  rw = 0;
  mdvptr->mdv_in = mdv_file_open(unit,rw);
  mdvptr->in_unit = *INUNIT;

  set_format_type(format,writeflg);

  /*
   * Read the master header
   */

   if (MDV_load_master_header(mdvptr->mdv_in, &master_hdr) == MDV_FAILURE)
      {
         fprintf(stderr, "Error reading master header from file unit fort.%d\n",
	         unit);
         fclose(mdvptr->mdv_in);
         exit(-1);
      }

   mdvptr->swap = 0;
   if(master_hdr.struct_id != MDV_MASTER_HEAD_MAGIC_COOKIE){
        MDV_master_header_to_BE(&master_hdr);
        set_global_swap_flag();
        mdvptr->swap = 1;
   }


   print_time = master_hdr.time_begin;
   ptr = gmtime(&print_time);
   ptr->tm_mon = ptr->tm_mon + 1;
   if(ptr->tm_mon < 10){
      volnam[0] = '0';
      volnam[1] = (char)ptr->tm_mon + 48;
   }
   else{
      temp = ptr->tm_mon/10;
      volnam[0] = (char)temp + 48;
      temp = ptr->tm_mon%10;
      volnam[1] = (char)temp + 48;
   }
   if(ptr->tm_mday < 10){   
      volnam[2] = '0';
      volnam[3] = (char)ptr->tm_mday + 48;
   }         
   else{
      temp = ptr->tm_mday/10;
      volnam[2] = (char)temp + 48;
      temp = ptr->tm_mday%10;
      volnam[3] = (char)temp + 48;
   }   


   if(ptr->tm_hour < 10){
      volnam[4] = '0';
      volnam[5] = (char)ptr->tm_hour + 48;
   }
   else{
      temp = ptr->tm_hour/10;
      volnam[4] = (char)temp + 48;
      temp = ptr->tm_hour%10;
      volnam[5] = (char)temp + 48;
   }      

   
   if(ptr->tm_min < 10){
      volnam[6] = '0';
      volnam[7] = (char)ptr->tm_hour + 48;
   }
   else{
      temp = ptr->tm_min/10;
      volnam[6] = (char)temp + 48;
      temp = ptr->tm_min%10;
      volnam[7] = (char)temp + 48;
   }  

   volnam[8] = ' ';


   /*
    * Copy the master header info
    */

    mf_master_hdr_to_fortran(&master_hdr,
			   master_hdr_ints,
			   master_hdr_reals,
			   dataset_info,
			   dataset_name,
			   dataset_source);
  
    *return_status = MDV_READ_SUCCESSFUL;
    mdvptr->bytes_master = master_hdr.record_len1;
    mdvptr->vlevels = master_hdr.vlevel_included;
    mdvptr->num_fields = master_hdr.n_fields;
    mdvptr->offset_vlevels = master_hdr.vlevel_hdr_offset;
    mdvptr->num_points = master_hdr.max_nx * master_hdr.max_ny;
    mdvptr->numx =  master_hdr.max_nx;
    mdvptr->numy =  master_hdr.max_ny;
    mdvptr->grids_differ = master_hdr.field_grids_differ;
    save_latlonalt(master_hdr.sensor_lat,master_hdr.sensor_lon,
                   master_hdr.sensor_alt);
    mdvptr->nativev = master_hdr.native_vlevel_type;
    mdvptr->src_flg = 0;
    mdvptr->lat = master_hdr.sensor_lat;
    mdvptr->lon = master_hdr.sensor_lon;


    /*
     *Load the NYQUIST velocities.
     */
    startindex = 0;
    for(index = 0; index < 6; index++){
        numnyquist = master_hdr.user_data_si32[index];
        if(numnyquist != -999){
           for(jindex = 0; jindex < numnyquist; jindex++){
               valnyq[startindex] = master_hdr.user_data_fl32[index];
               startindex++;
	   }
	}
    }
    if(master_hdr.data_set_info[0] == 'R'){
       test[0] = master_hdr.data_set_info[1];
       test[1] = master_hdr.data_set_info[2];
       test[2] = master_hdr.data_set_info[3];
       test[3] = master_hdr.data_set_info[4];
       test[4] = master_hdr.data_set_info[5];
       test[5] = master_hdr.data_set_info[6];
       test[6] = master_hdr.data_set_info[7];
       test[7] = master_hdr.data_set_info[8];
       test[8] = master_hdr.data_set_info[9];
       test[9] = master_hdr.data_set_info[10];
       if(strncmp(test,"ADAR NAME:",10) == 0) {
          radarname[0] = master_hdr.data_set_info[12];
          radarname[1] = master_hdr.data_set_info[13];
          radarname[2] = master_hdr.data_set_info[14];
          radarname[3] = master_hdr.data_set_info[15];
          radarname[4] = ' ';
          radarname[5] = ' ';
       }
    }
    if((master_hdr.data_set_name[0] == 'R') && (master_hdr.data_set_name[1] == 'A')
        && (master_hdr.data_set_name[2] == 'P')) mdvptr->src_flg = RAP;


    if(mdvptr->src_flg == 0){
       for(index = 0; index < MDV_NAME_LEN; index++){
          if(master_hdr.data_set_name[index] == 'h'){
            if(master_hdr.data_set_name[index+1] == 'd' && 
               master_hdr.data_set_name[index+2] == 'f') mdvptr->src_flg = HDF;
        }
          else if (master_hdr.data_set_name[index] == 'H'){
               if(master_hdr.data_set_name[index+1] == 'D' && 
                  master_hdr.data_set_name[index+2] == 'F') mdvptr->src_flg = HDF;

         }
       }
    }

    if(mdvptr->src_flg == 0){
       for(index = 0; index < MDV_NAME_LEN; index++){
           if(master_hdr.data_set_name[index] == 'g'){
              if(master_hdr.data_set_name[index+1] == 'r' && 
                 master_hdr.data_set_name[index+2] == 'i') mdvptr->src_flg = GRIB;
           }
           else if (master_hdr.data_set_name[index] == 'G'){
                if(master_hdr.data_set_name[index+1] == 'R' && 
                  master_hdr.data_set_name[index+2] == 'I') mdvptr->src_flg = GRIB;

           }
      }    
    }

    if(mdvptr->src_flg == 0){
      if((master_hdr.data_set_name[0] == 'C') && (master_hdr.data_set_name[1] == 'E') &&
          (master_hdr.data_set_name[2] == 'D') ) mdvptr->src_flg = CEDRIC;
    }

  
    return;

}/*mf_rm_read_master_hdr*/


/***********************************************************************
 * mf_master_hdr_to_fortran:  Fills in the FORTRAN arrays from the given
 *                            MDV_master_header_t structure.
 */

void mf_master_hdr_to_fortran(MDV_master_header_t *master_hdr,
			      si32 *master_hdr_ints,
			      fl32 *master_hdr_reals,
			      char *dataset_info,
			      char *dataset_name,
			      char *dataset_source)
{
  /*
   * Load the information into the arrays.
   */

  memcpy(master_hdr_ints, &master_hdr->struct_id,
	 sizeof(si32) * MDV_NUM_MASTER_HEADER_SI32);
  memcpy(master_hdr_reals, master_hdr->user_data_fl32,
	 sizeof(fl32) * MDV_NUM_MASTER_HEADER_FL32);
  memcpy(dataset_info, master_hdr->data_set_info, MDV_INFO_LEN);
  memcpy(dataset_name, master_hdr->data_set_name, MDV_NAME_LEN);
  memcpy(dataset_source, master_hdr->data_set_source, MDV_NAME_LEN);
  return;
}


/***********************************************************************
* LOAD CEDRIC INFORMATION *ldcedi*: This routine will get information needed by the 
* Cedric 510 word header from the MDV master header.
*
*
*    SI32 and fl32 are defined in port_types.h
*    int         master_hdr_ints(MDV_NUM_MASTER_HEADER_SI32)
*    float        master_hdr_reals(MDV_NUM_MASTER_HEADER_FL32)
*    character*(MDV_INFO_LEN) dataset_info
*    character*(MDV_NAME_LEN) dataset_name
*    character*(MDV_NAME_LEN) dataset_source
*    int        SCAN_TYPE  CART,ELEV OR COPLANE 
*    float      location - array containing the lat,lon and altitude of the sensor.
*    int        beg_time - beginning time of the data set
*    int        end_time - ending time of the data set
*    int        nflds    - the number of fields on the tape
*    int        numxyz   - the max number of array elements in the x,y,z direction.    
*    int        mdv_order- the ordering of cells in the arrray.
*  
*
*/
void FORTRAN_NAME(loadcedi)(
                             si32 *master_hdr_ints,fl32 *master_hdr_reals,
                             char *dataset_info,char *dataset_name,char *dataset_source,
                             int  *scan_type,float location[3],
                             int  *beg_time,int *end_time,
                             int  *nflds, int *numxyz, int *mdv_order
                          )  

{
  MDV_master_header_t master_hdr;
  void MDV_print_master_header_full();
  void mf_master_hdr_from_fortran();


  /*
   * Load the information in the master header
   */

  mf_master_hdr_from_fortran(master_hdr_ints,
			     master_hdr_reals,
			     dataset_info,
			     dataset_name,
			     dataset_source,
			     &master_hdr);


  *scan_type  = master_hdr.vlevel_type;
  location[0] = master_hdr.sensor_lat;
  location[1] = master_hdr.sensor_lon;
  location[2] = master_hdr.sensor_alt * 1000.;
  *beg_time   = master_hdr.time_begin;
  *end_time   = master_hdr.time_end;
  *nflds      = master_hdr.n_fields;
  numxyz[0]   = master_hdr.max_nx;
  numxyz[1]   = master_hdr.max_ny; 
  numxyz[2]   = master_hdr.max_nz;
  *mdv_order  = master_hdr.grid_order_indices;
  return;
}

/*****************************************************************
 * MDV_PRINT_MASTER_HEADER_FULL: print out all of the master
 * header info
 * --Nancy Rehak 4/96
 */

void MDV_print_master_header_full(MDV_master_header_t *mmh, FILE *outfile)
{
  int i;
  time_t print_time;         /* temporary storage area for printing time */
                             /*   values -- necessary on systems which have */
                             /*   time_t defined as other than 32-bit */
  
  fprintf(outfile, "\n");
  fprintf(outfile, "           MDV_print_master_header\n");
  fprintf(outfile, "           -----------------------\n");
  fprintf(outfile, "\n");
  fprintf(outfile, "record_len1:          %d\n", mmh->record_len1);
  fprintf(outfile, "struct_id:            %d\n", mmh->struct_id);
  fprintf(outfile, "revision_number:      %d\n", mmh->revision_number);
  fprintf(outfile, "\n");
  print_time = mmh->time_gen;
  fprintf(outfile, "time_gen:             %s",
	  asctime(gmtime(&print_time)));
  fprintf(outfile, "user_time:           %d\n", mmh->user_time);
  print_time = mmh->time_begin;
  fprintf(outfile, "time_begin:           %s",
	  asctime(gmtime(&print_time)));
  print_time = mmh->time_end;
  fprintf(outfile, "time_end:             %s",
	  asctime(gmtime(&print_time)));
  print_time = mmh->time_centroid;
  fprintf(outfile, "time_centroid:        %s",
	  asctime(gmtime(&print_time)));
  print_time = mmh->time_expire;
  if (mmh->time_expire == 0)
     fprintf(outfile, "time_expire:          %d\n",mmh->time_expire);
  else
     fprintf(outfile, "time_expire:          %s",
	     asctime(gmtime(&print_time)));
  fprintf(outfile, "num_data_times:       %d\n", mmh->num_data_times);
  fprintf(outfile, "index_number:         %d\n", mmh->index_number);
  fprintf(outfile, "data_dimension:       %d\n", mmh->data_dimension);
  fprintf(outfile, "data_collection_type: %s\n",
	  MDV_colltype2string(mmh->data_collection_type));
  fprintf(outfile, "user_data:            %d\n", mmh->user_data);
  fprintf(outfile, "native_vlevel_type:   %s\n",
	  MDV_verttype2string(mmh->native_vlevel_type));
  fprintf(outfile, "vlevel_type:          %s\n",
	  MDV_verttype2string(mmh->vlevel_type));
  fprintf(outfile, "vlevel_included:      %s\n",
	  BOOL_STR(mmh->vlevel_included));
  fprintf(outfile, "grid_order_direction: %s\n",
	  MDV_orient2string(mmh->grid_order_direction));
  fprintf(outfile, "grid_order_indices:   %s\n",
	  MDV_order2string(mmh->grid_order_indices));
  fprintf(outfile, "n_fields:             %d\n", mmh->n_fields);
  fprintf(outfile, "max_nx:               %d\n", mmh->max_nx);
  fprintf(outfile, "max_ny:               %d\n", mmh->max_ny);
  fprintf(outfile, "max_nz:               %d\n", mmh->max_nz);
  fprintf(outfile, "n_chunks:             %d\n", mmh->n_chunks);
  fprintf(outfile, "field_hdr_offset:     %d\n", mmh->field_hdr_offset);
  fprintf(outfile, "vlevel_hdr_offset:    %d\n", mmh->vlevel_hdr_offset);
  fprintf(outfile, "chunk_hdr_offset:     %d\n", mmh->chunk_hdr_offset);
  fprintf(outfile, "field_grids_differ:   %s\n",
          BOOL_STR(mmh->field_grids_differ));
  for (i = 0; i < 8; i++)
    fprintf(outfile, "user_data_si32[%d]:    %d\n",
	    i, mmh->user_data_si32[i]);
  
  fprintf(outfile, "\n");
  for (i = 0; i < 6; i++)
    fprintf(outfile, "user_data_fl32[%d]:    %f\n",
	    i, mmh->user_data_fl32[i]);
  fprintf(outfile, "sensor_lon:           %f\n", mmh->sensor_lon);
  fprintf(outfile, "sensor_lat:           %f\n", mmh->sensor_lat);
  fprintf(outfile, "sensor_alt:           %f\n", mmh->sensor_alt);
  fprintf(outfile, "\n");
  fprintf(outfile, "data_set_info:        <%s>\n", mmh->data_set_info);
  fprintf(outfile, "data_set_name:        <%s>\n", mmh->data_set_name);
  fprintf(outfile, "data_set_source:      <%s>\n", mmh->data_set_source);
  fprintf(outfile, "\n");
  fprintf(outfile, "record_len2:          %d\n", mmh->record_len2);
  fprintf(outfile, "\n\n");

  return;
}  
 

/***********************************************************************
 * mf_master_hdr_from_fortran:  Fills in the MDV_master_header_t structure
 *                              from the given FORTRAN arrays.
 */

void mf_master_hdr_from_fortran(si32 *master_hdr_ints,
				fl32 *master_hdr_reals,
				char *dataset_info,
				char *dataset_name,
				char *dataset_source,
				MDV_master_header_t *master_hdr)
{
  /*
   * Load the information from the arrays
   */

  memcpy(&master_hdr->struct_id, master_hdr_ints,
         MDV_NUM_MASTER_HEADER_SI32 * sizeof(si32));
  
  memcpy(master_hdr->user_data_fl32, master_hdr_reals,
         MDV_NUM_MASTER_HEADER_FL32 * sizeof(fl32));
  
  memcpy(master_hdr->data_set_info, dataset_info, MDV_INFO_LEN);

  memcpy(master_hdr->data_set_name, dataset_name, MDV_NAME_LEN);
  
  memcpy(master_hdr->data_set_source, dataset_source, MDV_NAME_LEN);
  
  /*
   * Make sure the strings are NULL terminated
   */

  master_hdr->data_set_info[MDV_INFO_LEN-1] = '\0';
  master_hdr->data_set_name[MDV_NAME_LEN-1] = '\0';
  master_hdr->data_set_source[MDV_NAME_LEN-1] = '\0';
  
  /*
   * Set the record length values
   */

  master_hdr->record_len1 = sizeof(MDV_master_header_t) - 2 * sizeof(si32);
  master_hdr->record_len2 = master_hdr->record_len1;
  
  return;
}
 
/***********************************************************************
* MF_pm_print_master_hdr: This routine will print an MDV master header
* from FORTRAN format data.
*
* Use the following to call the subroutine:
*
*    INTEGER*4     MASTER_HDR_INTS(MDV_NUM_MASTER_HEADER_SI32)
*    REAL*4        MASTER_HDR_REALS(MDV_NUM_MASTER_HEADER_FL32)
*    CHARACTER*(MDV_INFO_LEN) DATASET_INFO
*    CHARACTER*(MDV_NAME_LEN) DATASET_NAME
*    CHARACTER*(MDV_NAME_LEN) DATASET_SOURCE
*
*   CALL MF_PM_PRINT_MASTER_HDR(MASTER_HDR_INTS, MASTER_HDR_REALS,
*                               DATASET_INFO, DATASET_NAME, DATASET_SOURCE)
*
*/
void FORTRAN_NAME(mf_pm_print_master_hdr)(
			     si32 *master_hdr_ints,
                             fl32 *master_hdr_reals,
                             char *dataset_info,
                             char *dataset_name,
			     char *dataset_source)
{
  MDV_master_header_t master_hdr;
  
  /*
   * Load the information in the master header
   */

  mf_master_hdr_from_fortran(master_hdr_ints,
			     master_hdr_reals,
			     dataset_info,
			     dataset_name,
			     dataset_source,
			     &master_hdr);
  
  /*
   * Print the master header
   */

  MDV_print_master_header_full(&master_hdr, stdout);
  
  return;
}



/**********************************************************************/
void FORTRAN_NAME(mdvradnm)(
                 char radar[6],int *baseangle)
{

   int    index,city_index,save_index,nexrad_file;
   char   mdvcity[25],city[25],state[4],radar_name[4];
   char   line[128],character;
   char   temp[6];
   int    latd,latm,lats,lond,lonm,lons,found;
   int    site_number,ii,number,length;
   float  latitude,longitude,altitude,frequency_mhz,short_pulse_ns,long_pulse_ns;
   FILE   *fs;
   extern char radarname[6];

   radar[0] = radarname[0];
   radar[1] = radarname[1];
   radar[2] = radarname[2];
   radar[3] = radarname[3];
   radar[4] = radarname[4];
   radar[5] = radarname[5];
   *baseangle = 0;

}/*mdv_radar_name*/

/******************************************************************************
 * MDV_read_field_volume
 *
 * From an open file, read the volume data for the given field.
 *
 * The field header must already have been read an swapped into host
 * byte order. You are responsible for making sure the field_header is
 * correct for the open file
 *
 * You must specify output_encoding, output_compression and output_scaling
 * types. See <mdv/mdv_file.h>
 *
 * The field header is updated to reflect the output types and sizes.
 *
 * Returns: returns a pointer to the data volume, byte swapped if
 *          necessary, or NULL if there is an error.  This space for this
 *          data volume is allocated by this routine and must be freed
 *          by the calling routine using ufree().
 *
 *          Also, if output_volume_len is not NULL the length of the 
 *          field buffer is copied to it.
 */

void * MDV_read_field_volume(FILE *infile,
			     MDV_field_header_t *f_hdr,
			     int output_encoding,
			     int output_compression,
			     int output_scaling,
			     double output_scale,
			     double output_bias,
			     int *output_volume_len)
     
{

  MDV_field_handle_t *fhand;
  void *out_buf;
  int out_len;
  extern void *umalloc();

  /*
   * create a field_handle, read in the field
   */

  fhand = MDV_fhand_create_from_parts(f_hdr, NULL);

  /*
   * read in field data
   */

  if (MDV_fhand_read_vol(fhand, infile)) {
    MDV_fhand_delete(fhand);
    return (NULL);
  }

  /*
   * convert the data as appropriate
   */
  
  if (MDV_fhand_convert(fhand, 
			output_encoding, output_compression,
			output_scaling, output_scale, output_bias)) {
    fprintf(stderr, "ERROR - MDV_read_field_volume\n");
    fprintf(stderr, "  Cannot convert field '%s'\n", f_hdr->field_name);
    MDV_fhand_delete(fhand);
    return (NULL);
  }

  /*
   * allocate buffer for data, copy data in
   */

  out_len = MDV_fhand_get_vol_len(fhand);
  out_buf = umalloc(out_len);
  memcpy(out_buf, MDV_fhand_get_vol_ptr(fhand), out_len);
  *f_hdr = *(MDV_fhand_get_hdr(fhand));
  
  /*
   * free up
   */

  MDV_fhand_delete(fhand);

  if (output_volume_len != NULL) {
    *output_volume_len = out_len;
  }

  return(out_buf);

}
/***********************************************************************
* MF_RF_READ_FIELD_HDR: Read the indicated field header from a data file
* and return the information in FORTRAN-usable structures.
*
* Use the following to call the subroutine:
*
*    CHARACTER*1024 FNAME
*    INTEGER        FIELD_NUM
*    INTEGER*4      FIELD_HDR_INTS(MDV_NUM_FIELD_HEADER_SI32)
*    REAL*4         FIELD_HDR_REALS(MDV_NUM_FIELD_HEADER_FL32)
*    CHARACTER*(MDV_LONG_FIELD_LEN) FIELD_NAME_LONG
*    CHARACTER*(MDV_SHORT_FIELD_LEN) FIELD_NAME_SHORT
*    CHARACTER*(MDV_UNITS_LEN) FIELD_UNITS
*    CHARACTER*(MDV_TRANSFORM_LEN) FIELD_TRANSFORM
*    CHARACTER*(MDV_UNITS_LEN) FIELD_UNUSED_CHAR
*    INTEGER        RETURN_STATUS
*
*   CALL RDFIELDH(INUNIT, FIELD_NUM,
*                 FIELD_HDR_INTS, FIELD_HDR_REALS,
*                 FIELD_NAME_LONG, FIELD_NAME_SHORT,
*                 FIELD_UNITS, FIELD_TRANSFORM, FIELD_UNUSED_CHAR,
*		  F_NAME,RETURN_STATUS)
*
* This function will open the file named FNAME and read the indicated field
* header.  The header information is returned in the given arrays.
* 
* When finished, RETURN_STATUS can be MDV_READ_SUCCESSFUL,
*                                     MDV_READ_OPEN_FAILURE,
*                                     MDV_READ_BAD_MASTER_HDR,
*                                     MDV_READ_INVALID_FIELD_NUM
*                                     MDV_READ_BAD_FIELD_HDR
*/
void FORTRAN_NAME(rdfieldh)(
		 int *INUNIT,
                 int *field_num,
                 si32 *field_hdr_ints,
                 fl32 *field_hdr_reals,
                 char *field_name_long,
                 char *field_name_short,
                 char *field_units,
                 char *field_transform,
                 char *field_unused_char,
                 char f_name[8],
                 int  *return_status)
{

  MDV_field_header_t field_hdr;
  int index,len_short,unit,field,return_size,len_long;
  int array_size,data_size,requested_data_format;
  int format,num_points,sourceflg,i,nplane;
  int output_volume_len;
  int output_encoding,output_compression,output_scaling,output_scale;
  int output_bias;
  float tempf;
  void *volume_data;

  void mf_field_hdr_to_fortran(); 
  int MDV_load_field_header();

  extern unsigned char *cdata[NFMAX];
  extern struct mdv_file_information *mdvptr; 
  extern void MDV_field_header_to_BE();

  /*
   * Read the field header
   */

  if (MDV_load_field_header(mdvptr->mdv_in, &field_hdr, *field_num) != MDV_SUCCESS)
  {
    fprintf(stderr, "Error loading field %d header from file\n", *field_num);
    fclose(mdvptr->mdv_in);
    *return_status = MDV_READ_BAD_FIELD_HDR;
    return;
  }
  
  if(field_hdr.struct_id != MDV_FIELD_HEAD_MAGIC_COOKIE){
      MDV_field_header_to_BE(&field_hdr);
  }


  /*
   * Copy the field header info
   */

  mf_field_hdr_to_fortran(&field_hdr,
                          field_hdr_ints,
                          field_hdr_reals,
                          field_name_long,
                          field_name_short,
                          field_units,
                          field_transform,
                          field_unused_char);
  

  /*
   * Read the field data into global char data array.
   */
  
  field = *field_num;
  mdvptr->data_type = field_hdr.encoding_type;
  mdvptr->old_grid[0] =  field_hdr.grid_minx;
  mdvptr->old_grid[1] =  field_hdr.grid_minx + (mdvptr->numx - 1)*field_hdr.grid_dx;
  mdvptr->old_grid[2] =  field_hdr.grid_miny;
  mdvptr->old_grid[3] =  field_hdr.grid_miny + (mdvptr->numy - 1)*field_hdr.grid_dy;
  mdvptr->old_grid[4] =  field_hdr.grid_dx;
  mdvptr->old_grid[5] =  field_hdr.grid_dy;
  mdvptr->vertspacing[field] = field_hdr.grid_dz;
  num_points = field_hdr.nx * field_hdr.ny * field_hdr.nz;
  mdvptr->nvertlevels[field] = field_hdr.nz;
  if(!mdvptr->vlevels){
      mdvptr->vertorder[field][0] = field_hdr.grid_minz;
      for(index = 1; index < field_hdr.nz; index++){
          mdvptr->vertorder[field][index] = mdvptr->vertorder[field][index-1] + field_hdr.grid_dz;
      }
  }


  nplane = mdvptr->numx * mdvptr->numy;
  cdata[field] = (unsigned char *)malloc(nplane * field_hdr.nz * sizeof(char));
  if(!cdata){
      printf("unable to malloc space for mdv field data\n");
      exit(-1);
  }  

  /*INPORTANT NOTE:*/
  /*
   *The following lines specify the output encoding_type,output_compression,
   *and output_scaling.  This is for the output data only. The types accepted 
   *by the mdv routine MDV_read_field_volume are outlined in the mdv_file.h
   *file in the include directory.  The field header (field_hdr) is passed
   *into the MDV_read_field_volume routine and is read in there so we
   *don't need to worry about the encoding types of the data on the
   *data source itself.
   *For the purpose of Cedric we request the output data to be of type 
   *MDV_INT8, or 8 byte integers.  This way it can easily be put into
   *an unsigned char array after reading.  
   *The data is originaly put in a void * (void pointer array) since
   *this is what the mdv routine  MDV_read_field_volume expects.
   *It is then put into our cdata array by the following statement:
   *memcpy(cdata[field],volume_data,output_volume_len);
   *The data for all levels for a given field is read in all at once
   *and stored in the cdata[field] array.  
   */
  output_encoding = MDV_INT8;
  output_compression = MDV_COMPRESSION_NONE;
  output_scaling = MDV_SCALING_SPECIFIED;
  output_scale = field_hdr.scale;
  output_bias =  field_hdr.bias;

  if((volume_data =  MDV_read_field_volume(mdvptr->mdv_in,&field_hdr,output_encoding,
                     output_compression,output_scaling,output_scale,
		     output_bias,&output_volume_len)) == NULL)
    {
       fprintf(stderr, "Error loading data for field %d from file\n",
            *field_num);
      fclose(mdvptr->mdv_in);
      exit(-1);
    }
    memcpy(cdata[field],volume_data,output_volume_len);
  
  format = MDVFMT;
  

  mdvptr->iscale[field] = 10;
  if(field_hdr.max_value > 3276.8){
     if(field_hdr.max_value > 32768) mdvptr->iscale[field] = -1;
     if(field_hdr.min_value < 0) tempf = -field_hdr.min_value;
     if(tempf > 32768) mdvptr->iscale[field] = -2;
     else mdvptr->iscale[field] = 1;
  }

 /*
  *MDV Sattelite data has several fields that just have the name temperature.  The
  *word temperature has too many letters to fit into the Cedric header so we use
  *the alternative name instead.
  */
  len_short = strlen(field_name_short);
  len_long  = strlen(field_name_long);
  if(len_short > 8){
     if(len_long > 8){
        printf("FIELD NAME %s IS TOO LONG TO BE REPRESENTED IN CEDRIC\n",field_name_short);
        printf("CEDRIC ACCEPTS FIELDS NAMES UP TO 8 CHARACTERS\n");
     }
     else{
        printf("-------------------------------------------------------------------------\n");
        printf("WARNING MDV FIELD NAME %s HAS TOO MANY CHARACTERS.\n",field_name_short); 
        printf("USING THE ALTERNATE NAME %s IN THE FIELD HEADER INSTEAD\n",field_name_long);
        printf("SEE FIELD HEADER FOR THIS FIELD BELOW\n");
        strcpy(f_name,field_name_long);
        for(index = len_long; index < 8; index++) f_name[index] = ' ';
     }
   }
   else{
     strcpy(f_name,field_name_short);
     for(index = len_short; index < 8; index++) f_name[index] = ' ';
     }

  *return_status = MDV_READ_SUCCESSFUL;

  return;
}
/*****************************************************************************/
void sclinfo_(float *scale, float *bias, float *bad, float *missing, int *num)
{
  extern struct mdv_file_information *mdvptr;
  int field_num;

  field_num = *num - 1;  
  mdvptr->mdvscale[field_num] = *scale;
  mdvptr->mdvbias[field_num]  = *bias;
  mdvptr->mdvbad[field_num]   = *bad;
  mdvptr->mdvmiss[field_num]  = *missing;

}

/******************************************************************************
 * MDV_LOAD_FIELD_HEADER: Load mdv field header data into the given structure
 * from disk.  Memory for the field header is assumed to be allocated before
 * this routine is called.  The bytes in the header are swapped if necessary
 * to put the data in native format.
 *
 * Inputs: infile - pointer to the input file.  This is assumed to currently
 *                  be open for read.
 *         f_hdr - pointer to the field header to be loaded.
 *         field_num - number of the field being loaded.  This is used to
 *                     determine the position on disk where the field header
 *                     information is located.
 *
 * Outputs: f_hdr - updated to include the values read in from disk, byte
 *                  swapped as necessary.
 *
 * Returns: MDV_SUCCESS or MDV_FAILURE.
 */
int MDV_load_field_header(FILE *infile, MDV_field_header_t *f_hdr,
			  int field_num)
{
  long hdr_offset;
  
  hdr_offset = sizeof(MDV_master_header_t) +
    (field_num * sizeof(MDV_field_header_t));

  if (fseek(infile, hdr_offset, SEEK_SET)) {
     printf("error seeking\n");
    return MDV_FAILURE;
  }
  
  if((fread(f_hdr, sizeof(MDV_field_header_t),1,infile)) != 1) {
    printf("an error in reading field header\n");
    return MDV_FAILURE;
  }
  
  MDV_field_header_from_BE(f_hdr);

  /*
   * check that min/max is set in header. If not, read in 
   * field and do a null conversion, which forces the
   * min and max to be set. Copy the values to the field header.
   */

  if (f_hdr->min_value == 0.0 && f_hdr->max_value == 0.0) {
    MDV_field_handle_t *fhand = MDV_fhand_create_from_parts(f_hdr, NULL);
    if (MDV_fhand_read_vol(fhand, infile)) {
      return MDV_FAILURE;
    }
    MDV_fhand_convert(fhand, MDV_ASIS, MDV_COMPRESSION_ASIS,
		      MDV_SCALING_ROUNDED, 0.0, 0.0);
    f_hdr->min_value = MDV_fhand_get_hdr(fhand)->min_value;
    f_hdr->max_value = MDV_fhand_get_hdr(fhand)->max_value;
    MDV_fhand_delete(fhand);
  }

  return MDV_SUCCESS;
}

/***********************************************************************
 * mf_field_hdr_to_fortran:  Fills in the FORTRAN arrays from the given
 *                           MDV_field_header_t structure.
 */

void mf_field_hdr_to_fortran(MDV_field_header_t *field_hdr,
			     si32 *field_hdr_ints,
			     fl32 *field_hdr_reals,
			     char *field_name_long,
			     char *field_name_short,
			     char *field_units,
			     char *field_transform,
			     char *field_unused_char)
{

  double alt = 0;
  extern struct mdv_file_information *mdvptr;  
  extern void save_latlonalt();

  /*
   * Load the information into the arrays.
   */
  if((mdvptr->lat == 0.0) || (mdvptr->lon == 0.0))
      save_latlonalt(field_hdr->proj_origin_lat,field_hdr->proj_origin_lon,
                     alt);  


  memcpy(field_hdr_ints, &field_hdr->struct_id,
	 sizeof(si32) * MDV_NUM_FIELD_HEADER_SI32);

  memcpy(field_hdr_reals, &field_hdr->proj_origin_lat,
	 sizeof(fl32) * MDV_NUM_FIELD_HEADER_FL32);

  memcpy(field_name_long, field_hdr->field_name_long, MDV_LONG_FIELD_LEN);
  
  memcpy(field_name_short, field_hdr->field_name, MDV_SHORT_FIELD_LEN);
  
  memcpy(field_units, field_hdr->units, MDV_UNITS_LEN);
  
  memcpy(field_transform, field_hdr->transform, MDV_TRANSFORM_LEN);
  
  memcpy(field_unused_char, field_hdr->unused_char, MDV_UNITS_LEN);
  
  return;
}

/***********************************************************************
* MF_pf_print_field_hdr: This routine will print an MDV field header
* from FORTRAN format data.
*
* Use the following to call the subroutine:
*
*    INTEGER*4     FIELD_HDR_INTS(MDV_NUM_FIELD_HEADER_SI32)
*    REAL*4        FIELD_HDR_REALS(MDV_NUM_FIELD_HEADER_FL32)
*    CHARACTER*(MDV_LONG_FIELD_LEN) FIELD_NAME_LONG
*    CHARACTER*(MDV_SHORT_FIELD_LEN) FIELD_NAME_SHORT
*    CHARACTER*(MDV_UNITS_LEN) FIELD_UNITS
*    CHARACTER*(MDV_TRANSFORM_LEN) FIELD_TRANSFORM
*    CHARACTER*(MDV_UNITS_LEN) FIELD_UNUSED_CHAR
*
*   CALL MF_PF_PRINT_FIELD_HDR(FIELD_HDR_INTS, FIELD_HDR_REALS,
*                              FIELD_NAME_LONG, FIELD_NAME_SHORT,
*                              FIELD_UNITS, FIELD_TRANSFORM,
*                              FIELD_UNUSED_CHAR)
*
*/

void FORTRAN_NAME(mf_pf_print_field_hdr)(
			    si32 *field_hdr_ints,
			    fl32 *field_hdr_reals,
			    char *field_name_long,
			    char *field_name_short,
			    char *field_units,
			    char *field_transform,
			    char *field_unused_char)
{
  MDV_field_header_t field_hdr;
  void mf_field_hdr_from_fortran();
  
  /*
   * Load the information in the field header
   */

  mf_field_hdr_from_fortran(field_hdr_ints,
			    field_hdr_reals,
			    field_name_long,
			    field_name_short,
			    field_units,
			    field_transform,
			    field_unused_char,
			    &field_hdr);
  
  /*
   * Print the field header
   */

  MDV_print_field_header_full(&field_hdr, stdout);
  
  return;
}

/*****************************************************************
 * MDV_PRINT_FIELD_HEADER_FULL: print out all of the field header
 * info
 * --Nancy Rehak 4/96
 */
 
void MDV_print_field_header_full(MDV_field_header_t *fld_hdr, FILE *outfile)
{
  int i;
  time_t print_time;         /* temporary storage area for printing time */
                             /*   values -- necessary on systems which have */                             /*   time_t defined as other than 32-bit */
  
  
  fprintf(outfile, "\n");
  fprintf(outfile, "           MDV_print_field_header\n");
  fprintf(outfile, "           -----------------------\n");
  fprintf(outfile, "\n");
  fprintf(outfile, "record_len1:            %d\n", fld_hdr->record_len1);
  fprintf(outfile, "struct_id:              %d\n", fld_hdr->struct_id);
  fprintf(outfile, "\n");
  fprintf(outfile, "field_code:             %d\n", fld_hdr->field_code);
  fprintf(outfile, "user_time1:             %d\n", fld_hdr->user_time1);
  fprintf(outfile, "forecast_delta:         %d\n", fld_hdr->forecast_delta);
  fprintf(outfile, "user_time2:             %d\n", fld_hdr->user_time2);
  fprintf(outfile, "user_time3:             %d\n", fld_hdr->user_time3);
  print_time = fld_hdr->forecast_time;
  fprintf(outfile, "forecast_time:          %s",
	  asctime(gmtime(&print_time)));
  fprintf(outfile, "user_time4:             %d\n", fld_hdr->user_time4);
  fprintf(outfile, "nx:                     %d\n", fld_hdr->nx);
  fprintf(outfile, "ny:                     %d\n", fld_hdr->ny);
  fprintf(outfile, "nz:                     %d\n", fld_hdr->nz);
  fprintf(outfile, "proj_type:              %s\n",
	  MDV_proj2string(fld_hdr->proj_type));
  fprintf(outfile, "encoding_type:          %s\n",
	  MDV_encode2string(fld_hdr->encoding_type));
  fprintf(outfile, "data_element_nbytes:    %d\n",
	  fld_hdr->data_element_nbytes);
  fprintf(outfile, "field_data_offset:      %d\n", fld_hdr->field_data_offset);
  fprintf(outfile, "volume_size:            %d\n", fld_hdr->volume_size);
  for (i = 0; i < 10; i++)
    fprintf(outfile, "user_data_si32[%d]:      %d\n",
	    i, fld_hdr->user_data_si32[i]);
  
  fprintf(outfile, "\n");
  fprintf(outfile, "proj_origin_lon:        %f\n", fld_hdr->proj_origin_lon);
  fprintf(outfile, "proj_origin_lat:        %f\n", fld_hdr->proj_origin_lat);
  fprintf(outfile, "proj_rotation:          %f\n", fld_hdr->proj_rotation);
  for (i = 0; i < MDV_MAX_PROJ_PARAMS; i++)
    fprintf(outfile, "proj_param[%02d]:         %f\n",
	    i, fld_hdr->proj_param[i]);
  fprintf(outfile, "vert_reference:         %f\n", fld_hdr->vert_reference);
  fprintf(outfile, "\n");
  fprintf(outfile, "grid_dx:                %f\n", fld_hdr->grid_dx);
  fprintf(outfile, "grid_dy:                %f\n", fld_hdr->grid_dy);
  fprintf(outfile, "grid_dz:                %f\n", fld_hdr->grid_dz);
  fprintf(outfile, "grid_minx:              %f\n", fld_hdr->grid_minx);
  fprintf(outfile, "grid_miny:              %f\n", fld_hdr->grid_miny);
  fprintf(outfile, "grid_minz:              %f\n", fld_hdr->grid_minz);
  fprintf(outfile, "scale:                  %f\n", fld_hdr->scale);
  fprintf(outfile, "bias:                   %f\n", fld_hdr->bias);
  fprintf(outfile, "bad_data_value:         %f\n", fld_hdr->bad_data_value);
  fprintf(outfile, "missing_data_value:     %f\n",
	  fld_hdr->missing_data_value);
  fprintf(outfile, "proj_rotation:          %f\n", fld_hdr->proj_rotation);
  for (i = 0; i < 4; i++)
    fprintf(outfile, "user_data_fl32[%d]:      %f\n",
	    i, fld_hdr->user_data_fl32[i]);
  fprintf(outfile, "\n");
  fprintf(outfile, "field_name_long:        <%s>\n", fld_hdr->field_name_long);
  fprintf(outfile, "field_name:             <%s>\n", fld_hdr->field_name);
  fprintf(outfile, "units:                  <%s>\n", fld_hdr->units);
  fprintf(outfile, "transform:              <%s>\n", fld_hdr->transform);
  fprintf(outfile, "\n");
  fprintf(outfile, "record_len2:            %d\n", fld_hdr->record_len2);
  fprintf(outfile, "\n\n");
  
  return;
}

/***********************************************************************
* MF_RV_READ_VLEVEL_HDR: Read the indicated vlevel header from a data file
* and return the information in FORTRAN-usable structures.
*
* Use the following to call the subroutine:
*
*    INTEGER        FIELD_NUM
*    INTEGER*4      VLEVEL_HDR_INTS(MDV_NUM_VLEVEL_HEADER_SI32)
*    REAL*4         VLEVEL_HDR_REALS(MDV_NUM_VLEVEL_HEADER_FL32)
*    INTEGER        RETURN_STATUS
*
*   CALL MF_RV_READ_VLEVEL_HDR(FIELD_NUM,
*                              VLEVEL_HDR_INTS, VLEVEL_HDR_REALS,
*	                       RETURN_STATUS)
*
* This function will read the indicated vlevel header.
* The header information is returned in the given arrays.
* 
* When finished, RETURN_STATUS can be MDV_READ_SUCCESSFUL,
*                                     MDV_READ_OPEN_FAILURE,
*                                     MDV_READ_BAD_MASTER_HDR,
*                                     MDV_READ_INVALID_FIELD_NUM,
*                                     MDV_READ_BAD_VLEVEL_HDR
*/

void FORTRAN_NAME(mf_rv_read_vlevel_hdr)(
			    int *field_num,
			    si32 *vlevel_hdr_ints,
			    fl32 *vlevel_hdr_reals,
                            int  *return_status)
{
  MDV_vlevel_header_t vlevel_hdr;
  int offset,index;

  void mf_vlevel_hdr_to_fortran();
  extern struct mdv_file_information *mdvptr; 
  
  /*
   * Check to make sure the field number is valid
   */
  offset = mdvptr->offset_vlevels;
 

  if (*field_num >= mdvptr->num_fields)
  {
    fprintf(stderr, "Invalid field number %d given, file only has %d fields\n",
            *field_num, mdvptr->mdv_in);
    fclose(mdvptr->mdv_in);
    *return_status = MDV_READ_INVALID_FIELD_NUM;
    return;
  }
  
  /*
   * Check to make sure vlevel headers are included in this file before 
   * we try to read them.
   */

  if (!mdvptr->vlevels)
  {
    fprintf(stderr,
	    "Trying to read vlevel headers from file not including them\n");
    fclose(mdvptr->mdv_in);
    *return_status = MDV_READ_NO_VLEVEL_HDRS;
    return;
  }
  
  /*
   * Read the vlevel header
   */

  if (MDV_load_vlevel_header(mdvptr->mdv_in, &vlevel_hdr,
			     offset, *field_num) != MDV_SUCCESS)
  {
    fprintf(stderr, "Error loading field %d vlevelheader from file\n",
	    *field_num);
    fclose(mdvptr->mdv_in);
    *return_status = MDV_READ_BAD_VLEVEL_HDR;
    return;
  }
  

  /*
   * Copy the vlevel header info
   */

  mf_vlevel_hdr_to_fortran(&vlevel_hdr,
			   vlevel_hdr_ints,
			   vlevel_hdr_reals);
  
  *return_status = MDV_READ_SUCCESSFUL;

  for(index = 0; index < 122; index++){
      mdvptr->vertorder[*field_num][index] = vlevel_hdr.vlevel_params[index];
  }

  mdvptr->vtype[*field_num] = vlevel_hdr.vlevel_type[0];

   if (fseek(mdvptr->mdv_in,0,SEEK_SET)){ 
          fclose(mdvptr->mdv_in);
          *return_status = MDV_FAILURE; 
          return;
    } 
  return;
}

/***********************************************************************
 * mf_vlevel_hdr_to_fortran:  Fills in the FORTRAN arrays from the given
 *                            MDV_vlevel_header_t structure.
 */

void mf_vlevel_hdr_to_fortran(MDV_vlevel_header_t *vlevel_hdr,
			      si32 *vlevel_hdr_ints,
			      fl32 *vlevel_hdr_reals)
{

  /*
   * Load the information into the arrays.
   */

  memcpy(vlevel_hdr_ints, &vlevel_hdr->struct_id,
	 sizeof(si32) * MDV_NUM_VLEVEL_HEADER_SI32);

  memcpy(vlevel_hdr_reals, vlevel_hdr->vlevel_params,
	 sizeof(fl32) * MDV_NUM_VLEVEL_HEADER_FL32);

  
  return;
}

/***********************************************************************
* MF_pv_print_vlevel_hdr: This routine will print an MDV vlevel header
* from FORTRAN format data.
*
* Use the following to call the subroutine:
*
*    INTEGER*4     VLEVEL_HDR_INTS(MDV_NUM_VLEVEL_HEADER_SI32)
*    REAL*4        VLEVEL_HDR_REALS(MDV_NUM_VLEVEL_HEADER_FL32)
*    INTEGER*4     NUM_Z
*    CHARACTER*(MDV_LONG_FIELD_LEN) FIELD_NAME
*
*   CALL MF_PV_PRINT_VLEVEL_HDR(VLEVEL_HDR_INTS, VLEVEL_HDR_REALS,
*                               NUM_Z, FIELD_NAME)
*
*/

void FORTRAN_NAME(mf_pv_print_vlevel_hdr)(
			     si32 *vlevel_hdr_ints,
			     fl32 *vlevel_hdr_reals,
			     int *num_z,
			     char *field_name)
{
  MDV_vlevel_header_t vlevel_hdr;
  void mf_vlevel_hdr_from_fortran();


  /*
   * Load the information in the vlevel header
   */

  mf_vlevel_hdr_from_fortran(vlevel_hdr_ints,
			     vlevel_hdr_reals,
			     &vlevel_hdr);
  
  /*
   * Print the vlevel header
   */

  MDV_print_vlevel_header_full(&vlevel_hdr, *num_z,
			       field_name, stdout);
  
  return;
}

/***********************************************************************
 * mf_vlevel_hdr_from_fortran:  Fills in the MDV_vlevel_header_t structure
 *                              from the given FORTRAN arrays.
 */

void mf_vlevel_hdr_from_fortran(si32 *vlevel_hdr_ints,
				fl32 *vlevel_hdr_reals,
				MDV_vlevel_header_t *vlevel_hdr)
{
  /*
   * Load the information into the vlevel header.
   */

  memcpy(&vlevel_hdr->struct_id, vlevel_hdr_ints,
	 sizeof(si32) * MDV_NUM_VLEVEL_HEADER_SI32);

  memcpy(vlevel_hdr->vlevel_params, vlevel_hdr_reals,
	 sizeof(fl32) * MDV_NUM_VLEVEL_HEADER_FL32);

  /*
   * Set the record length values
   */

  vlevel_hdr->record_len1 = sizeof(MDV_vlevel_header_t) - 2 * sizeof(si32);
  vlevel_hdr->record_len2 = vlevel_hdr->record_len1;
  
  return;
}

/*****************************************************************
 * MDV_PRINT_VLEVEL_HEADER_FULL: print out all of the vlevel
 * header info
 * --Nancy Rehak 4/96
 */

void MDV_print_vlevel_header_full(MDV_vlevel_header_t *mvh, int nz, 
				  char *field_name,FILE *outfile)
{ 
  int i;
  
  fprintf(outfile, "\n");
  fprintf(outfile, "           Vlevel_header for %s\n",field_name);
  fprintf(outfile, "           -----------------------\n");
  fprintf(outfile, "\n");
  fprintf(outfile, "record_len1:             %d\n", mvh->record_len1);
  fprintf(outfile, "struct_id:               %d\n", mvh->struct_id);
  fprintf(outfile, "\n");
  for (i = 0; i < nz; i++)
  {
    fprintf(outfile, "vlevel_type[%02d]:         %s\n",
	    i, MDV_verttype2string(mvh->vlevel_type[i]));
    
    fprintf(outfile, "vlevel_params[%02d]:       %f\n",
	    i, mvh->vlevel_params[i]);
  }
  fprintf(outfile, "\n");
  fprintf(outfile, "record_len2:             %d\n", mvh->record_len2);
  fprintf(outfile, "\n\n");
  
  return;
}
 
/******************************************************************************
 * MDV_LOAD_VLEVEL_HEADER: Load mdv vlevel header data into the given
 * structure from disk.  Memory for the vlevel header is assumed to be
 * allocated before this routine is called.  The bytes in the header are
 * swapped if necessary to put the data in native format.
 *
 * Inputs: infile - pointer to the input file.  This is assumed to currently
 *                  be open for read.
 *         v_hdr  - pointer to the vlevel header to be loaded.
 *         offset - This information is used to determine the position 
 *                  on disk where the vlevel header information is located.
 *         field_num - the field number.  This information is also used to
 *                     determine the disk location.
 *
 * Output: v_hdr - updated to include the values read in from disk,
 *                 byte swapped as necessary.
 *
 * Returns: MDV_SUCCESS or MDV_FAILURE.
 */
 
int MDV_load_vlevel_header( FILE *infile, MDV_vlevel_header_t *v_hdr, 
                             int offset, int field_num)
{
     long hdr_offset;
 
     hdr_offset = offset + (field_num * sizeof(MDV_vlevel_header_t));
 
     if (fseek(infile,hdr_offset,SEEK_SET)) 
         return MDV_FAILURE;

     if((fread(v_hdr,sizeof(MDV_vlevel_header_t),1,infile)) != 1) {
        return MDV_FAILURE;
     }

     MDV_vlevel_header_from_BE(v_hdr);
 
     return MDV_SUCCESS;
}


/******************************************************************************
 * MDV_LOAD_VLEVEL_HEADER_OFFSET: Load mdv vlevel header data into the given
 * structure from disk given the offset for the first vlevel header in the
 * file rather than the master header.  Memory for the vlevel header is
 * assumed to be allocated before this routine is called.  The bytes in the
 * header are swapped if necessary to put the data in native format.
 *
 * Inputs: infile - pointer to the input file.  This is assumed to currently
 *                  be open for read.
 *         v_hdr - pointer to the vlevel header to be loaded.
 *         vlevel_offset - offset in the file to the first vlevel header.
 *                         This information is used to determine the disk
 *                         location of the desired vlevel header.
 *         field_num - the field number.  This information is also used to
 *                     determine the disk location.
 *
 * Output: v_hdr - updated to include the values read in from disk,
 *                 byte swapped as necessary.
 *
 * Returns: MDV_SUCCESS or MDV_FAILURE.
 */
 
int MDV_load_vlevel_header_offset(FILE *infile,
				  MDV_vlevel_header_t *v_hdr, 
				  int vlevel_offset,
				  int field_num)
{
     long hdr_offset;
     extern void MDV_vlevel_header_from_BE();
 
     hdr_offset = vlevel_offset + 
                  (field_num * sizeof(MDV_vlevel_header_t));
 
     if (fseek(infile, hdr_offset, SEEK_SET)) 
         return MDV_FAILURE;

     if ((fread(v_hdr, sizeof(MDV_vlevel_header_t), 1, infile)) != 1)
     {
        return MDV_FAILURE;
     }

     MDV_vlevel_header_from_BE(v_hdr);
 
     return MDV_SUCCESS;
}
/*************************************************************************/
void pvleveltype(int type)
{

  switch(type){
        case 1:{
          printf("Surface\n");
          break;
        }
        case 3:{
          printf("Pressure Units = mb\n");
          break;
        }
        case 4:{
          printf("Constant altitude Units = Km MSL\n");
          break;
        }
        case 9:{
          printf("Elevation angles\n");
          break;
        }
        case 12:{
          printf("Satellite Image\n");
          break;
        }
        default:{
          printf("Unknown vertical level type\n");
          break;
	}
  }

}/*pvleveltype*/

/*************************************************************************/
void FORTRAN_NAME(put_level_on_name)(
      char fname[8],int index)
{

int length,i,type;
extern struct mdv_file_information *mdvptr;
extern char field_names[NFMAX][8];


 if(mdvptr->src_flg == CEDRIC) return;
 if(mdvptr->grids_differ == 0) return;

 length = 0;
 for(i = 0; i < 8; i++){
   if((fname[i] == ' ') || fname[i] == '\0'){
       length = i;
       break;
   }
 }

 if(length >= 6) return;

 fname[length] = '_';    
 type = mdvptr->vtype[index];
 
 if(type == 12) return;
 switch(type){
        case 1:{
          fname[length+1] = 'S';
          break;
        }
        case 3:{
          fname[length+1] = 'P';
          break;
        }
        case 4:{
          fname[length+1] = 'Z';
          break;
        }
        case 9:{
          fname[length+1] = 'E';
          break;
        }
        default:{
          fname[length+1] = 'U';
          break;
	}
  }

  strcpy(field_names[index],fname);
}/*put_level_on_name*/
  
/*************************************************************************/
/*print field summary*/
void FORTRAN_NAME(pfldsum)(
                  int *ifld, int *iscale,char fname[8])
{
  int  index,nlevels;
  char thename[9];  
  extern struct mdv_file_information *mdvptr;

  index = *ifld - 1;
  if(index == 0){
     printf("Field vertical level information summary as it is on the data set\n");
     printf("Master header: Native vertical level type =  ");
     pvleveltype(mdvptr->nativev);
     printf("Field #   Field name    # levels    Level #1     Level #n    Spacing    Level type\n");
  }

  strncpy(thename,fname,7);
  thename[8] = '\0';
  *iscale = 10;
  /*put_level_on_name(fname,index);*/
  printf(" %2d        %s        %3d",*ifld,thename,mdvptr->nvertlevels[index]);
  if(mdvptr->nvertlevels[index] == 1){
     printf("    %8.2f     %8.2f    %8.2f     ",mdvptr->vertorder[index][0],mdvptr->vertorder[index][0],
                                                mdvptr->vertspacing[index]);
  }
  else{
     nlevels = mdvptr->nvertlevels[index];
     printf("    %8.2f     %8.2f    %8.2f     ",mdvptr->vertorder[index][0],
                                                   mdvptr->vertorder[index][nlevels-1],
                                                   mdvptr->vertspacing[index]);
  }
  pvleveltype(mdvptr->vtype[index]);

  *iscale = mdvptr->iscale[index];
  if(mdvptr->iscale[index] == -1){
     printf("NOTE: FIELD # %d WILL BE DIVIDED BY 1000.  IT HAS VALUES THAT ARE TOO LARGE TO FIT IN THE CEDRIC EDIT FILE\n",index+1);
     *iscale = 1;
  }

  if(mdvptr->iscale[index] == -2){
     printf("NOTE: FIELD # %d WILL BE DIVIDED BY 1000.  IT HAS NEGATIVE VALUES THAT ARE LESS THAN -2^16\n",index+1);
     *iscale = 1;
  }  
}/*pfldsum*/
/************************************************************************/
/*
 *This subroutine is called from the file CEDMDV.f.  The cdata array
 * has been read in earlier by rdfieldh.  The cdata array contains
 * all of the data for each field.
*/
  

void FORTRAN_NAME(gmdvdata)(
                   float *rbuf2,int *field_num,int *npoints,
                   int *nlev, int *mdvflg,int *srcflg, float oldgrid[6],
                   int *reverse)
{
  extern struct mdv_file_information *mdvptr;
  int istart,start,end,index,j,format,arg1,ssize,stemp;
  int ibias,rdpoints,xcount,nlevels,total;
  int xoffset,yoffset,oxmin,oymin,reqlevel;
  double themin,themax;
  static int oldstart[NFMAX];
  static int count = 0;
  double dscale,dbias,dmissing,dbad;
  double reflat,reflon,alt;
  float rnum,value,temp;
  char cname[8];
  extern unsigned char *cdata[NFMAX];
  void pmdvstats();

  /*------------------------------------------------------------------*/

  reqlevel = *nlev;
  if(reqlevel > mdvptr->nvertlevels[*field_num]) reqlevel = 1;

  *srcflg = mdvptr->src_flg;
  rdpoints = *npoints;
  if(rdpoints > MAXPLN){
    if(*mdvflg == 0){
       printf("CEDRIC IS UNABLE TO HANDLE YOUR GRID SIZE %d",rdpoints);
       printf("PLEASE LIMIT THE NUMBER OF POINTS PER PLANE TO %d\n",MAXPLN);
       exit(0);
    }
  }

  if((mdvptr->numx * mdvptr->numy) > MDVMAXGRID){
    if(*mdvflg == 1){
       printf("CEDRIC IS UNABLE TO HANDLE YOUR GRID SIZE %d",mdvptr->numx * mdvptr->numy);
       printf("PLEASE LIMIT THE NUMBER OF POINTS PER PLANE TO %d BEFORE INTREPOLATION\n",MDVMAXGRID);
       exit(0);
    }
  }

      
  if(*mdvflg == 1){
     /*
      *Load in information from the MDV headers to be used in the 
      *interpolation.
      */
      oldgrid[0] = mdvptr->old_grid[0];
      oldgrid[1] = mdvptr->old_grid[1];
      oldgrid[2] = mdvptr->old_grid[2];
      oldgrid[3] = mdvptr->old_grid[3];
      oldgrid[4] = mdvptr->old_grid[4];
      oldgrid[5] = mdvptr->old_grid[5];
      rdpoints = mdvptr->numx * mdvptr->numy;
  }

  /*
   *Some of the mdv files (pressure ones) have the vertical level surface numbers 
   *in decreasing order. For example the pressure files can have the 
   *vertical level surfaces going from 1000 - 100.  Cedric does not like since
   *it wants the numerical values of the vertical surfaces to go in increasing order.
   *The following if statements take care of this.
   */
  nlevels = mdvptr->nvertlevels[*field_num];
  if(mdvptr->vertorder[*field_num][0] > mdvptr->vertorder[*field_num][nlevels-1]){
     rdpoints = (mdvptr->numx * mdvptr->numy )*nlevels ;
     *reverse = 1;
     if(reqlevel == 1){
         start = rdpoints - (mdvptr->numx * mdvptr->numy);
         end = rdpoints - 1;
         oldstart[*field_num] = start;
      }      
     else{
         start = oldstart[*field_num] - (mdvptr->numx * mdvptr->numy) ;
         end = oldstart[*field_num] - 1;
         oldstart[*field_num] = start;
     }
  }
  else{
      rdpoints = mdvptr->numx * mdvptr->numy;
      *reverse = 0;
      if(reqlevel == 1){
         start = 0;
         end = rdpoints - 1;
      }      
      else{
         start = 0;
         for(index = 0; index  < *nlev-1; index++){
               start = start + rdpoints;
         }
         end = start + rdpoints;
     }
  }


  if((start < 0) || (end <= 0)){
      printf("Invalid loop indicies  in gmdvdata start %d  to  end  %d\n",start,end);
      exit(0);
  }


  j = 0;
  dscale = mdvptr->mdvscale[*field_num];
  dbias  = mdvptr->mdvbias[*field_num];
  dbad   = mdvptr->mdvbad[*field_num];
  dmissing =  mdvptr->mdvmiss[*field_num];


  themin = 1000000.;
  themax = -1000000.;
  for(index = start; index <= end; index++){
       temp = (float)cdata[*field_num][index];
       if(temp == dbad)  value = -32768.0;
       if(temp == dmissing) value = -32768.0;
       else{
            value  = (temp * dscale) + dbias;
            if(value < themin){
               mdvptr->minv[*field_num][reqlevel-1] = value;
               themin = value;
            }
            if(value > themax){
               mdvptr->maxv[*field_num][reqlevel-1] = value;
               themax = value;
            }     
       }       
       if(mdvptr->iscale[*field_num] < 0) value = value * .001;
       rbuf2[j] = value;
       j = j + 1;
  }/*for loop for data*/

  /*if(*field_num+1 == mdvptr->num_fields) pmdvstats();*/

}/*get_mdv_data*/
/****************************************************************************/

/*
 *Print mdv field information.
 */
void FORTRAN_NAME(pmdvfi)(
               int *ifield,short *id1,short *id2, short *id3, short *id4, short *id5)
{
  int index,format;
  int num1,num2,num3,num4,num5,num6,num7,num8;
  int ibias;
  double dscale,dbias,dmissing,dbad;
  char cname[8];
  extern int get_Cfield_info();
  extern struct mdv_file_information *mdvptr;

  num1 = *id1/256;
  num2 = *id1%256;
  num3 = *id2/256;
  num4 = *id2%256;
  num5 = *id3/256;
  num6 = *id3%256;
  num7 = *id4/256;
  num8 = *id4%256;

  if(mdvptr->swap == 0){ 
     cname[0] = (char)num1;
     cname[1] = (char)num2;
     cname[2] = (char)num3;
     cname[3] = (char)num4;
     cname[4] = (char)num5;
     cname[5] = (char)num6;
     cname[6] = (char)num7;
     cname[7] = (char)num8;
   }
   else{
     cname[0] = (char)num2;
     cname[1] = (char)num1;
     cname[2] = (char)num4;
     cname[3] = (char)num3;
     cname[4] = (char)num6;
     cname[5] = (char)num5;
     cname[6] = (char)num8;
     cname[7] = (char)num7;
   }    


  if( get_Cfield_info(&format,&dscale,&dbias,&dmissing,&dbad,cname) == -1){
      dscale = (double)*id5;
      dbias  = 0.0;
   }
  

   ibias = (int)dbias;
   if(*ifield < 10){
       printf("     %d    %c%c%c%c%c%c%c",*ifield,cname[0],cname[1],
              cname[2],cname[3],cname[4],cname[5],cname[6],cname[7]);
   }
   else {
       printf("    %d     %c%c%c%c%c%c%c",*ifield,cname[0],cname[1],
              cname[2],cname[3],cname[4],cname[5],cname[6],cname[7]);
   }
   printf("      %2.7f        %d\n",dscale,ibias);

}/*pmdvfi*/ 


  /**************************************************************************/
  /***************MDV ROUTINES USED FOR WRITING OUT MDV FORMAT**************/
  /**************************************************************************/
void FORTRAN_NAME(wmdvmhdr)(
                    int *lout,int numxyz[3], int times[6],
                    int dates[6],float position[3],
                    float valnyq[61],int *nfields,char radar_name[6])
{
  MDV_master_header_t master_hdr;
  MDV_field_header_t field_hdr;
  char fname[8];
  char time_string[24],tempc[5];
  char source[128];
  int unit,index,temp,tarray[9],num,rw;
  FILE * mdv_file_open();
  FILE *in_file,*out_file;
  int MDV_write_master_header();
  int MDV_write_field_header(); 
  int numval,numpoints;
  int master_offset,offset,data_offsets[NFMAX];
  int coord_sys;
  int diffnyq,numnyq[61];
  float presentnyq;
  double lat,lon,alt;
  time_t calendar_time;
  struct tm t,*local;
  time_t clock;

  extern struct mdv_file_information *mdvptr;
  extern void get_latlonalt();

  /*----------------------------------------------------------------------------
                      LOAD AND WRITE OUT THE MASTER HEADER.                    
   ------------------------------------------------------------------------------*/

      init_MDV_struct();
      master_hdr.record_len1 = 1016;
      master_hdr.record_len2 = 1016;
      master_hdr.struct_id = 14142;
      master_hdr.revision_number = 1;
     /*
      *Calculate the user time in seconds since 1970.
      */
      time(&clock);
      strftime(time_string,15,"%Y%m%d%H%M%S",localtime(&clock));
      tempc[0]  = time_string[0];
      tempc[1]  = time_string[1];
      tempc[2]  = time_string[2];
      tempc[3]  = time_string[3];
      tempc[4]  = '\0';
      tarray[2] = atoi(tempc);   /*this gives us all four digits of the year*/ 

      tempc[0]  = time_string[4];
      tempc[1]  = time_string[5];
      tempc[2]  = ' ';
      tempc[3]  = ' ';
      tempc[4]  = '\0';  
      tarray[1] = atoi(tempc);

      tempc[0]  = time_string[6];
      tempc[1]  = time_string[7];
      tempc[2]  = ' ';
      tempc[3]  = ' ';
      tempc[4]  = '\0';  
      tarray[0] = atoi(tempc);

      tempc[0]  = time_string[8];
      tempc[1]  = time_string[9];
      tempc[2]  = ' ';
      tempc[3]  = ' ';
      tempc[4]  = '\0';  
      tarray[3] = atoi(tempc);  

      tempc[0]  = time_string[10];
      tempc[1]  = time_string[11];
      tempc[2]  = ' ';
      tempc[3]  = ' ';
      tempc[4]  = '\0';  
      tarray[4] = atoi(tempc);

      tempc[0]  = time_string[12];
      tempc[1]  = time_string[13];
      tempc[2]  = ' ';
      tempc[3]  = ' ';
      tempc[4]  = '\0';  
      tarray[5] = atoi(tempc);
      calendar_time = date_to_seconds(tarray); 
      master_hdr.time_gen = calendar_time;
      master_hdr.user_time = 0;

     /*
      *Beginning time.
      */
      tarray[2] = dates[0]/10;
      if(tarray[2] > 5 && tarray[2] <= 9) 
         temp = 1900; 
      else
         temp = 2000;
      tarray[2] =  dates[0] + temp;     
      tarray[1] = dates[1];
      tarray[0] = dates[2];
      tarray[3] = times[0];
      tarray[4] = times[1];
      tarray[5] = times[2];
      calendar_time = date_to_seconds(tarray);
      master_hdr.time_begin =  calendar_time;

      /*
       *Ending Time.
       */
      tarray[2] = dates[3]/10;
      if(tarray[2] > 5 && tarray[2] <= 9) 
         temp = 1900; 
      else
         temp = 2000;
      tarray[2]  = dates[3] + temp;
      tarray[1]  = dates[4];
      tarray[0]  = dates[5];
      tarray[3]  = times[3];
      tarray[4]  = times[4];
      tarray[5]  = times[5];
      calendar_time = date_to_seconds(tarray);
      master_hdr.time_expire = calendar_time;
      master_hdr.time_end = calendar_time;

      master_hdr.time_centroid = (master_hdr.time_end + master_hdr.time_begin)/2 ;
      mdvptr->time_forecast = master_hdr.time_centroid;
      master_hdr.num_data_times = 0;
      master_hdr.index_number = 0;
      master_hdr.data_dimension = 3; /*VOLUME*/
      master_hdr.data_collection_type = MDV_DATA_MEASURED;
      master_hdr.user_data = 0;



      memset(master_hdr.data_set_source,' ',128);
      
    /*
     *VLEVEL type.
     */
      output_coord_system(&coord_sys);
      if(coord_sys == 0){
            master_hdr.vlevel_type = MDV_VERT_TYPE_Z;
            strcpy(master_hdr.data_set_name,"CEDRIC GENERATED MDV : COORD SYS = CRT");
            mdvptr->projection = MDV_PROJ_FLAT;
            mdvptr->nativev = MDV_VERT_TYPE_Z;
      }
      else if(coord_sys == 1){
            master_hdr.vlevel_type = MDV_VERT_TYPE_MIXED;
            strcpy(master_hdr.data_set_name,"CEDRIC GENERATED MDV : COORD SYS = COPLANE");
            mdvptr->nativev = MDV_VERT_TYPE_MIXED;
      }        
      else if(coord_sys == 2){
         master_hdr.vlevel_type = MDV_VERT_TYPE_ELEV;
         mdvptr->nativev = MDV_VERT_TYPE_ELEV;
         strcpy(master_hdr.data_set_name,"CEDRIC GENERATED MDV : COORD SYS = ELEVATION");
         mdvptr->projection = MDV_PROJ_FLAT;
         
      }
      else if(coord_sys == 3){
            strcpy(master_hdr.data_set_name,"CEDRIC GENERATED MDV : COORD SYS = LATLONELEV");
            master_hdr.vlevel_type = MDV_VERT_TYPE_ELEV; 
            mdvptr->projection = MDV_PROJ_LATLON;
            mdvptr->nativev = MDV_VERT_TYPE_ELEV; 
      }
      else if(coord_sys == 4){
            strcpy(master_hdr.data_set_name,"CEDRIC GENERATED MDV : COORD SYS = LONLATZ"); 
            master_hdr.vlevel_type = MDV_VERT_TYPE_Z;
            mdvptr->projection = MDV_PROJ_LATLON;
            mdvptr->nativev = MDV_VERT_TYPE_Z;
      }
      else if(coord_sys == 7){
            strcpy(master_hdr.data_set_name,"CEDRIC GENERATED MDV : COORD SYS = LONLATP"); 
            master_hdr.data_set_name[42] = '\0';
            master_hdr.vlevel_type = MDV_VERT_TYPE_PRESSURE;
            mdvptr->projection = MDV_PROJ_LATLON;
            mdvptr->nativev = MDV_VERT_TYPE_PRESSURE;
      }      
      else{
         printf("UNKNOWN MDV VERTICAL TYPE\n");
         exit(0);
      }
      for(index = 0; index < 8; index++) master_hdr.user_data_si32[index] = -999;
      for(index = 0; index < 12; index++) master_hdr.user_data_fl32[index] = -999.;

      diffnyq = 1;
      master_hdr.user_data_fl32[0] = valnyq[0];
      for(index = 0; index < 61; index++){
          numnyq[index] = 0;
      }
      for(index = 0; index < 61; index++){
          if(index == 0){
             presentnyq = valnyq[index];
             numnyq[0] = 1;
	  }
          else{
	    if(presentnyq != valnyq[index]){
                master_hdr.user_data_fl32[diffnyq] = valnyq[index];
                diffnyq++;
                presentnyq = valnyq[index]; 
                numnyq[diffnyq-1] = 1;
	     }
	    else{
                numnyq[diffnyq-1] = numnyq[diffnyq-1] + 1;
               }
	  }
      }/*for index loop*/
      for(index = 0; index < diffnyq; index++)  master_hdr.user_data_si32[index] = numnyq[index];

      if(radar_name[0] != 'U'){
         strcpy(master_hdr.data_set_info,"RADAR NAME: ");
         master_hdr.data_set_info[12] = radar_name[0];
         master_hdr.data_set_info[13] = radar_name[1];
         master_hdr.data_set_info[14] = radar_name[2];
         master_hdr.data_set_info[15] = radar_name[3];
         master_hdr.data_set_info[16] = '\0';
      }
      else
         master_hdr.data_set_info[0] = '\0';


      /*
       *if the original file was a mdv file then the nativev field of the 
       *mdv information structure will be set to what it was in the original
       *file. If the original file was not a mdv file then the nativev field
       *of the mdv information structure is -999.
       */
      if(mdvptr->nativev != -999.)  master_hdr.native_vlevel_type = master_hdr.vlevel_type; 
      else{
           master_hdr.native_vlevel_type = mdvptr->nativev;
           mdvptr->nativev = master_hdr.vlevel_type;
      }

        
      master_hdr.vlevel_included = 0;
      master_hdr.grid_order_direction = MDV_ORIENT_SN_WE;
      master_hdr.grid_order_indices = MDV_ORDER_XYZ;
      master_hdr.n_fields = *nfields;
      mdvptr->num_fields = *nfields;
      master_hdr.n_chunks = 0;
      master_hdr.field_hdr_offset = 1024; /*1016 + 8 * four bytes for record_len*/
      master_hdr.vlevel_hdr_offset = 0;  
      master_hdr.chunk_hdr_offset = 0;
      master_hdr.field_grids_differ = 0;
      master_hdr.max_nx = numxyz[0];
      master_hdr.max_ny = numxyz[1];
      master_hdr.max_nz = numxyz[2];
      mdvptr->numx = numxyz[0];
      mdvptr->numy = numxyz[1];
      mdvptr->vlevels = numxyz[2];
      master_hdr.sensor_lon = position[1];
      master_hdr.sensor_lat = position[0];
      master_hdr.sensor_alt = position[2];
      mdvptr->lat = (double)master_hdr.sensor_lat;
      mdvptr->lon = (double)master_hdr.sensor_lon;
      unit = *lout;
      rw = 1;
      out_file = mdv_file_open(unit,rw);
      mdvptr->mdv_out = out_file;

 
       if(MDV_write_master_header(out_file,&master_hdr) == MDV_FAILURE) {
          printf("UNABLE TO WRITE MASTER HEADER TO MDV OUPUT FILE\n");
          exit(-1);
       }
       printf("mdv master header written to output file\n");

}/*wmdvmhdr*/

/**************************************************************/
/*
 *prepmdvdata is called from WRITVL.f.  It mallocs space in the global
 *array cdata and scales and bias the data for writing out.
 */ 
void prepmdvdata_(int *fieldnum,int *level,int *iprocess,float *rbuf)
{
   int    num_data_points,nfields,thefield,index,nplane;
   static int dataindex;
   static float themin,themax;
   double dscale,dbias,drange;
   float  ftemp;
   extern unsigned char *cdata[NFMAX];
   extern struct mdv_file_information *mdvptr;

   thefield = *fieldnum - 1;
   num_data_points = mdvptr->numx * mdvptr->numy * mdvptr->vlevels;

   if((thefield  == 0) && (*level == 1)){
       fdata = (float *)malloc(num_data_points * sizeof(float));
        if(!fdata){
           printf("unable to malloc space for writing out mdv data\n");
           exit(-1);
        } 
       memset(fdata,-32768.,num_data_points);
   }
   
   if(*level == 1){
        dataindex = 0;
        themin = 100000000.;
        themax = -100000000.;
        cdata[thefield] = (unsigned char *)malloc(num_data_points * sizeof(unsigned char));
        if(!cdata){
           printf("unable to malloc space for writing out mdv data\n");
           exit(-1);
        }
    }
   
   nplane = mdvptr->numx * mdvptr->numy;
   for(index = 0; index < nplane; index++){
       fdata[dataindex] = rbuf[index];
       if(fdata[dataindex] != -32768.){
          if(fdata[dataindex] < themin) themin = fdata[dataindex];
          if(fdata[dataindex] > themax) themax = fdata[dataindex];
       }
       dataindex++;
   }


   /*
    *The following calculation for scale and bias are used for converting the data to 
    *8 byte integers.
    */
   if(*iprocess == 1){
      /*
       *Calculate the scale and the bias.
       */
       if(themin == themax){
          dscale = 1.0;
          dbias = themin - 5.0;   
       }
       else{
          drange = themax - themin;
          dscale = drange / 250.0; 
          dbias = themin - dscale * 4.0;  
       }      
       
       mdvptr->mdvscale[thefield] = dscale;
       mdvptr->mdvbad[thefield]   = 0;
       mdvptr->mdvmiss[thefield]  = 0;
       mdvptr->minv[thefield][0]  = themin;
       mdvptr->maxv[thefield][0]  = themax;
       mdvptr->mdvbias[thefield]   = dbias;

       for(index = 0; index <  num_data_points; index++){
           if(fdata[index] == -32768.) cdata[thefield][index] = 0;
           else{
               ftemp = fdata[index] - dbias;
               ftemp = (ftemp/dscale + 0.49999);
               if(ftemp > 254)  cdata[thefield][index] = 254;
               else cdata[thefield][index] = (unsigned char)ftemp;
	   }
       }
       memset(fdata,-32768.,num_data_points);
   }

}/*prepmdvdata*/
/******************************************************************************
 * WRITE_DATA_INT8: Write the given data volume to disk in MDV_INT8
 *                  format.  This routine performs any data swapping
 *                  necessary for the write.
 */

static int write_data_int8(FILE *outfile,
			   si32 volume_size,
			   ui08 *volume_data)
{
  int index;
  static char *routine_name = "write_data_int8";
  
  si32 output_volume_size = BE_from_si32(volume_size);
  
  /*
   * Write the FORTRAN record to disk.
   */

  if (fwrite(&output_volume_size, sizeof(output_volume_size),
	      1, outfile) != 1) 
  {
    fprintf(stderr,
	    "%s: Error writing beginning FORTRAN record to outfile\n",
	    routine_name);
    return (MDV_FAILURE);
  }

  /*
   * Write out the data.  Note that the data doesn't need to be swapped
   * since it is byte data.
   */
  if (fwrite(volume_data, volume_size, 1, outfile) != 1) 
  {
    fprintf(stderr,
	    "%s: Error writing data to outfile\n",
	    routine_name);
    return (MDV_FAILURE);
  }

  /*
   * Write the other FORTRAN record to disk.
   */

  if (fwrite(&output_volume_size, sizeof(output_volume_size),
		 1, outfile) != 1) 
  {
    fprintf(stderr,
	    "%s: Error writing final FORTRAN record to file\n",
	    routine_name);
    return (MDV_FAILURE);
  }

  return(MDV_SUCCESS);
} /* end write_data_int8 */


/********************************WRITE DATA******************************/
/*
 *cedric write mdv.
 */
void FORTRAN_NAME(cedwmdv)(
		int *num_fields,int fldhints[MDVFSI32],float fldhreal[MDVFFL32],
		char fldnm[NFMAX][8])

{
 MDV_field_header_t fld_array[NFMAX];
 MDV_field_header_t field_hdr;
 extern struct mdv_file_information *mdvptr;
 int dindex,index,numx,numy,numz,numval;
 int master_offset,offset,data_offsets[NFMAX];
 int output_encoding_type,bytes_written;
 int numpoints,nfields;
 float levelvalue;
 double test;
 int MDV_write_field_header(); 
 int returnval,temp,fhdrlength; 
 extern unsigned char *cdata[NFMAX];
 extern float *fdata;
 extern int MDV_write_field_header();
 extern int MDV_write_field_data();
 extern struct mdv_file_information *mdvptr;
 extern int write_data_int8();
 void     mdvfree_();


 free(fdata); 
 /*---------------------------------------------------------------------------*/
 /*                  Now write out the field headers.                         */
 /*---------------------------------------------------------------------------*/
  /*
   *Offset to first data field.  This includes all field headers and record lengths.
   */
  master_offset = 1024; /*1016 + 8*/
  fhdrlength = 416; /*408 field header bytes + 8 bytes for fortran blocking*/
  data_offsets[0] = master_offset + (fhdrlength * *num_fields) + 4;

  numx = fldhints[MDVFNXIX-1];
  numy = fldhints[MDVFNYIX-1];
  numz = fldhints[MDVFNZIX-1];
  numval = numx * numy * numz * sizeof(unsigned char);
  numpoints = numx * numy * numz;

  if(*num_fields > 1){
     for(index = 1; index < *num_fields; index++){
         data_offsets[index] =  data_offsets[index-1] + numval + 8;
     }
  }

  for(index = 0; index < 10; index++) field_hdr.user_data_si32[index] = 0;
  for(index = 0; index < MDV_MAX_PROJ_PARAMS; index++) field_hdr.proj_param[index] = 0.0;
  for(index = 0; index < 4; index++) field_hdr.user_data_fl32[index] = 0;
  field_hdr.record_len1 = 416;
  field_hdr.record_len2 = 416;
  field_hdr.struct_id = 14143;
  field_hdr.nx        = numx;
  field_hdr.ny        = numy;
  field_hdr.nz        = numz;
  field_hdr.grid_minx = fldhreal[MDVFXMIN-1];
  field_hdr.grid_miny = fldhreal[MDVFYMIN-1];
  field_hdr.grid_minz = fldhreal[MDVFZMIN-1]; 
  field_hdr.grid_dx   = fldhreal[MDVFGDX-1];
  field_hdr.grid_dy   = fldhreal[MDVFGDY-1];
  field_hdr.grid_dz   = fldhreal[MDVFGDZ-1];
  field_hdr.bad_data_value = 0.0;
  field_hdr.missing_data_value = 0.0;  
  field_hdr.encoding_type = MDV_INT8;  
  field_hdr.data_element_nbytes = 1;
  field_hdr.volume_size = numval;
  field_hdr.field_code = 0;
  field_hdr.user_time1 = 0;
  field_hdr.forecast_delta = 0;
  field_hdr.user_time2 = 0;
  field_hdr.user_time3 = 0; 
  field_hdr.forecast_time = mdvptr->time_forecast;
  field_hdr.user_time4 = 0; 
  field_hdr.scaling_type = MDV_SCALING_DYNAMIC;
  field_hdr.transform_type = MDV_TRANSFORM_NONE;
  field_hdr.vert_reference = 0.0;
  field_hdr.proj_rotation = 0.0;  
  field_hdr.proj_type = mdvptr->projection;
  field_hdr.proj_origin_lat = mdvptr->lat;
  field_hdr.proj_origin_lon = mdvptr->lon;
  memset(field_hdr.field_name_long,' ',MDV_LONG_FIELD_LEN);
  memset(field_hdr.units,' ',MDV_UNITS_LEN);
  memset(field_hdr.transform,' ',MDV_TRANSFORM_LEN);


  for(index = 0; index < *num_fields; index++){
      field_hdr.scale = (fl32)mdvptr->mdvscale[index];
      field_hdr.bias  = (fl32)mdvptr->mdvbias[index];
      field_hdr.min_value = mdvptr->minv[index][0];
      field_hdr.max_value = mdvptr->maxv[index][0]; 
      strncpy(field_hdr.field_name,fldnm[index],8);
      field_hdr.field_name[8] = '\0';
      strncpy(field_hdr.field_name_long,fldnm[index],8);
      field_hdr.field_name_long[8] = '\0';
      field_hdr.field_data_offset = data_offsets[index];
      returnval = MDV_write_field_header(mdvptr->mdv_out, &field_hdr,index);
      if(returnval != MDV_SUCCESS)
      {
          fprintf(stderr,
	          "%s: Error writing field header %d.\n","cwrite", index);
          for(index = 0; index < *num_fields; index++){
              free(cdata[index]);
          } 
          fclose(mdvptr->mdv_out);
          exit(0);
      }  
      else{
          printf("field header %d written out successfuly\n",index);
      }  
  }

  for(index = 0; index < *num_fields; index++){
      returnval = write_data_int8(mdvptr->mdv_out,numval,cdata[index]);
      if(returnval != MDV_SUCCESS){
         printf("Unable to write data to output file\n");
         nfields = *num_fields;
         mdvfree_(&nfields);
         exit(0);
      }
      free(cdata[index]);
  }
  fclose(mdvptr->mdv_out);
}/*cedwmdv*/

/******************************************************************************
 * MDV_SET_MASTER_HDR_OFFSETS: Set all of the offset values the master
 * header.  The offset values are set based on the values of the other
 * fields in the header, so these must be set before this routine is 
 * called.
 *
 * Inputs: m_hdr - pointer to master header to be updated.
 *
 * Outputs: m_hdr - all offset values are set appropriately.
 */
 
void MDV_set_master_hdr_offsets(MDV_master_header_t *m_hdr)
{
  m_hdr->field_hdr_offset = sizeof(MDV_master_header_t);

  m_hdr->vlevel_hdr_offset = m_hdr->field_hdr_offset +
    (m_hdr->n_fields * sizeof(MDV_field_header_t));
  
  if (m_hdr->vlevel_included)
    m_hdr->chunk_hdr_offset = m_hdr->vlevel_hdr_offset +
      (m_hdr->n_fields * sizeof(MDV_vlevel_header_t));
  else
    m_hdr->chunk_hdr_offset = m_hdr->vlevel_hdr_offset;
  
  return;
}


 /****************************************************************************/


FILE * mdv_file_open(int unit,int rw)
{
  int i1,i2,i3,itemp;
  char fname[9];
  FILE *fptr;


  /* construct filename */
  fname[0] = 'f';
  fname[1] = 'o';
  fname[2] = 'r';
  fname[3] = 't';
  fname[4] = '.';


  if(unit <= 0){
     printf("Invalid fortran unit number\n");
     exit(0);
  }
  if(unit > 999){
     printf("Cedric can not handle fortran unit numbers > 999\n");
     exit(0);
  }
  if(unit < 100){
     i1 = unit/10;    /* grab ten's digit */
     i2 = unit % 10;  /* grab one's digit */
     fname[5] = (char)i1 + 48;  /* convert to ascii */
     fname[6] = (char)i2 + 48;  /* convert to ascii */
     fname[7] = '\0'; 
  }
  else if(unit == 100){
     fname[5] = '1';
     fname[6] = '0';
     fname[7] = '0';
     fname[8] = '\0';
  }
  else if(unit > 100){
     i1 = unit/100;
     fname[5] = i1 + 48;
     itemp = unit%100;
     if(itemp < 10){
        fname[6] = '0';
        fname[7] = (char)itemp + 48;
        fname[8] = '\0';
     }
     else{
        i2 = itemp/10;
        i3 = itemp%10;
        fname[6] = (char)i2 + 48;
        fname[7] = (char)i3 + 48;
        fname[8] = '\0';    
     }
  }/*if unit > 100*/  



  /*
   * Open the file
   */

  if(rw == 0){
    if ((fptr = fopen(fname,"r")) == NULL)
    {
       fprintf(stderr, "Error opening input file for reading\n");
       perror(fname);
       exit(-1);
    }
      printf("Input file: fortran unit fort.%d\n",unit);
  }
  else{
    if ((fptr = fopen(fname,"w")) == NULL)
    {
       fprintf(stderr, "Error opening input file for writing\n");
       perror(fname);
       exit(-1);
    }
    printf("Output file: fortran unit fort.%d is open\n",unit);
  }
   return(fptr);

}/*mdv_file_open*/

/****************************************************************************/
void FORTRAN_NAME(mdvclose)(char type[3])
{

  extern struct mdv_file_information *mdvptr;

  if(type[0] == 'i'){
     fclose(mdvptr->mdv_in);
  }         
  else{
     fclose(mdvptr->mdv_out);
  }
}/*mdvclose*/

/****************************************************************************/
void FORTRAN_NAME(mdvfree)(int *nfields)
{
extern unsigned char *cdata[NFMAX];
int index;

  printf("freeing the cdata array\n");
  for(index = 0; index < *nfields; index++) free(cdata[index]);

}

/****************************************************************************/
void gmdvnx_(int *orignx, int *origny)
{

extern struct mdv_file_information *mdvptr;

  *orignx = mdvptr->numx;
  *origny = mdvptr->numy;
}/*mdvnx*/


/****************************************************************************/
void pmdvstats()
{
   int    index,level,reverse,nlevels,thelevel,inc;
   extern char field_names[NFMAX][8];
   extern struct mdv_file_information *mdvptr;

   reverse = 0;
   printf("Stats after being read in from the tape before being scaled and put in the Cedric edit file\n");
   for(index = 0; index < mdvptr->num_fields; index++){
       nlevels = mdvptr->nvertlevels[index];
       if(mdvptr->vertorder[index][0] > mdvptr->vertorder[index ][nlevels-1]) reverse = 1;

       if(reverse == 1){
          thelevel =  nlevels - 1;
          inc = -1;
       }
       else{
          thelevel = 0;
          inc = 1;
       }
       printf("Cedric field name %s\n",field_names[index]);
       printf("  Level       Min           Max\n");

       for(level = 0; level < nlevels; level++){
            printf(" %6.1f    %8.2f     %8.2f\n",mdvptr->vertorder[index][thelevel],
                                                 mdvptr->minv[index][level],
                                                 mdvptr->maxv[index][level]);
            thelevel = thelevel + inc;
        }
   }/*for index loop*/
           
       
   
}/*pmdvstats*/












