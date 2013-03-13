#include <stdio.h>
#include <stdlib.h>
#include <netcdf.h>
#include <hdf5.h>
#include <hdf5_hl.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <assert.h>
#include <fcntl.h>

#include "cedric.h"
#include "ced_cdf.h"

static struct variables *var;
#define RANK    1
#define MAXVARS 50

/*************************************************************************
       ROUTINES FOR READING IN THE NETCDF VARIABLES AND DIMENSIONS
************************************************************************/
/* Routine copencdf opens the Netcdf file and reads in the number of 
 *dimensions, variables and attributes associated with a variable.
 *Integer dim_sizes[NC_MAX_DIMS] array holds the number of dimensions at the beginning of the file.
 *Integer varids[NC_MAX_VARS] array holds the variable id's for each variable.
 *Integer numatts[NC_MAX_VARS] array hold the number of attributes associated with each variable.
 *NC_MAX_DIMS and NC_MAX_VARS are define in the netcdf.h include file.
 */


#if defined (IBMRISC) || defined (HP)
   void copencdf(
#elif defined (CRAY)
   void COPENCDF(
#else
   void copencdf_(
#endif
                  int *inunit,int *cunit,int *varscnt,
                  int dim_sizes[NC_MAX_DIMS],
                  int varids[NC_MAX_VARS],
                  int *file_type,int *grid_resolution,int *globlatt)
{
  struct ncdim {			/* dimension */
    char name[NC_MAX_NAME];
    size_t size;
  };

  struct ncvar {			/* variable */
    char name[NC_MAX_NAME];
    nc_type type;
    int ndims;
    int dims[MAX_VAR_DIMS];
    int natts;
    boolean has_fillval;
    double fillval;
  };


 int ndims,nvars;
 int ngatts;			/* number of global attributes */
 int xdimid;			/* id of unlimited dimension */
 int dimid;			/* dimension id */
 int varid;			/* variable id */
 struct ncdim dims[NC_MAX_DIMS]; /* dimensions */
 size_t vdims[NC_MAX_DIMS];	/* dimension sizes for a single variable */
 struct ncvar var;		/* variable */
 int id;			/* dimension number per variable */
 int is_coord;		        /* true if variable is a coordinate variable */
 int nc_status;		        /* return from netcdf calls */
 int ncid;
 int write_type,cedric;
 int grid_resolution_num;
 int zebalt= 0;
 char * type_name();
 char fname[9];
 int cdferror,format;
 int i1, i2, i3,temp;
 extern void open_netcdf_file();

 printf("inside COPENCDF: inunit= %d \n",*inunit);
    if(*inunit > 99){
       i1 = *inunit/100;
       temp = *inunit - (i1 *100);
       i2 = temp/10;
       temp = temp - (i2 * 10);
       i3 = temp;
    }
    else{
      i1 = *inunit/10;    /* grab ten's digit */
      i2 = *inunit % 10;  /* grab one's digit */
      i3 = -1;
    }
    fname[0] = 'f';
    fname[1] = 'o';
    fname[2] = 'r';
    fname[3] = 't';
    fname[4] = '.';
    fname[5] = i1 + 48;  /* convert to ascii */
    fname[6] = i2 + 48;  /* convert to ascii */

    if(i3 == -1){
       fname[7] = '\0';
    }
    else{
       fname[7] = i3 + 48;
       fname[8] = '\0';
    }

    printf("inside COPENCDF: before nc_open\n");
    nc_status = nc_open(fname, NC_NOWRITE, &ncid);
    if (nc_status != NC_NOERR){
	printf("UNABLE TO OPEN NETCDF FILE %s\n",fname);
        exit(0);
    }
    *cunit = ncid;
    printf("inside COPENCDF: cunit= %d \n",*cunit);
    /*
     *Get the number of dimensions and variables in the netcdf file.
     */
    printf("inside COPENCDF: before nc_inq\n");
    if ( nc_inq(ncid, &ndims, &nvars, &ngatts, &xdimid) != NC_NOERR){
        printf("unable to get netcdf dimensions\n");
        exit(0);
    }

    *varscnt = nvars;
    grid_resolution_num = *grid_resolution;
    *globlatt = ngatts;
    printf("inside COPENCDF: varscnt= %d \n",*varscnt);
    printf("inside COPENCDF: grid_resolution= %d \n",*grid_resolution);
    printf("inside COPENCDF: globlatt= %d \n",*globlatt);
    /*
     *Get netcdf dimension information.
     */

    printf("inside COPENCDF: before get dimension, ndims= %d\n",ndims);
    if (ndims > 0){
       for(dimid = 0; dimid < 10; dimid++) dim_sizes[dimid] = -1;
       for(dimid = 0; dimid < ndims; dimid++){
	   nc_inq_dim(ncid, dimid, dims[dimid].name, &dims[dimid].size);
           if(strncmp(dims[dimid].name,"fields",6) == 0) 
              dim_sizes[NUM_FIELDS_INDEX] = dims[dimid].size;
           else if(strncmp(dims[dimid].name,"short_string",12) == 0)
              dim_sizes[SHORT_STRING_INDEX] = dims[dimid].size;
           else if(strncmp(dims[dimid].name,"long_string",11) == 0)
              dim_sizes[LONG_STRING_INDEX] = dims[dimid].size;
           else if(strncmp(dims[dimid].name,"grids",5) == 0)
              dim_sizes[USWRP_NUM_GRIDS_INDEX] = dims[dimid].size;
           else if(strncmp(dims[dimid].name,"resolutions",11) == 0)
              dim_sizes[RESOLUTION_INDEX] = dims[dimid].size;
           else if(strncmp(dims[dimid].name,"landmarks",9) == 0)
              dim_sizes[NUM_LANDMARKS_INDEX] = dims[dimid].size;

           else if(strncmp(dims[dimid].name,"elevations",10) == 0)
              dim_sizes[NUM_LEVELS_DIM_INDEX] = dims[dimid].size;

           else if(strncmp(dims[dimid].name,"altitude",10) == 0)
                   zebalt = 1;

           else if(strncmp(dims[dimid].name,"z",1) == 0)
              dim_sizes[NUM_LEVELS_DIM_INDEX] = dims[dimid].size;
           else if(strncmp(dims[dimid].name,"lon0",4) == 0){
              if(grid_resolution_num == 0) 
                 dim_sizes[NUM_X_INDEX] = dims[dimid].size;
	   }
           else if(strncmp(dims[dimid].name,"lon1",4) == 0){
               if(grid_resolution_num == 1) 
                  dim_sizes[NUM_X_INDEX] = dims[dimid].size;
	   }
           else if(strncmp(dims[dimid].name,"lon2",4) == 0){
               if(grid_resolution_num == 2) 
                  dim_sizes[NUM_X_INDEX] = dims[dimid].size;
	   }
           else if(strncmp(dims[dimid].name,"lon3",4) == 0){
               if(grid_resolution_num == 3) 
                  dim_sizes[NUM_X_INDEX] = dims[dimid].size;
	   }
           else if(strncmp(dims[dimid].name,"x",1) == 0){
                dim_sizes[NUM_X_INDEX] = dims[dimid].size;
	   }
           else if(strncmp(dims[dimid].name,"longitude",9) == 0){
                dim_sizes[NUM_X_INDEX] = dims[dimid].size;
	   }
           else if(strncmp(dims[dimid].name,"lat0",4) == 0){
              if(grid_resolution_num == 0) 
                 dim_sizes[NUM_Y_INDEX] = dims[dimid].size;
	   }
           else if(strncmp(dims[dimid].name,"lat1",4) == 0){
               if(grid_resolution_num == 1) 
                  dim_sizes[NUM_Y_INDEX] = dims[dimid].size;
	   }
           else if(strncmp(dims[dimid].name,"lat2",4) == 0){
               if(grid_resolution_num == 2) 
                  dim_sizes[NUM_Y_INDEX] = dims[dimid].size;
	   }
           else if(strncmp(dims[dimid].name,"lat3",4) == 0){
               if(grid_resolution_num == 3) 
                  dim_sizes[NUM_Y_INDEX] = dims[dimid].size;
	   }
           else if(strncmp(dims[dimid].name,"latitude",8) == 0){
                dim_sizes[NUM_Y_INDEX] = dims[dimid].size;
           }           
           else if(strncmp(dims[dimid].name,"y",1) == 0){
                dim_sizes[NUM_Y_INDEX] = dims[dimid].size;
           }
           else if(strncmp(dims[dimid].name,"date_string",11) == 0)
                dim_sizes[DATE_STRING_INDEX] = dims[dimid].size;

           else if(strncmp(dims[dimid].name,"number_radars",13) == 0)
                dim_sizes[NUM_RADARS_INDEX] = dims[dimid].size;
           
        }/*for dimid loop*/
    }
    else{
       printf("UNABLE TO GET THE NETCDF DIMENSIONS\n");
       nc_close(ncid);
       exit(-1);
    }
    printf("inside COPENCDF: after get dimension, ndims= %d\n",ndims);
     cedric = 0;
    *file_type = CDFFMT;
     format = CDFFMT;
     write_type = 0; 
    /* get variable info, with variable attributes */
    for (varid = 0; varid < NC_MAX_VARS; varid++) varids[varid] = -1;
    for (varid = 0; varid < nvars; varid++) {
	 nc_inq_var(ncid, varid, var.name, &var.type, &var.ndims,
			     var.dims, &var.natts);

	 if(strncmp(var.name,"grid_names",10) == 0){
            *file_type = USWRPGD;     
            varids[GRID_NAME_INDEX] = varid;
            format = USWRPGD;
	 }
	 else if(strncmp(var.name,"IDHEAD",6) == 0){
            *file_type = 0;
            format = CDFFMT;
            write_type = OLDCEDRIC;    
	 }
         else if(strncmp(var.name,"field_names",11) == 0){
              varids[FIELD_NAME_INDEX] = varid; 
	 }
         else if(strncmp(var.name,"program",7) == 0){ 
             *file_type = CDFFMT;   
             write_type = 1;  
             format =  CDFFMT; 
             cedric = 1;
	 }  
         else if(strncmp(var.name,"grid_type",9) == 0){ 
             varids[GRID_TYPE_INDEX] = varid;
	 }    
         else if(strncmp(var.name,"elevations",10) == 0){ 
             varids[NUM_LEVELS_VAR_INDEX] = varid;
	 } 
         else if(strncmp(var.name,"z",1) == 0){ 
              if(strncmp(var.name,"z_spacing",9) != 0)
                 varids[NUM_LEVELS_VAR_INDEX] = varid;
	 } 
         else if(strncmp(var.name,"lon0",4) == 0){
               if(grid_resolution_num == 0) 
                  varids[GRIDDED_X_INDEX] = varid;
	  }
         else if(strncmp(var.name,"lon1",4) == 0){
               if(grid_resolution_num == 1) 
                  varids[GRIDDED_X_INDEX] = varid;
	  }   
         else if(strncmp(var.name,"lon2",4) == 0){
               if(grid_resolution_num == 2) 
                  varids[GRIDDED_X_INDEX] = varid;
	  }
         else if(strncmp(var.name,"lon3",4) == 0){
               if(grid_resolution_num == 3) 
                  varids[GRIDDED_X_INDEX] = varid;
	  }  
         else if(strncmp(var.name,"lat0",4) == 0){
               if(grid_resolution_num == 0) 
                  varids[GRIDDED_Y_INDEX] = varid;
	  }  
         else if(strncmp(var.name,"lat1",4) == 0){
               if(grid_resolution_num == 1) 
                  varids[GRIDDED_Y_INDEX] = varid;
	  }  
         else if(strncmp(var.name,"lat2",4) == 0){
               if(grid_resolution_num == 2) 
                  varids[GRIDDED_Y_INDEX] = varid;
	  }    
         else if(strncmp(var.name,"lat3",4) == 0){
               if(grid_resolution_num == 3) 
                  varids[GRIDDED_Y_INDEX] = varid;
	  } 
         else if(strncmp(var.name,"grid_resolutions", 16) == 0)
                 varids[VAR_RESOLUTION_INDEX] = varid;

         else if(strncmp(var.name,"longitude_spacing",17) == 0)
	         varids[X_SPACING_INDEX] = varid;  

         else if(var.name[0] == 'x' && var.name[1] == '\0')
              varids[X_MIN_INDEX] = varid; 

         else if(var.name[0] == 'y' && var.name[1] == '\0')
              varids[Y_MIN_INDEX] = varid; 
                  
         else if(strncmp(var.name,"x_spacing",9) == 0)
	         varids[X_SPACING_INDEX] = varid;
         else if(strncmp(var.name,"y_spacing",9) == 0)
	         varids[Y_SPACING_INDEX] = varid;  
         else if(strncmp(var.name,"latitude_spacing",16) == 0)
	         varids[Y_SPACING_INDEX] = varid;  
         else if(strncmp(var.name,"x_min",5) == 0)
	         varids[X_MIN_INDEX] = varid;  
         else if(strncmp(var.name,"x_max",5) == 0)
	         varids[X_MAX_INDEX] = varid;    
         else if(strncmp(var.name,"y_min",5) == 0)
	         varids[Y_MIN_INDEX] = varid;  
         else if(strncmp(var.name,"y_max",5) == 0)
	         varids[Y_MAX_INDEX] = varid;    
         else if(strncmp(var.name,"latitude",8) == 0){
                 if(cedric == 0) varids[LAT_INDEX] = varid;
	 }
         else if(strncmp(var.name,"longitude",9) == 0){
	         if(cedric == 0) varids[LON_INDEX] = varid;  
	 }
         else if(strncmp(var.name,"altitude",8) == 0){
	   if(zebalt == 0){	
	         varids[ALT_INDEX] = varid; 
	   }
           else{
	         varids[NUM_LEVELS_VAR_INDEX] = varid;  
	   }               
	 }
         else if(strncmp(var.name,"radar_latitude",14) == 0)
	         varids[LAT_INDEX] = varid;    
         else if(strncmp(var.name,"radar_longitude",15) == 0)
	         varids[LON_INDEX] = varid;   
         else if(strncmp(var.name,"radar_altitude",14) == 0)	
	         varids[ALT_INDEX] = varid; 
         else if(strncmp(var.name,"cedric_run_date",15) == 0)	
	         varids[CEDRIC_DATE_INDEX] = varid;
         else if(strncmp(var.name,"cedric_run_time",15) == 0)	
	         varids[CEDRIC_TIME_INDEX] = varid;
         else if(strncmp(var.name,"start_date",15) == 0)	
	         varids[START_DATE_INDEX] = varid;  
         else if(strncmp(var.name,"start_time",10) == 0)
	         varids[START_TIME_INDEX] = varid; 
         else if(strncmp(var.name,"end_date",13) == 0)	
	         varids[END_DATE_INDEX] = varid;  
         else if(strncmp(var.name,"stop_times",10) == 0)	
	         varids[END_TIME_INDEX] = varid;    
         else if(strncmp(var.name,"end_time",15) == 0)
	         varids[END_TIME_INDEX] = varid;   
         else if(strncmp(var.name,"landmark_x_coordinates",22) == 0)
	         varids[LANDMARK_X_INDEX] = varid;  
         else if(strncmp(var.name,"landmark_y_coordinates",22) == 0)
	         varids[LANDMARK_Y_INDEX] = varid; 
         else if(strncmp(var.name,"landmark_z_coordinates",22) == 0)
	         varids[LANDMARK_Z_INDEX] = varid;                       
         else if(strncmp(var.name,"landmark_names",14) == 0)
	         varids[LANDMARK_NAMES_INDEX] = varid; 
         else if(strncmp(var.name,"nexrad_radar_name",17) == 0){
	         varids[RADAR_NAME_INDEX] = varid; 
	 } 
         else if(strncmp(var.name,"radar_or_data_origin",20) == 0) 
	         varids[RADAR_NAME_INDEX] = varid; 
         else if(strncmp(var.name,"reference_Nyquist_vel",21) == 0)
                 varids[REF_NYQUIST_INDEX] = varid;
         else if(strncmp(var.name,"nyquist_velocities",18) == 0)
                 varids[NYQUIST_VELS_INDEX] = varid; 
         else if(strncmp(var.name,"nyq_vel",7) == 0)
                 varids[NYQUIST_VELS_INDEX] = varid; 
         else if(strncmp(var.name,"data_source",11) == 0)
                 varids[SOURCE_INDEX] = varid;
         else if(strncmp(var.name,"project_name",12) == 0)
                 varids[PROJ_INDEX] = varid;
         else if(strncmp(var.name,"volume_header",13) == 0)
                 varids[VOL_HEAD_INDEX] = varid;
   
    }
   printf("inside COPENCDF: after varids\n");	
   set_format_type(format,write_type);
   if(varids[VAR_RESOLUTION_INDEX] != -1) 
      printf("GRID RESOLUTION NUMBER : %d\n",grid_resolution_num);
   else
      *grid_resolution = -1;
      printf("GRID RESOLUTION : %d\n",*grid_resolution);
   return;
}/*copencdf*/
/****************************************************************/
/*
 *Netcdf field information.
 */
#if defined (IBMRISC) || defined (HP)
  void cdffldi(
#elif defined (CRAY)
  void CDFFLDI(
#else
  void cdffldi_(
#endif
                 int *cunit,char fldnam[NFMAX][8],
                 int varids[NC_MAX_VARS],
                 int isclfld[NFMAX],
                 int dim_sizes[NC_MAX_DIMS],
                 int *varscnt,
                 int *file_type,int *num_fields)
{
  char tname[NFMAX][8],gname[8];
  int  rcode,index,row,col,jindex;
  int  ss_size;      /*short string size*/
  int  fid,id;          /*field name variable id*/
  int  number,length;
  int  ncid;
  int  the_read_type,found_name,gridded,format;
  int  num_vars,iscale;
  int  attachments;
  long start[3],count[3];

  void read_field_attributes();
  extern int compare_griddednames();
  int  get_field_id();

  /*-----------------------------------------------------------------*/
  ss_size  = dim_sizes[SHORT_STRING_INDEX];
  if(*file_type == USWRPGD) 
      fid = varids[GRID_NAME_INDEX];
  else fid = 
      varids[FIELD_NAME_INDEX];

 /*
  *Get the field names.
  */
   ncid = *cunit;
   found_name = 0;
   row = 0;
   col = 0;

   if(*file_type == USWRPGD){
      printf("we have a uswrp gridded data set\n");
     /*
      *First we need to compare the gridded names entered on the cedric card
      *to all of the grid names to find which grid names we actually want.
      */
      for(index = 0; index < dim_sizes[USWRP_NUM_GRIDS_INDEX]; index++){
          start[0] = row;
          start[1] = col;
          start[2] = 0;
          count[0] = 1;
          count[1] = 1;
          count[2] = ss_size;
          rcode = ncvarget(ncid,fid,start,count,gname);
          if(rcode != NC_NOERR){
             printf("UNABLE TO GET GRID NAMES\n");
             exit(0);
          } 
          col++;
          if(col == dim_sizes[NUM_FIELDS_INDEX]){
             col = 0;
             row = row + 1;
          }
          if(compare_griddednames(gname) == 1){
             fldnam[found_name][0] = gname[0];
             fldnam[found_name][1] = gname[1];
             fldnam[found_name][2] = gname[2];
             fldnam[found_name][3] = gname[3];
             fldnam[found_name][4] = gname[4];
             fldnam[found_name][5] = gname[5];
             fldnam[found_name][6] = gname[6];
             fldnam[found_name][7] = gname[7];  
             found_name++;
	  }
      }/*for index*/

     if(found_name == 0){
        printf("UNABLE TO MATCH GRID NAMES WITH NAMES ENTERED IN THE CEDRIC INPUT FILE\n");
        exit(-1);
      }

     gridded = get_numgridded();
     if(gridded != found_name){
        printf("UNABLE TO MATCH GRID NAMES WITH NAMES ENTERED IN THE CEDRIC INPUT FILE\n");
        exit(-1);
     }
     *num_fields = gridded;   
   }
   else{
     format = CDFFMT;
     start[0] = 0;
     start[1] = 0;
     count[0] = dim_sizes[NUM_FIELDS_INDEX];
     count[1] = ss_size;
     rcode = ncvarget(ncid,fid,start,count,tname);
     if(rcode != NC_NOERR){
      printf("UNABLE TO GET FIELD NAMES\n");
      exit(0);
     } 
        
     for(index = 0; index < count[0]; index++){
       for(jindex = 0; jindex < 8; jindex++){
	 if(tname[index][jindex] != ' '){
           fldnam[index][jindex] = tname[index][jindex];
	 }
         else{
           fldnam[index][jindex] = ' ';
         }
       }
    }
     *num_fields = count[0];
   } 

   /*
    *Now get the field variable id.
    *Read the field scale from the variable attributes.
    */
   num_vars = *varscnt;
   for(index = 0; index < *num_fields; index++){
        fid = get_field_id(ncid,num_vars,fldnam[index],&attachments);
        if(fid == -1){
           printf("UNABLE TO GET VARIABLE ID FOR FIELD %s\n",fldnam[index]);
           exit(-1);
	}
        varids[START_FIELD_IDS + index] = fid;
        read_field_attributes(ncid,fid,attachments,format,&iscale,
                              fldnam[index]);
        isclfld[index] = iscale;
   }

}/*cdffldi*/
/****************************************************************/

int get_field_id(int ncid,int num_vars,char fname[8],int *attachments)
{

  struct ncvar {			/* variable */
    char name[NC_MAX_NAME];
    nc_type type;
    int ndims;
    int natts;
    int dims[MAX_VAR_DIMS];
    boolean has_fillval;
    double fillval;
  };

  struct ncvar var;		/* variable */
  int    varid,rcode;
  int    ivname[8],ifname[8],index;
  int    name_end;


    for (varid = 0; varid < num_vars; varid++){
	 rcode = nc_inq_var(ncid, varid, var.name, &var.type, &var.ndims,
			     var.dims, &var.natts);
         if(rcode != NC_NOERR){
            printf("UNABLE TO GET FIELD VARIABLE ID's\n");
            exit(0);
	 }
         name_end = 0;
         for(index = 0; index < 8; index++){
             ivname[index] = -1;
             ifname[index] = -1;
         }
          for(index = 0; index < 8; index++){
            if(var.name[index] != ' ' && var.name[index] != '\0'){
               ivname[index] = (int)var.name[index];
	    }
            else{
               name_end = 1;
	    }
            if(fname[index] != ' ' && fname[index] != '\0'){
               ifname[index] = (int)fname[index];  
	    }
            if(name_end == 1){
               if(ifname[0] == ivname[0] &&
                 ifname[1] == ivname[1] &&
                 ifname[2] == ivname[2] &&
                 ifname[3] == ivname[3] &&
                 ifname[4] == ivname[4] &&
                 ifname[5] == ivname[5] &&
                 ifname[6] == ivname[6] &&
                 ifname[7] == ivname[7]){
                    *attachments = var.natts;
                    return(varid);
                 }
	    }/*name end*/
	 }/*for index*/
    }/*for varid*/

    *attachments = -1;
    return(-1);
}/*get_field_id*/
/******************************************************************/
void read_field_attributes(int ncid,int fid,int attachments,int format,
                           int *iscale,char fname[8])
{
struct ncatt {			/* attribute */
    int var;
    char name[NC_MAX_NAME];
    nc_type type;
    size_t len;
    char *string;		/* for text attributes (type = NC_CHAR) */
    double *vals;		/* for numeric attributes of all types */
};

 struct ncatt att;		/* attribute */
 int index,rcode,length;
 double dval,dscale,dbias,dbad[2];
 float  fval;
 int    ival;
 short  sval;


 *iscale = 1;
 for(index = 0; index < attachments; index++){
    rcode = nc_inq_attname(ncid, fid, index, att.name);
    rcode = nc_inq_att(ncid, fid, att.name, &att.type, &att.len);
    if(rcode != NC_NOERR){
        printf("UNABLE TO READ FIELD SCALE,BIAS AND BAD FLAG INFORMATION\n");
        exit(-1);
    }
    if(strncmp(att.name,"scale_factor",12) == 0){
      *iscale = 1;
      dscale = 1;
      switch(att.type){
        case NC_DOUBLE:{
                        rcode = nc_get_att_double(ncid,fid,att.name,&dval);
                        *iscale = (int)dval;
                        dscale = dval;
                        break;
	}
        case NC_FLOAT:{
                        rcode = nc_get_att_float(ncid,fid,att.name,&fval);
                        *iscale = (int)fval;
                        dscale = (double)fval;                
                        break;
	}
        case NC_INT:{
                        rcode = nc_get_att_int(ncid,fid,att.name,&ival);
                        *iscale = ival;
                        dscale = (double)ival;
                        break;
        }
       case NC_SHORT:{
                        rcode = nc_get_att_short(ncid,fid,att.name,&sval);
                        *iscale = (int)sval;
                        dscale = (double)sval;
                        break;
       }
      default:{
       	printf("type_name: bad type %d for field scale", att.type);
        exit(0);
      }
      }/*end switch*/
      if(rcode != NC_NOERR){
         printf("UNABLE TO GET FIELD SCALE FOR FIELD ID %d\n",fid);
         nc_close(ncid);
      }
    }/*end the strncmp if*/

    else if(strncmp(att.name,"add_offset",10) == 0){
      dbias = 0;
      switch(att.type){
        case NC_DOUBLE:{
                        rcode = nc_get_att_double(ncid,fid,att.name,&dval);
                        dbias = dval;
                        break;
	}
        case NC_FLOAT:{
                        rcode = nc_get_att_float(ncid,fid,att.name,&fval);
                        dbias = (double)fval;                
                        break;
	}
        case NC_INT:{
                        rcode = nc_get_att_int(ncid,fid,att.name,&ival);
                        dbias = (double)ival;
                        break;
        }
       case NC_SHORT:{
                        rcode = nc_get_att_short(ncid,fid,att.name,&sval);
                        dbias = (double)sval;
                        break;
       }
      default:{
       	printf("type_name: bad type %d for field bias", att.type);
        exit(0);
      }
      }/*end switch*/
      if(rcode != NC_NOERR){
         printf("UNABLE TO GET FIELD BIAS FOR FIELD ID %d\n",fid);
         nc_close(ncid);
         exit(-1);
      }
    }/*end else if for strncmp add_offset*/

    else if(strncmp(att.name,"missing_value",13) == 0){
        length = att.len;
	att.vals = (double *) malloc(att.len * sizeof(double));
	if (!att.vals) {
	    printf("Out of memory!");
	    nc_close(ncid);
	    exit(-1);
	}         
        rcode = nc_get_att_double(ncid, fid, att.name, att.vals );
        if(rcode != NC_NOERR){
           printf("UNABLE TO GET BAD DATA VALUE(S)FOR FIELD \n",fid);
           nc_close(ncid);
           exit(0);
	}
        dbad[0] = -32768;
        dbad[1] = -32768;
        dbad[0] = att.vals[0];
        if(length > 1) dbad[1] = att.vals[1];
        free(att.vals);
    }/*end else for strncmp missing*/

 }       

}/*read_field_attributes*/

 /********************************************************************************/
 /*
  *cdf level information.
  */
#if defined (IBMRISC) || defined (HP)
    void cdflvli(
#elif defined (CRAY)
    void CDFLVLI__(
#else
    void cdflvli_(
#endif
		   int *cunit,
                   char type_of_grid[3], 
                   float levs[MAXZLEV],
                   float nyquists[MAXZLEV],
                   float *ref_nyq,
                   int *v_levels,
                   int varids[NC_MAX_VARS],
                   int dimsizes[NC_MAX_DIMS])
{

 int ncid,index,length;
 int rcode,varid,ls_id,grid_id;
 int vertid,nlevels;
 char the_grid_type[80],blanks[10];
 long ls,num;
 float *levary; 
 float fvalue;
 double grid_nyqs[MAXZLEV]; 
 static long count,start;
 size_t astart[5],acount[5];
 extern void save_gridded_nyquists();


 ncid = *cunit;

/*
 *Get number of elevations or z levels.
 */   
  nlevels = dimsizes[NUM_LEVELS_DIM_INDEX];
  vertid  = varids[NUM_LEVELS_VAR_INDEX];
  astart[0] = 0;
  acount[0] = nlevels;
  levary = (float *)malloc(nlevels * sizeof(float));
  if(!levary){
     printf("UNABLE TO GET THE LEVEL VALUES\n");
     nc_close(ncid);
     exit(0);
  }
  rcode = nc_get_vara_float(ncid,vertid,astart,acount,levary);
  if(rcode != NC_NOERR){
       printf("UNABLE TO GET THE LEVEL VALUES\n");
       nc_close(ncid);
       exit(0);
  }
  printf("NETCDF LEVEL VALUES    :\n");
  for(index = 0; index < nlevels; index++){
      levs[index] = levary[index];
      num = index + 1;
      if(levs[index] < 10.0)
          printf("                        %2d   %3.3f\n",num,levs[index]);
      else
          printf("                        %2d  %3.3f\n",num,levs[index]);
  }

  *v_levels = nlevels;

/*
 *Get nyquist velocities.
 */

 *ref_nyq = 0.0;

 for(index = 0; index < nlevels; index++)  nyquists[index] = 0.0;
 if(varids[NYQUIST_VELS_INDEX] != -1){
       printf("NETCDF NYQUIST VALUES   :\n");
       count = nlevels;
       start = 0;
       rcode = nc_get_vara_float(ncid,varids[NYQUIST_VELS_INDEX],astart,acount,levary);
       if(rcode != NC_NOERR){
          printf("UNABLE TO GET NYQUIST VELOCITIES\n");
          nc_close(ncid);
          exit(-1);
       }
       for(index = 0; index < count; index++){
          num = index + 1;          
          nyquists[index] = levary[index];
          if(nyquists[index] < 10.0)
              printf("                       %2d    %3.3f\n",num,nyquists[index]);
          else
              printf("                       %2d   %3.3f\n",num,nyquists[index]);
          grid_nyqs[index] = (double)nyquists[index];
       }
       *ref_nyq = nyquists[0];
       set_nyquists_flag();
       save_gridded_nyquists(grid_nyqs,nlevels);
 }
 free(levary);

 
 if(varids[REF_NYQUIST_INDEX] != -1){
     nc_get_var_float(ncid,varids[REF_NYQUIST_INDEX],&fvalue);  
     *ref_nyq = fvalue;
 }


 /*
  *Get grid type.
  */
  grid_id = varids[GRID_TYPE_INDEX];
  count = ls = dimsizes[LONG_STRING_INDEX];
  start  = 0; 
  rcode = ncvarget(ncid,grid_id,&start,&count,the_grid_type); 
       if(rcode != NC_NOERR){
          printf("UNABLE TO GET NETCDF FILE GRID TYPE\n");
          nc_close(ncid);
          exit(-1);
       }
  length = strlen(the_grid_type);
  if(length == 0){
     printf("UNKNOWN GRID TYPE IN GRIDDED NETCDF FILE\n");
     exit(0);
  }

  if(the_grid_type[1] == 'l' && the_grid_type[2] == 'o' && 
     the_grid_type[3] == 'n' ){
     type_of_grid[0] = 'l';
     type_of_grid[1] = 'l';
     if(the_grid_type[9] == 'e'){
        type_of_grid[2] = 'e';
     }
     else if(the_grid_type[9] == 'z'){
        type_of_grid[2] = 'z';
     }
     else{
        printf("UNRECOGNIZED GRID TYPE IN NETCDF FILE\n");
        exit(0);
     }
  }
  else if(the_grid_type[1] == 'P' || the_grid_type[1] == 'R'){
     type_of_grid[0] = the_grid_type[1];
     type_of_grid[1] = the_grid_type[2];
     type_of_grid[2] = the_grid_type[3];
  }       
  else{ 
     type_of_grid[0] = the_grid_type[1];
     type_of_grid[1] = the_grid_type[3];
     type_of_grid[2] = the_grid_type[5];
  }

}/*cdf_level_info*/
    
/****************************************************************/
#if defined (IBMRISC) || defined (HP)
  void scanmode(
#elif defined (CRAY)
  void SCANMODE(
#else
  void scanmode_(
#endif
                 char type_of_grid[3],char cscan1[2],char cscan2[2])

{
  if((type_of_grid[0] == 'l') && (type_of_grid[2] == 'e')){
      cscan1[0] = 'L';
      cscan1[1] = 'L';
      cscan2[0] = 'E';
      cscan2[1] = ' ';
   }
  else if((type_of_grid[0] == 'l') && (type_of_grid[2] == 'z')){
      cscan1[0] = 'L';
      cscan1[1] = 'L';
      cscan2[0] = 'Z';
      cscan2[1] = ' ';
   }
  else if((type_of_grid[0] == 'x') && (type_of_grid[2] == 'z')){
      cscan1[0] = 'C';
      cscan1[1] = 'R';
      cscan2[0] = 'T';
      cscan2[1] = ' ';
   }
   else if((type_of_grid[0] == 'x') && (type_of_grid[2] == 'e')){
      cscan1[0] = 'E';
      cscan1[1] = 'L';
      cscan2[0] = 'E';
      cscan2[1] = 'V';
   }
   else if((type_of_grid[0] == 'x') && (type_of_grid[2] == 'c')){
      cscan1[0] = 'C';
      cscan1[1] = 'O';
      cscan2[0] = 'P';
      cscan2[1] = 'L';
      }
   else if((type_of_grid[0] == 'P') && (type_of_grid[1] == 'P')){
      cscan1[0] = 'P';
      cscan1[1] = 'P';
      cscan2[0] = 'I';
      cscan2[1] = ' ';
   }
   else if((type_of_grid[0] == 'R') && (type_of_grid[1] == 'H')){
      cscan1[0] = 'R';
      cscan1[1] = 'H';
      cscan2[0] = 'I';
      cscan2[1] = ' ';
   }

}/*scan_mode*/

 /********************************************************************************/
 /*
  *cdf xyz information.
  */
#if defined (IBMRISC) || defined (HP)
    void cdfxyzi(
#elif defined (CRAY)
    void CDFXYZI(
#else
    void cdfxyzi_(
#endif
                       int *funit,int numxyz[3],int spacing[NFMAX],
                       int minxy[2],int maxxy[2],int varids[NC_MAX_VARS],
                       int dimsizes[NC_MAX_DIMS], int *grid)


{
  int ncid,rcode,ival,index,gscale;
  int ignscl = 100;
  float fval;
  long  resolid;
  float varray[MAXX];
  size_t astart[5],acount[5],vstart[3];
  int ndims;
  int grid_resolution_num,num;
  char ctemp[80],var_units[80];

struct ncatt {			/* attribute */
    int var;
    char name[NC_MAX_NAME];
    nc_type type;
    size_t len;
    char *string;		/* for text attributes (type = NC_CHAR) */
    double *vals;		/* for numeric attributes of all types */
};

    struct ncvar {                        /* variable */
    char name[NC_MAX_NAME];
    nc_type type;
    int ndims;
    int natts;
    int dims[MAX_VAR_DIMS];
    boolean has_fillval;
    double fillval;
  };

  struct ncvar var;             /* variable */
  struct ncatt att;		/* attribute */


  ncid = *funit;
  grid_resolution_num = *grid;

  numxyz[0] = dimsizes[NUM_X_INDEX];
  numxyz[1] = dimsizes[NUM_Y_INDEX];

  if( varids[VAR_RESOLUTION_INDEX]!= -1){/*A USWRP gridded file*/
      resolid   = varids[VAR_RESOLUTION_INDEX];
      astart[0]  = 0;
      acount[0]  = dimsizes[RESOLUTION_INDEX];
      rcode = nc_get_vara_float(ncid,resolid,astart,acount, varray);  
      if(rcode != NC_NOERR){
        printf("unable to get grid spacing from USWRP gridded data set\n");
        nc_close(ncid);
        exit(-1);
      }
      printf("GRID RESOLUTIONS CHOSEN : %f in x and %f y\n",varray[grid_resolution_num],varray[grid_resolution_num]);
      fval= varray[grid_resolution_num] * 1000.;
      spacing[0] = (int)fval;
      spacing[1] = (int)fval;
      acount[0]  = dimsizes[NUM_X_INDEX];
      rcode = nc_get_vara_float(ncid,varids[GRIDDED_X_INDEX],astart,acount, varray);
      fval = varray[0] * ignscl;
      minxy[0] = (int)fval;

      num = dimsizes[NUM_X_INDEX];
      fval = varray[num-1] * ignscl;
      maxxy[0] = (int)fval;
      acount[0]  = dimsizes[NUM_Y_INDEX];
      rcode = nc_get_vara_float(ncid,varids[GRIDDED_Y_INDEX],astart,acount, varray);
      fval = varray[0] * ignscl;
      minxy[1] = (int)fval;
      num = dimsizes[NUM_Y_INDEX];   
      fval = varray[num-1] * ignscl;              
      maxxy[1] = (int)fval;
      rcode = nc_get_att_text(ncid,NC_GLOBAL,"grid_center",ctemp);

  }

  else{
      /*
       *x y and z grid spacing values.
       */
      vstart[0] = 0;
      rcode = nc_get_var1_float(ncid,varids[X_SPACING_INDEX],vstart,&fval);
      if(rcode != NC_NOERR){
        printf("unable to get x grid spacing\n");
        nc_close(ncid);
        exit(-1);
      }
      /*
       *Determine the units of the x and y spacing.
       */
       strncmp(var.name,"x_spacing",9);
       var.dims[0] = 0;
       nc_inq_var(ncid, varids[X_SPACING_INDEX], var.name, &var.type, &var.ndims,var.dims, &var.natts);
       for(index = 0; index < var.natts; index++){
           rcode = nc_inq_attname(ncid,varids[X_SPACING_INDEX], index, att.name);
           rcode = nc_inq_att(ncid, varids[X_SPACING_INDEX], att.name, &att.type, &att.len);
           if(strncmp(att.name,"units",5) == 0){
                rcode = nc_get_att_text(ncid,varids[X_SPACING_INDEX],att.name,var_units);
                if(rcode != NC_NOERR){
                   printf("unable to get units for the grid x and y axis\n");
                   nc_close(ncid);
                   exit(-1);
                }  
	     }
       }/*end of for index loop*/
       gscale = 1;  
       if(var_units[0] == 'k' && var_units[1] == 'm') gscale = 1000; 
       if(var_units[0] == 'd' && var_units[1] == 'e' && var_units[2] == 'g') gscale = 1000;  
       fval = fval * gscale;  
       spacing[0] = (int)fval;


      rcode = nc_get_var1_float(ncid,varids[Y_SPACING_INDEX],vstart,&fval);
      if(rcode != NC_NOERR){
        printf("unable to get y grid spacing\n");
        nc_close(ncid);
        exit(-1);
      }
      fval = fval * gscale;
      spacing[1] = (int)fval;

      /* ---------MIN X AND MAX X VALUES-----------------*/
      nc_inq_varndims(ncid,varids[X_MIN_INDEX],&ndims);
      if(ndims == 0){/*a cedric generated file*/
         gscale = 100.;
         spacing[1] = fval;
         rcode = nc_get_var1_float(ncid,varids[X_MIN_INDEX],vstart,&fval);
         if(rcode != NC_NOERR){
            printf("unable to get minimim x value\n");
            nc_close(ncid);
             exit(-1);
         }      
         minxy[0] = fval * gscale;
      }
      else{/*an Oye generated file.  The x values will be in array the units will be attached*/
         astart[0]  = 0;
         acount[0]  = dimsizes[NUM_X_INDEX];
         rcode = nc_get_vara_float(ncid,varids[X_MIN_INDEX],astart,acount, varray); 
         if(rcode != NC_NOERR){
            printf("unable to get x grid values array\n");
            nc_close(ncid);
            exit(-1);
         }  
         /*
          *Determine the units of the x values.
          */
         var.name[0] = 'x';
         var.name[1] = '\0';
         var.dims[0] = 1;
         nc_inq_var(ncid, varids[X_MIN_INDEX], var.name, &var.type, &var.ndims,var.dims, &var.natts);
         for(index = 0; index < var.natts; index++){
             rcode = nc_inq_attname(ncid,varids[X_MIN_INDEX], index, att.name);
             rcode = nc_inq_att(ncid, varids[X_MIN_INDEX], att.name, &att.type, &att.len);
             if(strncmp(att.name,"units",5) == 0){
                rcode = nc_get_att_text(ncid,varids[X_MIN_INDEX],att.name,var_units);
                if(rcode != NC_NOERR){
                   printf("unable to get units for the grid x and y axis\n");
                   nc_close(ncid);
                   exit(-1);
                }  
	     }
	 }/*end of for index loop*/
         gscale = 1;  /*gscale is the cedric general scaling factor.*/
         if(var_units[0] == 'k' && var_units[1] == 'm') gscale = 100;
         minxy[0] = varray[0] * gscale;
         maxxy[0] = varray[dimsizes[NUM_X_INDEX] -1] * gscale;
      }
      if(ndims == 0){
         rcode = nc_get_var1_float(ncid,varids[X_MAX_INDEX],vstart,&fval); 
         if(rcode != NC_NOERR){
            printf("unable to get max x value\n");
            nc_close(ncid);
            exit(-1);
         }       
         maxxy[0] = fval * gscale;
      }
      /*-------------MIN AND MAX Y VALUES ------------------*/
      nc_inq_varndims(ncid,varids[X_MIN_INDEX],&ndims);
      if(ndims == 0){
         rcode = nc_get_var1_float(ncid,varids[Y_MIN_INDEX],vstart,&fval);
         if(rcode != NC_NOERR){
            printf("unable to get minimim y value\n");
            nc_close(ncid);
            exit(-1);
         }  
         minxy[1] = fval * gscale;
      }
      else{/*an Oye generated file*/
         astart[0]  = 0;
         acount[0]  = dimsizes[NUM_Y_INDEX];
         rcode = nc_get_vara_float(ncid,varids[Y_MIN_INDEX],astart,acount, varray);  
         if(rcode != NC_NOERR){
            printf("unable to get x grid values array\n");
            nc_close(ncid);
            exit(-1);
         }  
         minxy[1] = varray[0] * gscale;
         maxxy[1] = varray[dimsizes[NUM_Y_INDEX] -1] * gscale;
      }         
      if(ndims == 0){
         rcode = nc_get_var1_float(ncid,varids[Y_MAX_INDEX],vstart,&fval); 
         if(rcode != NC_NOERR){
            printf("unable to get max y value\n");
            nc_close(ncid);
            exit(-1);
         }  
         maxxy[1] = fval * gscale;  
      }
  }      
}/*cdf_xyz_info*/

/********************************************************************************/
/*
 *Netcdf position information.  Lat lon and altitude.
 */
#if defined (IBMRISC) || defined (HP)
   void cdfposn(
#elif defined (CRAY)
   void CDFPOSN(
#else
   void cdfposn_(
#endif
                    int *cunit,int item[NID], 
                    int varids[NC_MAX_VARS])

{
   int ncid,rcode;
   double lonv,latv,altv;
   size_t  vstart[2];
   int  deg,min;
   double sec;
   void convert_to_degminsec();


   ncid = *cunit;
   vstart[0] = 0;

   rcode = nc_get_var1_double(ncid,varids[LON_INDEX],vstart,&lonv); 
   if(rcode != NC_NOERR){
      printf("UNABLE TO READ LONGITUDE FROM NETCDF FILE\n");
      nc_close(ncid);
      exit(-1);
   }
   convert_to_degminsec(&deg,&min,&sec,lonv);
   item[35] = deg;
   item[36] = min;
   item[37] = (int) (sec * 100);

   rcode = nc_get_var1_double(ncid,varids[LAT_INDEX],vstart,&latv); 
   if(rcode != NC_NOERR){
      printf("UNABLE TO READ LATITUDE FROM NETCDF FILE\n");
      nc_close(ncid);
      exit(-1);
   }
   convert_to_degminsec(&deg,&min,&sec,latv);
   item[32] = deg;
   item[33] = min;
   item[34] = (int)(sec * 100);


   rcode = nc_get_var1_double(ncid,varids[ALT_INDEX],vstart,&altv); 
   if(rcode != NC_NOERR){
      printf("UNABLE TO READ ALTITUDE FROM NETCDF FILE\n");
      nc_close(ncid);
      exit(-1);
   }
   
  /*
   *Save a copy of the lat and lon for writing out.
   */
   save_latlonalt(latv,lonv,altv);

}/*cdf_latlon_info*/


 /********************************************************************************/
#if defined (IBMRISC) || defined (HP)
   void cdftimei(
#elif defined (CRAY)
   void CDFTIMEI
#else
   void cdftimei_(
#endif
                   int *cunit,int item[NID],
                   int varids[NC_MAX_VARS],
                   int dimsizes[NC_MAX_DIMS],
                   char volname[8],char the_date[8],
                   char the_time[8],int *globlatt,
                   int *filetype)
{
  int  thisyear,datayear;
  int  attid;
  long num_levels;
  int temp,ncid,num,rcode,index;
  int year,diff,tarray[4];
  int num_to_malloc,ival;
  double *value;
  size_t the_size;
  static char ctemp[100],charyear[5];
  char name[80];
  time_t dtime;
  struct tm *dt;
  extern void convert_string_int();
  void   get_volume_name();
  extern int get_pesent_year();

  thisyear = get_present_year();
  ncid = *cunit;
  num_to_malloc = dimsizes[NUM_FIELDS_INDEX] * dimsizes[NUM_LEVELS_DIM_INDEX];


  item[20] = item[22] = item[21] = -1;
      
  if(*filetype == CDFFMT){
     if(varids[START_DATE_INDEX] == -1){/*a non cedric generated file*/
        for(attid = 0; attid < *globlatt; attid++){
	    nc_inq_attname(ncid, NC_GLOBAL,attid,name);
            if(strncmp(name,"Year",4)){
               rcode = nc_get_att_int(ncid,NC_GLOBAL,"Year",&ival);
               item[20]  = ival;
               item[115] = ival;
	    }
            if(strncmp(name,"Month",5)){
               rcode = nc_get_att_int(ncid,NC_GLOBAL,"Month",&ival);
               item[21] = ival;
               item[116] = ival;
               tarray[0] = ival;
	    }
            if(strncmp(name,"Day",3)){
               rcode = nc_get_att_int(ncid,NC_GLOBAL,"Day",&ival);
               item[22] = ival;
               item[117] = ival; 
               tarray[1] = ival;
	    } 
            if(strncmp(name,"Hour",4)){
               rcode = nc_get_att_int(ncid,NC_GLOBAL,"Hour",&ival);
               item[23] = ival;
               item[118] = ival; 
               tarray[2] = ival;   
	    }  
            if(strncmp(name,"Minute",6)){
               rcode = nc_get_att_int(ncid,NC_GLOBAL,"Minute",&ival);
               item[24]  = ival;
               item[119] = ival; 
               tarray[3] = ival;   
	    }    
            if(strncmp(name,"Second",6)){
               rcode = nc_get_att_int(ncid,NC_GLOBAL,"Second",&ival);
               item[25] = ival;
               item[120] = ival;  
	    }     
	}/*for attid*/
        get_volume_name(volname,tarray);
        time(&dtime);
        strftime(ctemp,9,"%Y%m%d",localtime(&dtime));
        the_date[0] = ctemp[0];
        the_date[1] = ctemp[1];
        the_date[2] = ctemp[2];
        the_date[3] = ctemp[3];
        the_date[4] = ctemp[4];
        the_date[5] = ctemp[5];
        the_date[6] = ctemp[6];
        the_date[7] = ctemp[7];
        strftime(ctemp,9,"%H%M%S",localtime(&dtime));
        the_time[0] = ctemp[0];
        the_time[1] = ctemp[1];
        the_time[2] = ctemp[2];
        the_time[3] = ctemp[3];
        the_time[4] = ctemp[4];
        the_time[5] = ctemp[5];
        the_time[6] = ctemp[6];
        the_time[7] = ctemp[7];
     }/*end if */
     else{/*a cedric generated netcdf file*/
        nc_get_var_text(ncid,varids[START_DATE_INDEX],ctemp);
        convert_string_int(ctemp,tarray,1);
        item[21]  = tarray[0];
        item[116] = tarray[0];
        item[22]  = tarray[1];
        item[117] = tarray[1];
        item[20]  = tarray[2];
        item[115] = tarray[2];

        memset(ctemp,' ',100);
        nc_get_var_text(ncid,varids[END_DATE_INDEX],ctemp);
        convert_string_int(ctemp,tarray,1); 
        item[26]  = tarray[2];
        item[121] = tarray[2];
        item[27]  = tarray[0];
        item[122] = tarray[0];
        item[28]  = tarray[1];
        item[123] = tarray[1];

 
        memset(ctemp,' ',100);
        nc_get_var_text(ncid,varids[START_TIME_INDEX],ctemp);
        convert_string_int(ctemp,tarray,0); 
        item[25]  = tarray[2];
        item[120] = tarray[2];
        item[24]  = tarray[1]; 
        item[119] = tarray[1];
        item[23]  = tarray[0];
        item[118] = tarray[0];

  
        memset(ctemp,' ',100);
        nc_get_var_text(ncid,varids[END_TIME_INDEX],ctemp);
        convert_string_int(ctemp,tarray,0); 
        item[29]  = tarray[2];
        item[126] = tarray[2];
        item[30]  = tarray[1]; 
        item[125] = tarray[1];
        item[31]  = tarray[0];
        item[124] = tarray[0];

        memset(ctemp,' ',100);
        nc_get_var_text(ncid,varids[VOL_HEAD_INDEX],volname);    
        nc_get_var_text(ncid,varids[CEDRIC_DATE_INDEX],ctemp); 
        the_date[0] = ctemp[0];
        the_date[1] = ctemp[1];
        the_date[2] = ctemp[3];
        the_date[3] = ctemp[4];
        the_date[4] = ctemp[6];
        the_date[5] = ctemp[7];
        the_date[6] = ctemp[8];
        the_date[7] = ctemp[9];
        memset(ctemp,' ',100);
        nc_get_var_text(ncid,varids[CEDRIC_TIME_INDEX],ctemp);
        the_time[0] = ctemp[0];
        the_time[1] = ctemp[1];
        the_time[2] = ctemp[2];
        the_time[3] = ctemp[3];
        the_time[4] = ctemp[4];
        the_time[5] = ctemp[5];
        the_time[6] = ctemp[6];
        the_time[7] = ctemp[7];
     }

     
  }/*end if file type is 2*/

  if(*filetype == USWRPGD){
      
      value = (double *)malloc(num_to_malloc  * sizeof(double));
      if(!value){
         printf("NOT ENOUGH SPACE TO MALLOC ARRAY FOR START TIMES\n");
         exit(0);
      }

      rcode = nc_get_var_double(ncid,varids[START_TIME_INDEX], value);
      if(rcode != NC_NOERR){
         printf("UNABLE TO GET START TIME ARRAY FROM NETCDF FILE\n");
         exit(0);
      }

      temp = (int)value[0];
      dtime = temp;
      dt = gmtime(&dtime);
      item[21] = dt->tm_mon + 1;
      item[116] =dt->tm_mon + 1; 
      item[22] = dt->tm_mday;
      item[117] = dt->tm_mday;
      item[25] = dt->tm_sec;
      item[120] = dt->tm_sec;
      item[24] = dt->tm_min; 
      item[119] = dt->tm_min;
      item[23] = dt->tm_hour;
      item[118] = dt->tm_hour;
      tarray[0] = dt->tm_mon + 1;
      tarray[1] = dt->tm_mday;
      tarray[2] = dt->tm_hour;
      tarray[3] = dt->tm_min;
      datayear = dt->tm_year;
      temp = dt->tm_year/100;
      if(temp == 0){
         temp = 2000 + datayear;
         diff = thisyear - temp;
         if(diff < 0) 
            datayear = 1900 + datayear;
         else
            datayear = 2000 + datayear;
      }
      item[20] = datayear;
      item[115] = datayear;
      get_volume_name(volname,tarray);

      num = dimsizes[NUM_LEVELS_DIM_INDEX];
      for(index = 0; index < num; index++) value[index] = 0.0;
      nc_get_var_double(ncid,varids[END_TIME_INDEX] , value);
      temp = (int)value[num-1];
      dtime = temp;
      dt = gmtime(&dtime);
      item[27] = dt->tm_mon + 1;
      item[122] = dt->tm_mon + 1;
      item[28] = dt->tm_mday;
      item[123] = dt->tm_mday;
      item[29] = dt->tm_sec;
      item[126] = dt->tm_sec;
      item[30] = dt->tm_min; 
      item[125] = dt->tm_min;
      item[31] = dt->tm_hour;
      item[124] = dt->tm_hour;
      datayear = dt->tm_year;
      temp = dt->tm_year/100;
      if(temp == 0){
         temp = 2000 + datayear;
         diff = thisyear - temp;
         if(diff < 0) 
            datayear = 1900 + datayear;
         else
            datayear = 2000 + datayear;
      }
      item[26] = datayear;
      item[121] = datayear;
      free(value);

    /*
     *Get production date.
     */
      rcode = nc_get_att_text(ncid,NC_GLOBAL,"production_date",ctemp);
      if(rcode != NC_NOERR){
         printf("unable to get production date from NETCDF file\n");
         strncpy(the_date,"        ",8);
         strncpy(the_time,"        ",8);
         return;
      }

      switch (ctemp[4]){
        case 'A':{
              if(ctemp[5] == 'p'){
                the_date[0] = '0';
                the_date[1] = '4';
	      }
              else if(ctemp[5] == 'u'){
                 the_date[0] = '0';
                 the_date[1] = '8';
	      }
	      else{
                  printf("UNKNOWN PRODUCTION DATE\n");
                  strncpy(the_date,"        ",8);
                  strncpy(the_time,"        ",8);
                  return;
	      }              
              break;
        }
        case 'D':{
              the_date[0] = '1';
              the_date[1] = '2';
              break;
        }
        case 'F':{
              the_date[0] = '0';
              the_date[1] = '2';
              break;
        }
        case 'J':{
              if(ctemp[5] == 'a'){
                 the_date[0] = '0';
                 the_date[1] = '1';
	      }
	      else if(ctemp[5] == 'u' && ctemp[6] == 'n'){
                 the_date[0] = '0';
                 the_date[1] = '6';
	      }
              else if(ctemp[5] == 'u' && ctemp[6] == 'l'){
                 the_date[0] = '0';
                 the_date[1] = '7';
	      }
	      else{
                  printf("UNKNOWN PRODUCTION DATE\n");
                  strncpy(the_date,"        ",8);
                  strncpy(the_time,"        ",8);
                  return;
	      }
              break;
        }
        case 'M':{
              if(ctemp[6] == 'r'){
                 the_date[0] = '0';
                 the_date[1] = '3';
	      }
              else if(ctemp[6] == 'y'){
                 the_date[0] = '0';
                 the_date[1] = '5';
	      }
	      else{
                  printf("UNKNOWN PRODUCTION DATE\n");
                  strncpy(the_date,"        ",8);
                  strncpy(the_time,"        ",8);
                  return;
	      }
              break;
        }
        case 'N':{
              the_date[0] = '1';
              the_date[1] = '1';
              break;
        }
        case 'O':{
              the_date[0] = '1';
              the_date[1] = '0';
              break;
        }
        case 'S':{
              the_date[0] = '0';
              the_date[1] = '9';
              break;
        }
        default:{
              printf("UNKNOWN PRODUCTION DATE\n");
              strncpy(the_date,"        ",8);
              strncpy(the_time,"        ",8);
              return;
              break;
	}
      }/*END OF SWITCH*/

      if(ctemp[8] == ' '){
         the_date[2] = '0';
         the_date[3] = ctemp[9];
      }
      else{
         the_date[2] = ctemp[8];
         the_date[3] = ctemp[9];
      }
      the_date[4] = ctemp[20];
      the_date[5] = ctemp[21];
      the_date[6] = ctemp[22];
      the_date[7] = ctemp[23];
  

      the_time[0] = ctemp[11];
      the_time[1] = ctemp[12];
      the_time[2] = ctemp[13];
      the_time[3] = ctemp[14];
      the_time[4] = ctemp[15];
      the_time[5] = ctemp[16];
      the_time[6] = ctemp[17];
      the_time[7] = ctemp[18];
   }

}/*cdf_time_info*/   

 /*********************************************************************/
/*
 *get volume name
 */
  void get_volume_name(char volnam[8],int tarray[6])
{
    int num,index;
    char c1;


    num = tarray[0];
    if(num < 10){
       volnam[0] = '0';
       c1 = num + 48;
       volnam[1] = c1;
    }
    else{
       c1 = num/10 + 48;  /*convert to char*/
       volnam[0] = c1;
       c1 = num%10 + 48;
       volnam[1] = c1;
    }

    num = tarray[1];
    if(num < 10){
       volnam[2] = '0';
       c1 = num + 48;
       volnam[3] = c1;
    }
    else{
       c1 = num/10 + 48; 
       volnam[2] = c1;
       c1 = num%10 + 48;
       volnam[3] = c1;
    }      
    
    num = tarray[2];
    if(num < 10){
       volnam[4] = '0';
       c1 = num + 48;
       volnam[5] = c1;
    }
    else{
       c1 = num/10 + 48; 
       volnam[4] = c1;
       c1 = num%10 + 48;
       volnam[5] = c1;
    }     
 
    num = tarray[3];
    if(num < 10){
       volnam[6] = '0';
       c1 = num + 48;
       volnam[7] = c1;
    }
    else{
       c1 = num/10 + 48; 
       volnam[6] = c1;
       c1 = num%10 + 48;
       volnam[7] = c1;
    } 

}/*end get_volume_name*/


 /********************************************************************************/
/*
 *Get netcdf general information.
 */
#if defined (IBMRISC) || defined (HP)
   void gcdfgeni(
#elif defined (CRAY)
   void GCDFGENI(
#else
   void gcdfgeni_(
#endif
                      int *cunit, char source[8], 
                      char proj_name[4],char volhead[8],
                      int varids[NC_MAX_VARS])
{

 int ncid,index;
 int ivalue,theid;
 size_t count,start;
 float fvalue;
 char  ctemp[8];

 ncid = *cunit;

 memset(source,' ',8);
 if(varids[SOURCE_INDEX] != -1)  nc_get_var_text(ncid,varids[SOURCE_INDEX],source);

 memset(proj_name,' ',4);
 if(varids[PROJ_INDEX] != -1){
    nc_get_var_text(ncid,varids[PROJ_INDEX],ctemp); 
    proj_name[0] = ctemp[0];
    proj_name[1] = ctemp[1];
    proj_name[2] = ctemp[2];
    proj_name[3] = ctemp[3];
 } 


 if(varids[VOL_HEAD_INDEX] == -1){
    memset(volhead,' ',8);  
    nc_get_var_text(ncid,varids[VOL_HEAD_INDEX],volhead);
 }
 else if(varids[RADAR_NAME_INDEX] == -1){
    memset(volhead,' ',8);  
    nc_get_var_text(ncid,varids[RADAR_NAME_INDEX],volhead); 
 }
 else{
     memset(volhead,' ',8);  
 }
 
}/*cdf_gen_info*/

/********************************************************************************/
/*
 *uswrp gridded file information.
 */
#if defined (IBMRISC) || defined (HP)
   void uswrpi(
#elif defined (CRAY)
   void USWRPI(
#else
   void uswrpi_(
#endif
                 int *cunit)
{

  int ncid,rcode,id,vcp;
  char ctemp[80];
  
  ncid = *cunit;
  rcode = nc_get_att_text(ncid,NC_GLOBAL,"title",ctemp);
  if(rcode == NC_NOERR) printf("TITLE        : %s\n",ctemp);

  rcode = nc_get_att_text(ncid,NC_GLOBAL,"project",ctemp);
  if(rcode == NC_NOERR) printf("PROJECT      : %s\n",ctemp);

  id = ncvarid(ncid,"VCP");
  if(rcode == NC_NOERR){
     rcode = nc_get_var_int(ncid,id,&vcp);  
            if(rcode == NC_NOERR) printf("WSR88D VCP   : %d\n",vcp);
  }

}/*uswrpi*/

/********************************************************************************/
#if defined (IBMRISC) || defined (HP)
   void cdflandi(
#elif defined (CRAY)
   void CDFLANDI(
#else
   void cdflandi_(
#endif
                   int *funit,int item[NID],
                   char namlnd[MAX_LAND][8],
                   int dim_sizes[NC_MAX_DIMS],
                   int varids[NC_MAX_VARS],
                   char radar_name[8],
                   float x[MAX_LAND],float y[MAX_LAND],
                   float z[MAX_LAND])
{

  int    ncid,rcode,index;
  int    numland,numradars;
  int    lndid,xid,yid,zid,nameid;
  long   lvalue;
  long   start[2],count[2];
  char   ctemp[8];
  double altv,lonv,latv;
  size_t astart = 0;
  size_t  vstart[2];
  ncid = *funit;

  if(dim_sizes[NUM_RADARS_INDEX] == -1) 
     item[302] = 1;
  else 
     item[302] = dim_sizes[NUM_RADARS_INDEX];


  if(varids[RADAR_NAME_INDEX] != -1){
        rcode = nc_get_var_text(ncid,varids[RADAR_NAME_INDEX],ctemp);
        if(rcode != NC_NOERR){
           printf("UNABLE TO GET RADAR NAME\n");
           nc_close(ncid);
           exit(0);
	}
        ctemp[4] = ' ';
        ctemp[5] = ' ';
        ctemp[6] = ' ';
        ctemp[7] = ' ';
        strncpy(radar_name,ctemp,8);
  }
  

  if(dim_sizes[NUM_LANDMARKS_INDEX] == -1){
     item[301] = 2;
     strncpy(namlnd[0],"ORIGIN ",8);
     strncpy(namlnd[1],ctemp,8);  
     rcode = nc_get_var1_double(ncid,varids[ALT_INDEX],0,&altv); 
     if(rcode != NC_NOERR){
          printf("UNABLE TO READ ALTITUDE FROM NETCDF FILE\n");
          nc_close(ncid);
          exit(-1);
     }     
     z[0] = 0;
     z[1] = altv;
     vstart[1] = 0;
     rcode = nc_get_var1_double(ncid,varids[LON_INDEX],vstart,&lonv); 
     if(rcode != NC_NOERR){
        printf("UNABLE TO READ RADAR LONGITUDE FROM NETCDF FILE\n");
         nc_close(ncid);
         exit(-1);
      }   
      x[1] = lonv;

      rcode = nc_get_var1_double(ncid,varids[LAT_INDEX],vstart,&latv); 
      if(rcode != NC_NOERR){
         printf("UNABLE TO READ LATITUDE FROM NETCDF FILE\n");
         nc_close(ncid);
         exit(-1);
      }
      y[1] = latv;

  }
  else{
      item[301] = dim_sizes[NUM_LANDMARKS_INDEX];
      for(index = 0; index < MAX_LAND; index++){
        x[index] = -1;
        y[index] = -1;
        z[index] = -1;
      }
      nc_get_var_float(ncid,varids[LANDMARK_X_INDEX],x);
      nc_get_var_float(ncid,varids[LANDMARK_Y_INDEX],y);
      nc_get_var_float(ncid,varids[LANDMARK_Z_INDEX],z);

      nameid = varids[LANDMARK_NAMES_INDEX];
      start[0] = 0;
      start[1] = 0;
      count[0] = dim_sizes[NUM_LANDMARKS_INDEX];
      count[1] = 8;
      ncvarget(ncid,nameid,start,count,namlnd);
  }
}/*cdf_landmark_info*/
     
 

 /********************************************************************************/

   /*static char * */
char *
type_name(nc_type type)
{
    switch (type) {
      case NC_BYTE:
	return "byte";
      case NC_CHAR:
	return "char";
      case NC_SHORT:
	return "short";
      case NC_INT:
	return "int";
      case NC_FLOAT:
	return "float";
      case NC_DOUBLE:
	return "double";
      default:
	printf("type_name: bad type %d", type);
	return "bogus";
    }
}
/****************************************************************/

/*The procedure get_data is called from SUBROUTINE FTCHCDF
 *in the file CEDCDF.f.  This subroutine returns an elevation
 *levels(nx*ny) worth of data in the array data_buf.
 */
#if defined (IBMRISC) || defined (HP)
  void cdfgdata(
#elif defined (CRAY)
  void CDFGDATA(
#else
  void cdfgdata_(
#endif
 
               int *funit,int *varid,float data_buf[MAXPLN],
               int *levels,int *numx,int *numy,int *format)  

{ 
     int    index,rcode,unit,id,data_index,numpoints;  
     int    num,ii,jj,lat,lon,fformat,swap;
     int    attachments,ival,length;
     double scale,dbias,fbad,rbad,dval,dbad[2],dscale; 
     float  temp,fval;
     char   ceddata[MAXPLN];  /*512 * 512*/
     char   cname[8];
     short  sval;
     static long start[4];
     static long count[4]; 
     int    get_Cfield_info();

  struct ncvar {			/* variable */
    char name[NC_MAX_NAME];
    nc_type type;
    int ndims;
    int natts;
    int dims[MAX_VAR_DIMS];
    boolean has_fillval;
    double fillval;
  };
   struct ncvar var;		/* variable */

struct ncatt {			/* attribute */
    int var;
    char name[NC_MAX_NAME];
    nc_type type;
    size_t len;
    char *string;		/* for text attributes (type = NC_CHAR) */
    double *vals;		/* for numeric attributes of all types */
};

 struct ncatt att;		/* attribute */
   /*----------------------------------------*/

     unit = *funit;
     id = *varid;
     dscale = 1;


     rcode = nc_inq_var(unit, id, var.name, &var.type, &var.ndims,
			     var.dims, &var.natts);
     if(rcode != NC_NOERR){
            printf("UNABLE TO GET FIELD VARIABLE ID's\n");
            exit(0);
     }

     attachments = var.natts;
     for(index = 0; index < attachments; index++){
         rcode = nc_inq_attname(unit, id, index, att.name);
         rcode = nc_inq_att(unit, id, att.name, &att.type, &att.len);
         if(rcode != NC_NOERR){
            printf("UNABLE TO READ FIELD SCALE,BIAS AND BAD FLAG INFORMATION\n");
            exit(-1);
         }     

        if(strncmp(att.name,"scale_factor",12) == 0){
           switch(att.type){
             case NC_DOUBLE:{
                        rcode = nc_get_att_double(unit,id,att.name,&dval);
                        dscale = dval;
                        break;
	     }
             case NC_FLOAT:{
                        rcode = nc_get_att_float(unit,id,att.name,&fval);
                        dscale = (double)fval;                
                        break;
	     }
             case NC_INT:{
                        rcode = nc_get_att_int(unit,id,att.name,&ival);
                        dscale = (double)ival;
                        break;
             }
             case NC_SHORT:{
                        rcode = nc_get_att_short(unit,id,att.name,&sval);
                        dscale = (double)sval;
                        break;
             }
             default:{
         	printf("type_name: bad type %d for field scale", att.type);
                exit(0);
            }
            }/*end switch*/
            if(rcode != NC_NOERR){
              printf("UNABLE TO GET FIELD SCALE FOR FIELD ID %d\n",id);
              nc_close(unit);
            }
         }/*end the strncmp if*/
         else if(strncmp(att.name,"add_offset",10) == 0){
           dbias = 0;
           switch(att.type){
                 case NC_DOUBLE:{
                        rcode = nc_get_att_double(unit,id,att.name,&dval);
                        dbias = dval;
                        break;
	         }
                 case NC_FLOAT:{
                        rcode = nc_get_att_float(unit,id,att.name,&fval);
                        dbias = (double)fval;                
                        break;
	         }
                 case NC_INT:{
                        rcode = nc_get_att_int(unit,id,att.name,&ival);
                        dbias = (double)ival;
                        break;
                 }
                 case NC_SHORT:{
                        rcode = nc_get_att_short(unit,id,att.name,&sval);
                        dbias = (double)sval;
                        break;
                 }
                 default:{
                        dbias = 0;
                 }
           }/*end switch*/
           if(rcode != NC_NOERR){
              printf("UNABLE TO GAET FIELD BIAS FOR FIELD ID %d\n",id);
              nc_close(unit);
              exit(-1);
           }
        }/*end else if for strncmp add_offset*/

        else if(strncmp(att.name,"missing_value",13) == 0){
            length = att.len;
	    att.vals = (double *) malloc(att.len * sizeof(double));
	    if (!att.vals) {
	        printf("Out of memory!");
	        nc_close(unit);
	        exit(-1);
	    }         
            rcode = nc_get_att_double(unit, id, att.name, att.vals );
            if(rcode != NC_NOERR){
               printf("UNABLE TO GET BAD DATA VALUE(S)FOR FIELD \n",id);
               nc_close(unit);
               exit(0);
	    }
            dbad[0] = -32768;
            dbad[1] = -32768;
            dbad[0] = att.vals[0];
            rbad =  dbad[0];
            if(length > 1){
               dbad[1] = att.vals[1];
               fbad =  dbad[1];
	    }
            free(att.vals);
       }/*end else for strncmp missing*/

    }/*for loop*/

     numpoints = *numx * *numy;
     if(*format == USWRPGD){
         start[0] = (long)*levels-1;
         start[1] = (long)0;
         start[2] = (long)0;
         count[0] = (long)1;
         count[1] = (long)*numy;
         count[2] = (long)*numx;
         rcode = ncvarget(unit,id,start,count,ceddata);
         if(rcode != NC_NOERR) {        
            printf("error reading in data from data file for level\n",*levels);
            exit(-1);
         }
         for(index = 0; index < numpoints; index++){
           temp = (float)ceddata[index];
	   if((temp == fbad) || (temp  == rbad)){
                data_buf[index] = -32768.0;
	   }
           else{
                data_buf[index] = (temp / dscale) + dbias;
	   }
	 }
     }
     else{
         start[0] = (long)0;
         start[1] = (long)*levels-1;
         start[2] = (long)0;
         start[3] = (long)0;
         count[0] = (long)1;
         count[1] = (long)1;
         count[2] = (long)*numy;
         count[3] = (long)*numx;
         rcode = ncvarget(unit,id,start,count,data_buf); 
         if(rcode !=  NC_NOERR){
            printf("error reading in data from data file for level\n",*levels);
            exit(-1);
         }

         for(index = 0; index < numpoints; index++){   
	   if((data_buf[index] == fbad) || (data_buf[index] == rbad)){
                      data_buf[index] = -32768.0;
	   }
           else{
                temp = data_buf[index];
                data_buf[index] = temp; 
	   }
	 }
     }

}/*cget_data*/
/***********************END OF NETCDF READING ROUTINES**********************/



 /********************************************************************************/
/*
 *Write out the Netcdf dimensions that go at the beginning of a Netcdf file.
 */
#if defined (IBMRISC) || defined (HP)
void wcdfdims(
#elif defined (CRAY)
void WCDFDIMS(
#else
void wcdfdims_(
#endif
               int *unit,float csp[3][3],int ncx[3],
               int *nfields,int *gsf,int *csf,
               int *num_radars,int *num_landmarks,
               int dimids[NC_MAX_DIMS],char gridtype[80],
               char filename[8])
{
  int ncid,rcode,nelevs,resolutions;
  int num_fields,ngrids,id,nradars;
  int xy,spacing,ls,dim[4],ss,bad,ele;
  int ds,dsid;
  char out_file_name[8];
  static char str[80];
  int start_time,end_time,start_date,end_date;
  int coord_sys,grid_type_id;
  size_t unlim;
  extern void output_coord_system();

  if(filename[6] == ' ') 
     filename[6] = '\0';
  else
     filename[7] = '\0';
  ncid = nccreate( filename, NC_CLOBBER );
  *unit = ncid;
  num_fields = *nfields;
  nradars = *num_radars;
  ls = 80;
  ss = 8;
  ds = 10;
  output_coord_system(&coord_sys);

  
  if(coord_sys == 1){
         xy = 1;
         ele = 0;
         strncpy(gridtype,"(x,y,copl)",10);
         gridtype[10] = '\0';
  }
  else if(coord_sys == 2){
         xy = 1;
         ele = 1;
         strncpy(gridtype,"(x,y,elev)",10);
         gridtype[10] = '\0';
  }
  else if(coord_sys == 0){
         xy = 1;
         ele = 0;
         strncpy(gridtype,"(x,y,z)",7);
         gridtype[7] = '\0';         
  }
  else if(coord_sys == 3){
         xy = 0;
         ele = 1;
         strncpy(gridtype,"(lon,lat,elev)",14);
         gridtype[14] = '\0';
  }
  else if(coord_sys == 4){
         xy = 0;
         ele = 0;
         strncpy(gridtype,"(lon,lat,z)",9);
         gridtype[19] = '\0';
  }
  else if(coord_sys == 5){
         xy = 1;
         ele = 0;
         strncpy(gridtype,"(PPI)",5);
         gridtype[5] = '\0';
  }
  else if(coord_sys == 6){
         xy = 1;
         ele = 0;
         strncpy(gridtype,"(RHI)",5);
         gridtype[5] = '\0';
  }
  else{
        printf("UNDEFINED COORDINATE SYSTEM IN write_netcdf_dimensions");
        exit(0);
  }

  

  rcode = nc_def_dim( ncid, "fields", num_fields,&id);
  dimids[NUM_FIELDS_INDEX] = id;
  rcode = nc_def_dim( ncid, "long_string", ls,&id);
  dimids[LONG_STRING_INDEX] = id;
  rcode = nc_def_dim( ncid, "short_string",ss,&id);
  dimids[SHORT_STRING_INDEX] = id;
  rcode = nc_def_dim( ncid, "date_string",ds,&id);
  dimids[DATE_STRING_INDEX] = id;
  rcode = nc_def_dim( ncid, "cedric_general_scaling_factor",*gsf,&id);
  rcode = nc_def_dim( ncid, "cedric_angle_scaling_factor",*csf,&id);
  rcode = nc_def_dim( ncid, "time",NC_UNLIMITED, &id);
  dimids[TIME_INDEX] = id;
  rcode = nc_def_dim( ncid, "number_radars",nradars,&id);
  dimids[NUM_RADARS_INDEX] = id;
  rcode = nc_def_dim( ncid, "landmarks",*num_landmarks, &id);
  dimids[NUM_LANDMARKS_INDEX] = id;

 /*
  *Write out the number of lat lon or x y grid points.
  */
  if(xy == 0){
      rcode = nc_def_dim( ncid, "longitude",ncx[0],&id);
      dimids[NUM_X_INDEX] = id;
      rcode = nc_def_dim( ncid, "latitude",ncx[1],&id);
      dimids[NUM_Y_INDEX] = id;
      if(ele == 1){
         rcode = nc_def_dim( ncid, "elevations",ncx[2],&id);
         dimids[NUM_LEVELS_DIM_INDEX] = id;
         rcode = nc_def_dim( ncid, "altitude",ncx[2],&id); 
         dimids[NUM_LEVELS_DIM_INDEX] = id;
      } 
  }
  else{
      rcode = nc_def_dim( ncid, "x",ncx[0],&id);
      dimids[NUM_X_INDEX] = id;
      rcode = nc_def_dim( ncid, "y",ncx[1],&id); 
      dimids[NUM_Y_INDEX] = id;
      rcode = nc_def_dim( ncid, "z",ncx[2],&id);
      dimids[NUM_LEVELS_DIM_INDEX] = id;
      rcode = nc_def_dim( ncid, "el",ncx[2],&id);
  }    
 

  ncendef(ncid);
}/*write_netcdf_dimensions*/
 /**************************************************************************/
#if defined (IBMRISC) || defined (HP)
   void cdfvids(
#elif defined (CRAY)
   void CDFVIDS(
#else
   void cdfvids_(
#endif
                  int *unit,int dimids[NC_MAX_DIMS],
                  int varids[NC_MAX_VARS])
{

  int ncid,rcode;
  int id,dim[5],coord_sys;
  char str[80];
  extern void output_coord_system();


  ncid = *unit;
  ncredef(ncid);
  dim[0] = dimids[SHORT_STRING_INDEX];

  rcode = nc_def_var( ncid,"radar_or_data_origin",NC_CHAR, 1, dim, &id);
  if(rcode != NC_NOERR) printf("UNABLE TO GET RADAR NAME ID\n");
  varids[RADAR_NAME_INDEX] = id;  


  rcode = nc_def_var( ncid, "project_name", NC_CHAR, 1, dim, &id);
  if(rcode != NC_NOERR) printf("UNABLE TO GET PROJ NAME ID\n");
  varids[PROJ_INDEX] = id;  

  rcode = nc_def_var( ncid, "scientist_name", NC_CHAR, 1, dim, &id);
  if(rcode != NC_NOERR) printf("UNABLE TO GET SCIENTIST NAME ID\n");
  varids[SCIENTIST_INDEX] = id;  


  rcode = nc_def_var( ncid, "submitters_name", NC_CHAR, 1, dim, &id);
  if(rcode != NC_NOERR) printf("UNABLE TO GET SUBMITTERS NAME ID\n");
  varids[SUBMITTERS_INDEX] = id;  


  rcode = nc_def_var( ncid, "data_source", NC_CHAR, 1, dim, &id);
  if(rcode != NC_NOERR) printf("UNABLE TO GET CEDRIC RUN DATE ID\n");
  varids[SOURCE_INDEX] = id;  


  rcode = nc_def_var( ncid, "volume_header", NC_CHAR, 1, dim, &id);
  if(rcode != NC_NOERR) printf("UNABLE TO GET CEDRIC RUN TIME ID\n");
  varids[VOL_HEAD_INDEX] = id; 

  /*
   *Date and time information.
   */
  dim[0] = dimids[DATE_STRING_INDEX]; 
  rcode = nc_def_var( ncid, "start_date", NC_CHAR, 1, dim,&id);
  varids[START_DATE_INDEX] = id; 

  rcode = nc_def_var( ncid, "end_date", NC_CHAR, 1, dim, &id);
  varids[END_DATE_INDEX] = id; 

  dim[0] = dimids[SHORT_STRING_INDEX]; 
  rcode = nc_def_var( ncid, "start_time", NC_CHAR, 1, dim, &id);
  varids[START_TIME_INDEX] = id; 

  rcode = nc_def_var( ncid, "end_time", NC_CHAR, 1, dim, &id);
  varids[END_TIME_INDEX] = id; 

  dim[0] = dimids[DATE_STRING_INDEX];   
  rcode = nc_def_var( ncid, "cedric_run_date", NC_CHAR, 1, dim, &id);
  if(rcode != NC_NOERR) printf("UNABLE TO GET CEDRIC RUN DATE ID\n");
  varids[CEDRIC_DATE_INDEX] = id;  

  dim[0] = dimids[SHORT_STRING_INDEX]; 
  rcode = nc_def_var( ncid, "cedric_run_time", NC_CHAR, 1, dim, &id);
  if(rcode != NC_NOERR) printf("UNABLE TO GET CEDRIC RUN TIME ID\n");
  varids[CEDRIC_TIME_INDEX] = id;  

  rcode = nc_def_var( ncid, "reference_Nyquist_vel", NC_FLOAT, 0, 0,&id);
  varids[REF_NYQUIST_INDEX] = id;
  dim[0] = dimids[NUM_LEVELS_DIM_INDEX];
  rcode = nc_def_var( ncid, "nyquist_velocities",NC_FLOAT,1,dim,&id);
  varids[NYQUIST_VELS_INDEX] = id;
  rcode = nc_def_var( ncid, "bad_data_flag",NC_INT,0,0,&id);
  varids[BAD_FLAG_INDEX] = id;
  dim[0] = dimids[TIME_INDEX];
  rcode = nc_def_var( ncid, "time_offset", NC_FLOAT, 1, dim, &id);
  varids[TIME_OFFSET_INDEX] = id;
  rcode = nc_def_var( ncid, "base_time", NC_INT, 0, 0, &id);
  varids[BASE_TIME_INDEX] = id;
  dim[0] = dimids[SHORT_STRING_INDEX];
  rcode = nc_def_var( ncid, "program", NC_CHAR,1,dim, &id);
  varids[PROGRAM_ID_INDEX] = id; 
  dim[0] = dimids[NUM_LANDMARKS_INDEX];
  rcode = nc_def_var( ncid, "landmark_x_coordinates", NC_FLOAT, 1, dim, &id);
  varids[LANDMARK_X_INDEX] = id;
  strcpy( str, "km" );
  rcode  = nc_put_att_text( ncid, id, "units", strlen( str ) +1, str );
  rcode = nc_def_var( ncid, "landmark_y_coordinates", NC_FLOAT, 1, dim, &id);
  rcode  = nc_put_att_text( ncid, id, "units", strlen( str ) +1, str );
  varids[LANDMARK_Y_INDEX] = id;
  rcode = nc_def_var( ncid, "landmark_z_coordinates", NC_FLOAT, 1, dim, &id);
  strcpy(str,"meters");
  rcode  = nc_put_att_text( ncid,id,"units", strlen( str ) +1, str );
  varids[LANDMARK_Z_INDEX] = id;
  dim[1] = dimids[SHORT_STRING_INDEX]; /*The short string id*/
  rcode = nc_def_var( ncid, "landmark_names", NC_CHAR, 2, dim, &id);
  varids[LANDMARK_NAMES_INDEX] = id;
  dim[0] = dimids[LONG_STRING_INDEX];
  rcode = nc_def_var( ncid, "grid_type", NC_CHAR,1,dim, &id);
  varids[GRID_TYPE_INDEX] = id;


  output_coord_system(&coord_sys);
  if(coord_sys == 3 || coord_sys == 4){/* lat lon coordinate system*/
       rcode = nc_def_var( ncid, "radar_latitude", NC_DOUBLE, 0, 0, &id );
       varids[LAT_INDEX] = id;
       rcode = nc_def_var( ncid, "radar_longitude", NC_DOUBLE, 0, 0, &id );
       varids[LON_INDEX]  = id;
       rcode = nc_def_var( ncid, "radar_altitude", NC_DOUBLE, 0, 0, &id );
       varids[ALT_INDEX] = id;
       dim[0] = dimids[NUM_Y_INDEX]; 
       rcode = nc_def_var( ncid, "latitude", NC_DOUBLE, 1, dim, &id );
       varids[ZEBRA_Y_INDEX] = id;
       dim[0] = dimids[NUM_X_INDEX]; 
       rcode = nc_def_var( ncid, "longitude", NC_DOUBLE, 1, dim, &id );
       varids[ZEBRA_X_INDEX]  = id;
       dim[0] = dimids[NUM_LEVELS_DIM_INDEX]; 
       rcode = nc_def_var( ncid, "altitude", NC_DOUBLE, 1, dim, &id );
       varids[ZEBRA_Z_INDEX] = id;

       rcode = nc_def_var( ncid, "x_spacing", NC_FLOAT, 0, 0, &id);
       strcpy(str,"degrees");
       rcode  = nc_put_att_text( ncid,id,"units", strlen( str ) +1, str );
       varids[X_SPACING_INDEX] = id;
       rcode = nc_def_var( ncid, "y_spacing", NC_FLOAT, 0, 0, &id);
       strcpy(str,"degrees");
       rcode  = nc_put_att_text( ncid,id,"units", strlen( str ) +1, str );
       varids[Y_SPACING_INDEX] = id;
       rcode = nc_def_var( ncid, "z_spacing", NC_FLOAT, 0, 0, &id );
       varids[Z_SPACING_INDEX] = id;

       rcode = nc_def_var( ncid, "x_min", NC_FLOAT, 0, 0, &id );
       strcpy(str,"The minimum x grid value");
       rcode  = nc_put_att_text( ncid,id,"value", strlen( str ) +1, str );
       strcpy(str,"deg of longitude");
       rcode  = nc_put_att_text( ncid,id,"units", strlen( str ) +1, str );
       varids[X_MIN_INDEX] = id;

       rcode = nc_def_var( ncid, "y_min", NC_FLOAT, 0, 0, &id );
       strcpy(str,"The minimum y grid value");
       rcode  = nc_put_att_text( ncid,id,"value", strlen( str ) +1, str );
       strcpy(str,"deg of latitude");
       rcode  = nc_put_att_text( ncid,id,
                               "units", strlen( str ) +1, str );
       varids[Y_MIN_INDEX] = id;

       rcode = nc_def_var( ncid, "x_max", NC_FLOAT, 0, 0, &id );
       strcpy(str,"The maximum x grid value");
       rcode  = nc_put_att_text( ncid,id,"value", strlen( str ) +1, str );
       strcpy(str,"deg of longitude");
       rcode  = nc_put_att_text( ncid,id,
                               "units", strlen( str ) +1, str );
       varids[X_MAX_INDEX] = id;


       rcode = nc_def_var( ncid, "y_max", NC_FLOAT, 0, 0, &id );
       strcpy(str,"The maximum y grid value");
       rcode  = nc_put_att_text( ncid,id,"value", strlen( str ) +1, str );
       strcpy(str,"deg of latitude");
       rcode  = nc_put_att_text( ncid,id,
                               "units", strlen( str ) +1, str );
       varids[Y_MAX_INDEX] = id;
       
  }
  else{/*Cartesian coordinate system*/
       rcode = nc_def_var( ncid, "radar_latitude", NC_DOUBLE, 0, 0, &id );
       varids[LAT_INDEX] = id;
       rcode = nc_def_var( ncid, "radar_longitude", NC_DOUBLE, 0, 0, &id );
       varids[LON_INDEX]  = id;
       rcode = nc_def_var( ncid, "radar_altitude", NC_DOUBLE, 0, 0, &id );
       varids[ALT_INDEX] = id;
       rcode = nc_def_var( ncid, "lat", NC_FLOAT, 0, 0, &id );
       varids[GRID_LAT_INDEX] = id;
       rcode = nc_def_var( ncid, "lon", NC_FLOAT, 0, 0, &id );
       varids[GRID_LON_INDEX]  = id;
       
       rcode = nc_def_var( ncid, "alt", NC_DOUBLE, 0, 0, &id ); 
       strcpy(str,"Meters");
       rcode  = nc_put_att_text( ncid,id,"units", strlen( str ) +1, str );
       varids[ZEBRA_Z_INDEX] = id;

       rcode = nc_def_var( ncid, "x_min", NC_FLOAT, 0, 0, &id );
       strcpy(str,"The minimum x grid value");
       rcode  = nc_put_att_text( ncid,id,"value", strlen( str ) +1, str );
       strcpy(str,"km");
       rcode  = nc_put_att_text( ncid,id,"units", strlen( str ) +1, str );
       varids[X_MIN_INDEX] = id;

       rcode = nc_def_var( ncid, "y_min", NC_FLOAT, 0, 0, &id );
       strcpy(str,"The minimum y grid value");
       rcode  = nc_put_att_text( ncid,id,"value", strlen( str ) +1, str );
       strcpy(str,"km");
       rcode  = nc_put_att_text( ncid,id,
                               "units", strlen( str ) +1, str );
       varids[Y_MIN_INDEX] = id;

       rcode = nc_def_var( ncid, "x_max", NC_FLOAT, 0, 0, &id );
       strcpy(str,"The maximum x grid value");
       rcode  = nc_put_att_text( ncid,id,"value", strlen( str ) +1, str );
       strcpy(str,"km");
       rcode  = nc_put_att_text( ncid,id,
                               "units", strlen( str ) +1, str );
       varids[X_MAX_INDEX] = id;


       rcode = nc_def_var( ncid, "y_max", NC_FLOAT, 0, 0, &id );
       strcpy(str,"The maximum y grid value");
       rcode  = nc_put_att_text( ncid,id,"value", strlen( str ) +1, str );
       strcpy(str,"km");
       rcode  = nc_put_att_text( ncid,id,
                               "units", strlen( str ) +1, str );
       varids[Y_MAX_INDEX] = id;

       rcode = nc_def_var( ncid, "x_spacing", NC_FLOAT, 0, 0, &id);
       strcpy(str,"km");
       rcode  = nc_put_att_text( ncid, id,"units", strlen( str ) +1, str );
       varids[X_SPACING_INDEX] = id;

       rcode = nc_def_var( ncid, "y_spacing", NC_FLOAT, 0, 0, &id);
       rcode  = nc_put_att_text( ncid,id,"units", strlen( str ) +1, str );
       varids[Y_SPACING_INDEX] = id;

       dim[0] = dimids[NUM_X_INDEX ];
       rcode = nc_def_var( ncid, "x", NC_FLOAT, 1, dim, &id );
       varids[ZEBRA_X_INDEX] = id;     
       dim[0] = dimids[NUM_Y_INDEX ];
       rcode = nc_def_var( ncid, "y", NC_FLOAT, 1, dim, &id );
       varids[ZEBRA_Y_INDEX] = id;     

  
       dim[0] = dimids[NUM_LEVELS_DIM_INDEX];
       rcode = nc_def_var( ncid, "z", NC_FLOAT, 1, dim, &id );
       varids[ZEBRA_Z_INDEX] = id;
       rcode = nc_def_var( ncid, "el", NC_FLOAT, 1, dim, &id );
       varids[NUM_LEVELS_VAR_INDEX] = id;
       rcode = nc_def_var( ncid, "z_spacing", NC_FLOAT, 0, 0, &id );
       varids[Z_SPACING_INDEX] = id;
  }   
  ncendef(ncid);
  return;
}/*CDFVIDS*/
/********************************************************************/
/* 
 *Write out date and time information to Netcdf output file.
 */
#if defined (IBMRISC) || defined (HP)
   void wcdfdtti(
#elif defined (CRAY)
   void WCDFDTTI(
#else
   void wcdfdtti_(
#endif
                  int *unit,int temp[400],int varids[NC_MAX_VARS])
{
char str[4],dstr[10],tstr[8],gmstr[10];
int  size,number,index,rcode;
int ncid;
struct tm *t;
time_t local;
size_t the_size;
void convert_to_char();


 /*
  *Beginning date. 
  */
  ncid = *unit;

  number = temp[116]; /*start month*/
  convert_to_char(number,str);
  dstr[0] = str[0];
  dstr[1] = str[1];
  dstr[2] ='/';
  number = temp[117]; /*start day*/
  convert_to_char(number,str);
  dstr[3] = str[0];
  dstr[4] = str[1];
  dstr[5] ='/';
  number = temp[115]; /*start year*/
  convert_to_char(number,str);
  dstr[6] = str[0];
  dstr[7] = str[1];
  dstr[8] = str[2];
  dstr[9] = str[3];
  rcode = nc_put_var_text( ncid, varids[START_DATE_INDEX] , dstr);
  if(rcode != NC_NOERR){
     printf("UNABLE TO WRITE BEGINNING DATE\n");
     nc_close(ncid);
     exit(-1);
  }

 /*
  *Ending date.
  */
  number = temp[122]; /*end month*/
  convert_to_char(number,str);
  dstr[0] = str[0];
  dstr[1] = str[1];
  dstr[2] ='/';
  number = temp[123]; /*end day*/
  convert_to_char(number,str);
  dstr[3] = str[0];
  dstr[4] = str[1];
  dstr[5] ='/';
  number = temp[121]; /*end year*/
  convert_to_char(number,str);
  dstr[6] = str[0];
  dstr[7] = str[1];
  dstr[8] = str[2];
  dstr[9] = str[3];
  rcode = nc_put_var_text( ncid, varids[END_DATE_INDEX] , dstr);
  if(rcode != NC_NOERR){
     printf("UNABLE TO WRITE END DATE OF DATA\n");
     nc_close(ncid);
     exit(-1);
  }


  /*
   *Start time.
   */
  number = temp[118];
  convert_to_char(number,str);
  tstr[0] = str[0];
  tstr[1] = str[1];
  tstr[2] =':'; 
  number = temp[119];
  convert_to_char(number,str);
  tstr[3] = str[0];
  tstr[4] = str[1];
  tstr[5] =':';  
  number = temp[120]; 
  convert_to_char(number,str);
  tstr[6] = str[0];
  tstr[7] = str[1];
  rcode = nc_put_var_text( ncid, varids[START_TIME_INDEX] , tstr);
  if(rcode != NC_NOERR){
     printf("UNABLE TO WRITE START TIME OF DATA\n");
     nc_close(ncid);
     exit(-1);
  }

  /*
   *End time.
   */
  number = temp[124];
  convert_to_char(number,str);
  tstr[0] = str[0];
  tstr[1] = str[1];
  tstr[2] =':'; 
  number = temp[125];
  convert_to_char(number,str);
  tstr[3] = str[0];
  tstr[4] = str[1];
  tstr[5] =':';  
  number = temp[126]; 
  convert_to_char(number,str);
  tstr[6] = str[0];
  tstr[7] = str[1];
  rcode = nc_put_var_text( ncid, varids[END_TIME_INDEX] , tstr);
  if(rcode != NC_NOERR){
     printf("UNABLE TO WRITE END TIME OF DATA\n");
     nc_close(ncid);
     exit(-1);
  }


  /*
   *Cedric program run date and time.
   */
   local = time(NULL);
   t  = localtime(&local);
   the_size = 9;
   strftime(gmstr,the_size,"%m%d%Y",t);
   dstr[0] = gmstr[0];
   dstr[1] = gmstr[1];
   dstr[2] = '/';
   dstr[3] = gmstr[2];
   dstr[4] = gmstr[3];
   dstr[5] = '/';
   dstr[6] = gmstr[4];
   dstr[7] = gmstr[5];
   dstr[8] = gmstr[6];
   dstr[9] = gmstr[7];   
   rcode = nc_put_var_text( ncid, varids[CEDRIC_DATE_INDEX ] , dstr);
   if(rcode != NC_NOERR){
     printf("UNABLE TO CEDRIC PROGRAM RUN DATE\n");
     nc_close(ncid);
     exit(-1);
   }


   strftime(gmstr,the_size,"%H%M%S",t);
   tstr[0] = gmstr[0];
   tstr[1] = gmstr[1];
   tstr[2] = ':';
   tstr[3] = gmstr[2];
   tstr[4] = gmstr[3];
   tstr[5] = ':';
   tstr[6] = gmstr[4];
   tstr[7] = gmstr[5];
   rcode = nc_put_var_text( ncid, varids[CEDRIC_TIME_INDEX ] , tstr);
   if(rcode != NC_NOERR){
     printf("UNABLE TO CEDRIC PROGRAM RUN TIME\n");
     nc_close(ncid);
     exit(-1);
   }

}/*cdf_out_date_time*/

 /**************************************************************************/
/*
 *Write out Netcdf grid information.
 *
 * NOTE: Zebra needs.
 *       lle - Zebra needs an altitude field.  In this case an elevation and 
 *       an altitude variable are defined.  These variables will have exactly
 *       the same values in them.  Zebra expects the variable altitude.
 *       For Zebra we will have 
 *       latitude  - an array of latitude grid values.
 *       longitude - an array of longitude grid values.
 *       radar_lat - the actual latitude of the radar.
 *       radar_lon - the actual longitude of the radar.
 *       altitude  - the elevation values.
 *       elevation - the elevation values.
 *
 *       xyz - Zebra needs
 *       lat  - the actual latitude of the radar.
 *       lon  - the actual longitude of the radar.
 *       x,y,z grid spacing values
 *       z    - array of z or(altitude) values.
 *       x,y min and max grid values.
 */
#if defined (IBMRISC) || defined (HP)
  void wcdfgrdi()
#elif defined (CRAY)
  void WCDFGRDI(
#else
  void wcdfgrdi_(
#endif
                 int *unit,int temp[400],
                 float vallev[MAXZLEV],
                 int dimids[NC_MAX_DIMS],
                 int varids[NC_MAX_VARS],
                 char gridtype[80])

{
  int rcode,dim[4],index;
  float  fvalue,xvals[MAXX],yvals[MAXX];
  float  hspacing,vspacing,zspacing,scale;
  int    coord_sys;
  int    neg,len;
  int    numx,numy,mult;
  int ivalue,ele,ncid,itemp;
  int lindex,thediv;
  char str[80],str1[2],str2[3];
  double lat,lon,alt,dvalue;
  size_t start[5],count[5];
  extern void output_coord_system();
  extern void get_latlonalt();

  int lat_id,lon_id,alt_id,elev_id,ispace;
  int horizsp_id,vertsp_id,grid_type_id;
  int themod;

  ncid = *unit;

  scale = (float)temp[67];
 /*
  *Write out the radar latitude and longitude and altitude.
  */
  get_latlonalt(&lat,&lon,&alt);
  if(lat == -32768.){
     neg = 0;
     lat = (double)(temp[32] + temp[33]/60. + temp[34]/(64.*3600.));
     lat = atof(str);
     if(temp[35] < 0.0){
        temp[35] = - temp[35];
        neg = 1;
     }
     if(temp[36] < 0.0) temp[36] = - temp[36];
     if(temp[37] < 0.0) temp[37] = - temp[37];
     lon = (double)(temp[35] + temp[36]/60. + temp[37]/(64.*3600.));
     if(neg == 1) lon = - lon;
     alt = (double)temp[316];
  }
  rcode = nc_put_var_double( ncid, varids[LAT_INDEX], &lat );
  rcode = nc_put_var_double( ncid, varids[LON_INDEX], &lon );
  rcode = nc_put_var_double( ncid, varids[ALT_INDEX], &alt );
  rcode = nc_put_var_text(ncid,varids[GRID_TYPE_INDEX] ,gridtype);  
  if(rcode != NC_NOERR){
     printf("UNABLE TO WRITE GRID TYPE TO NETCDF FILE\n");
     exit(0);
  }

  /*
   *These are grid values they are used in the Cedric 510 word header to 
   *define the grid.
   */
  hspacing = temp[162]/1000.;
  vspacing = temp[167]/1000.;
  numx     = temp[161];
  numy     = temp[166];

  fvalue = (float)temp[159]/scale;
  rcode = nc_put_var_float( ncid, varids[X_MIN_INDEX], &fvalue);
  fvalue = (float)temp[160]/scale;
  rcode = nc_put_var_float( ncid, varids[X_MAX_INDEX], &fvalue);     
  fvalue = (float)temp[164]/scale;
  rcode = nc_put_var_float( ncid, varids[Y_MIN_INDEX], &fvalue);
  fvalue = (float)temp[165]/scale;
  rcode = nc_put_var_float( ncid, varids[Y_MAX_INDEX], &fvalue);

  rcode  = nc_put_var_float( ncid, varids[X_SPACING_INDEX], &hspacing);
  if(rcode != NC_NOERR){
     printf("UNABLE TO WRITE OUT X SPACING\n");
     exit(0);
  }   

  fvalue = (float)temp[170]/10.;
  rcode  = nc_put_var_float( ncid, varids[Y_SPACING_INDEX], &vspacing);
  if(rcode != NC_NOERR){
       printf("UNABLE TO WRITE OUT Y SPACING\n");
       exit(0);
  }

  fvalue = (float)temp[172]/1000.;
  rcode  = nc_put_var_float( ncid, varids[Z_SPACING_INDEX], &fvalue);
  if(rcode != NC_NOERR){
       printf("UNABLE TO WRITE OUT Z SPACING\n");
       exit(0);
    }



  /*
   *These are for Zebra.  They are not used by Cedric.
   */

    fvalue = (float)temp[159]/100.;
    for(index = 0; index < numx; index++){
        if(index > 0) fvalue = fvalue + hspacing;
        sprintf(str,"%.2f",fvalue);
        xvals[index] =  atof(str);
    }
    start[0] = 0;
    count[0] = numx;
    rcode = nc_put_vara_float( ncid,varids[ZEBRA_X_INDEX],start,count,xvals);


    fvalue = temp[164]/100.;
    for(index = 0; index < numy; index++){
        sprintf(str,"%.2f",fvalue);
        yvals[index] =  atof(str);
        fvalue = fvalue + vspacing;
    }
    start[0] = 0;
    count[0] = numy; 
    rcode = nc_put_vara_float( ncid,varids[ZEBRA_Y_INDEX],start,count,yvals); 

  vallev[0] = temp[169]/1000.;
  for(index = 1; index < temp[171]; index++){
      vallev[index] = vallev[index-1] + temp[172]/1000.;
  }
  output_coord_system(&coord_sys);
  if(coord_sys == LLE || coord_sys == LLZ){/*a lat lon grid*/
  /*
   *These are for Zebra.  They are latitude and longitude of the grid.
   *They are not used by Cedric.
   */
   
    start[0] = 0;
    count[0] = temp[171];    
    rcode = nc_put_vara_float( ncid,varids[ZEBRA_Z_INDEX],start,count,vallev);   
    if(rcode != NC_NOERR){
       printf("UNABLE TO WRITE OUT ALTITUDE ARRAY FOR ZEBRA\n");
       exit(0);
    }  
  }

  else{
    start[0] = 0;
    count[0] = temp[171];    
    rcode = nc_put_vara_float( ncid,varids[ZEBRA_Z_INDEX],start,count,vallev);   
    if(rcode != NC_NOERR){
       printf("UNABLE TO WRITE OUT Z LEVELS ARRRAY\n");
       exit(0);
    }  


    /*
     *These are for Zebra they are the X and Y grid values and an elevation array.
     */
    rcode = nc_put_vara_float( ncid,varids[NUM_LEVELS_VAR_INDEX],start,count,vallev);   
    if(rcode != NC_NOERR){
       printf("UNABLE TO WRITE OUT ALTITUDE ARRAY FOR ZEBRA\n");
       exit(0);
    }  
    /*
     *These are for Zebra.
     */ 
    fvalue = (float)lat;
    rcode = nc_put_var_float( ncid, varids[GRID_LAT_INDEX], &fvalue );
    fvalue = (float)lon;
    rcode = nc_put_var_float( ncid, varids[GRID_LON_INDEX], &fvalue );
    
  }/*else*/



}/*cdf_out_grid_info*/
/**************************************************************************/
/*
 *Write out the Netcdf field information to ouput file.
 */
#if defined (IBMRISC) || defined (HP)
   void wcdffldi(
#elif defined (CRAY)
   void WCDFFLDI(
#else
   void wcdffldi_(
#endif
                  int *unit, char fldnm[25][8],int *num_fields,
                  int scale[25],int dimids[NC_MAX_DIMS],int fvars[NFMAX])
{
int    ncid;
int    dim[4],index,rcode,str_index;
int    field_id,fld_data_id[NFMAX],length[NFMAX],clength;
int    bads[2],id,format;
float  fvalue;
double dscale,dbias,dbad1,dbad2;
char   str[200]; /*200 = MAX FIELDS * 8*/
char   name[8],units[80];
static char two[3],three[4],four[5],five[6],six[7],seven[8],eight[9],one[2];
size_t start[5],count[5];


 ncid = *unit;
 ncredef(ncid);

/*
 *Field names.
 */
  dim[0] = dimids[NUM_FIELDS_INDEX];
  dim[1] = dimids[SHORT_STRING_INDEX];
  rcode = nc_def_var(ncid, "field_names", NC_CHAR, 2, dim, &field_id);
  if(rcode != NC_NOERR){
     printf("UNABLE TO GET FIELD NAMES VARIABLE ID\n");
     exit(0);
  } 
  /*
   *Write out the field names.
   */
  ncendef(ncid );

  str_index = 0;
  for(index = 0; index < *num_fields; index++){
     str[str_index] = fldnm[index][0];
     str_index++;
     str[str_index] = fldnm[index][1];
     str_index++;
     str[str_index] = fldnm[index][2];    
     str_index++;
     str[str_index] = fldnm[index][3]; 
     str_index++;
     str[str_index] = fldnm[index][4]; 
     str_index++;
     str[str_index] = fldnm[index][5]; 
     str_index++;
     str[str_index] = fldnm[index][6];
     str_index++;
     str[str_index] = fldnm[index][7];  
     str_index++;
  }
    str[str_index] = '\0';
    rcode = nc_put_var_text(ncid, field_id,str);
    if(rcode != NC_NOERR){
        printf("UNABLE TO WRITE FIELD NAMES\n");
        exit(0);
    }


  ncredef(ncid);
  dim[0] = dimids[TIME_INDEX]; /*time*/
  dim[1] = dimids[NUM_LEVELS_DIM_INDEX]; /*elevations or altitudes*/
  dim[2] = dimids[NUM_Y_INDEX]; /*lat or y*/
  dim[3] = dimids[NUM_X_INDEX]; /*lon or x*/


  for(index = 0; index < *num_fields; index++){
     clength = 0;
     while(fldnm[index][clength] != ' ' && fldnm[index][clength] != '\0'){
          clength++;
     }
     switch(clength){
        case 1 :{
                 one[0] = fldnm[index][0];
                 one[1] = '\0';
                 rcode = nc_def_var(ncid,one,NC_FLOAT,4,dim,&fld_data_id[index]);
                 length[index] = 1;
                 break;
	}
        case 2 :{
                 two[0] = fldnm[index][0];
                 two[1] = fldnm[index][1];
                 two[2] = '\0';
                 rcode = nc_def_var(ncid,two,NC_FLOAT,4,dim,&fld_data_id[index]);
                 length[index] = 2;
                 break;
	}
        case 3:{
                length[index] = 3;
                three[0] = fldnm[index][0];
                three[1] = fldnm[index][1];
                three[2] = fldnm[index][2];
                three[3] = '\0';
                rcode = nc_def_var(ncid,three,NC_FLOAT,4,dim,&fld_data_id[index]);
                break; 
	}
        case 4:{
                four[0] = fldnm[index][0];
                four[1] = fldnm[index][1];
                four[2] = fldnm[index][2];
                four[3] = fldnm[index][3];   
                four[4] = '\0';
                length[index] = 4;            
                rcode = nc_def_var(ncid,four,NC_FLOAT,4,dim,&fld_data_id[index]);
                break; 
	}
        case 5:{
                five[0] = fldnm[index][0];
                five[1] = fldnm[index][1];
                five[2] = fldnm[index][2];
                five[3] = fldnm[index][3];
                five[4] = fldnm[index][4]; 
                length[index] = 5;
                five[5] = '\0';                
                rcode = nc_def_var(ncid,five,NC_FLOAT,4,dim,&fld_data_id[index]);
                break; 
	}
        case 6:{
                six[0] = fldnm[index][0];
                six[1] = fldnm[index][1];
                six[2] = fldnm[index][2];
                six[3] = fldnm[index][3];
                six[4] = fldnm[index][4]; 
                six[5] = fldnm[index][5];    
                six[6] =  '\0';   
                rcode = nc_def_var(ncid,six,NC_FLOAT,4,dim,&fld_data_id[index]);
                break; 
	}
        case 7:{
                seven[0] = fldnm[index][0];
                seven[1] = fldnm[index][1];
                seven[2] = fldnm[index][2];
                seven[3] = fldnm[index][3];
                seven[4] = fldnm[index][4]; 
                seven[5] = fldnm[index][5];  
                seven[6] = fldnm[index][6]; 
                seven[7] = '\0';
                length[index] = 7;               
                rcode = nc_def_var(ncid,seven,NC_FLOAT,4,dim,&fld_data_id[index]);
                break; 
	}
        case 8:{
                eight[0] = fldnm[index][0];
                eight[1] = fldnm[index][1];
                eight[2] = fldnm[index][2];
                eight[3] = fldnm[index][3];
                eight[4] = fldnm[index][4]; 
                eight[5] = fldnm[index][5];  
                eight[6] = fldnm[index][6];  
                eight[7] = fldnm[index][7];  
                length[index] = 8;               
                rcode = nc_def_var(ncid,eight,NC_FLOAT,4,dim,&fld_data_id[index]);
                break; 
	}
     default:{
               printf("FIELD %s NAME IS LONGER THAN 8 CHARACTERS\n",fldnm[index]);
               exit(0);
	}
                  
     }/*Switch*/

     fvars[index] = fld_data_id[index];
        dscale = (float)scale[index];
        dbias  = 0.0;
        dbad1  = -32768;
        dbad2  = -32768;

     if(format == MDVFMT){
        dscale = 1.0;
        dbias  = 0.0;
        dbad1  = -32768;
        dbad2  = -32768;        
     }
     fvalue = (float)dscale;
     rcode  = nc_put_att_float(ncid, fld_data_id[index]
                                , "scale_factor",NC_FLOAT, 1,&fvalue); 
     fvalue = dbias;
     rcode  = nc_put_att_float(ncid, fld_data_id[index]
                         , "add_offset",NC_FLOAT, 1, &fvalue);

     bads[0] = -32768;
     bads[1] = -32768;
     rcode  = nc_put_att_int(ncid, fld_data_id[index]
                         , "missing_value",NC_INT,2,bads);

  }/*end of for loop*/
  ncendef(ncid );
}/*cdf_out_field_info*/
 /**************************************************************************/
/*
 *Write Netcdf Landmark information to output file.
 */
#if defined (IBMRISC) || defined (HP)
  void wcdfldmk(
#elif defined (CRAY)
  void WCDFLDMK(
#else
  void wcdfldmk_(
#endif
                            int *unit,int *num_landmarks,
                            char namlnd[MAX_LAND][8],
                            float x[MAX_LAND],float y[MAX_LAND], 
                            float z[MAX_LAND],
                            int varids[NC_MAX_VARS])
{
  int rcode,index,dim[4],str_index;
  int landmark_id,name_id,xid,yid,zid;
  double lat,lon;
  int ncid;
  char   str[120]; /*120 = MAX LAND MARKS  * 8*/

  ncid = *unit;

  str_index = 0;
  for(index = 0; index < *num_landmarks; index++){
     str[str_index] = namlnd[index][0];
     str_index++;
     str[str_index] = namlnd[index][1];
     str_index++;
     str[str_index] = namlnd[index][2];    
     str_index++;
     str[str_index] = namlnd[index][3]; 
     str_index++;
     str[str_index] = namlnd[index][4]; 
     str_index++;
     str[str_index] = namlnd[index][5]; 
     str_index++;
     str[str_index] = ' ';
     str_index++;
     str[str_index] = '\0';  
     str_index++;
  }

  rcode = nc_put_var_text( ncid,varids[LANDMARK_NAMES_INDEX], str);
  rcode = nc_put_var_float( ncid,varids[LANDMARK_X_INDEX],x);
  rcode = nc_put_var_float( ncid,varids[LANDMARK_Y_INDEX],y);
  rcode = nc_put_var_float( ncid,varids[LANDMARK_Z_INDEX],z);



}/*cdf_out_landmark_info*/

 /**************************************************************************/
#if defined (IBMRISC) || defined (HP)
   void wcdftext(
#elif defined (CRAY)
   void WCDFTEXT(
#else
   void wcdftext_(
#endif
                  int *unit,int varids[NC_MAX_VARS],
                  short *one, short *two,
                  short *three,short *four,int *num_char,
                  int *var)
{

  int ncid,rcode,id,index;
  int first,second,swap;
  char str[8],temp[8];

  ncid = *unit;
  if(*num_char == 4){
     first = *one/256;
     second = *one%256;
     str[0] = (char)first;
     str[1] = (char)second;
     first = *two/256;
     second = *two%256;
     str[2] = (char)first;
     str[3] = (char)second;
     str[4] = ' ';
     str[5] = ' ';
     str[6] = ' ';
     str[7] = ' ';
  }

  if(*num_char == 6){
     first = *one/256;
     second = *one%256;
     str[0] = (char)first;
     str[1] = (char)second;
     first = *two/256;
     second = *two%256;
     str[2] = (char)first;
     str[3] = (char)second;
     first = *three/256;
     second = *three%256;
     str[4] = (char)first;
     str[5] = (char)second;
     str[6] = ' ';
     str[7] = ' ';
  }

  if(*num_char == 8){
     first = *one/256;
     second = *one%256;
     str[0] = (char)first;
     str[1] = (char)second;
     first = *two/256;
     second = *two%256;
     str[2] = (char)first;
     str[3] = (char)second;
     first = *three/256;
     second = *three%256;
     str[4] = (char)first;
     str[5] = (char)second;
     first = *four/256;
     second = *four%256;
     str[6] = (char)first;
     str[7] = (char)second;
  }


  if(*var == 1) id = varids[RADAR_NAME_INDEX];
  else if(*var == 2) id = varids[PROJ_INDEX];
  else if(*var == 3) id = varids[SCIENTIST_INDEX];
  else if(*var == 4) id = varids[SUBMITTERS_INDEX];
  else if(*var == 5) id = varids[SOURCE_INDEX];
  else if(*var == 6) id = varids[VOL_HEAD_INDEX];


  swap = 0;
#if defined (DEC)
    swap = 1;
#elif defined (LINUX)
    swap = 1;
#endif

  if(swap == 1){
     temp[0] = str[1];
     temp[1] = str[0];
     temp[2] = str[3];
     temp[3] = str[2];
     temp[4] = str[5];
     temp[5] = str[4];
     temp[6] = str[7];
     temp[7] = str[8];
     for(index = 0; index < 7; index++) str[index] = temp[index];
   }


  rcode = nc_put_var_text( ncid, id , str);
  if(rcode != NC_NOERR){
     printf("UNABLE TO WRITE TEXT VARIABLE NUMBER  %d TO OUTPUT FILE\n",*var);
     return;
  }

}/*wcdftext*/


 /**************************************************************************/
#if defined (IBMRISC) || defined (HP)
   void wcdfmisc(
#elif defined (CRAY)
   void WCDFMISC(
#else
   void wcdfmisc_(
#endif
                  int *unit,int temp[400],int dimids[NC_MAX_DIMS],
                  int varids[NC_MAX_VARS])
{

int field_id,lev,juldat;
int radar_id,rnyquist_id,nyqvels_id;
int rcode,rank,gridded,index,str_index;
int dim[4],num_chars,timeoff_id,baset_id;
int ivar,bad,ncid,ibtime;
struct tm t;
time_t btime;
double dvalue;
int    tarray[9];
float  fvalue,vels[MXCRT];
float  spacing,scale;
char   grid[4],junk[2];
char   cfldnm[8],program[6],char_scale[3];
char   str[200]; /*200 = MAX FIELDS * 8*/
size_t start[5], count[5];
extern int get_nyquist_velocities();

  ncid = *unit;
  scale = (float)temp[67];


  bad = -32768;
  rcode = nc_put_var_int( ncid,varids[BAD_FLAG_INDEX],&bad);


  for(index = 0; index < 8; index++) program[index] = ' ';
  strncpy(program,"cedric",6);
  rcode = nc_put_var_text( ncid, varids[PROGRAM_ID_INDEX] , program);


  lev = temp[171];
  if(get_nyquist_velocities(vels,lev) == 1){
     start[0] = 0;
     count[0] = lev;
     rcode = nc_put_vara_float(ncid,varids[NYQUIST_VELS_INDEX],start,count,vels);
  }


  fvalue = temp[303]/scale;
  rcode = nc_put_var_float( ncid,varids[REF_NYQUIST_INDEX] , &fvalue);

  t.tm_year = temp[20] - 1900;
  t.tm_mon  = temp[21] - 1;
  t.tm_mday  = temp[22];
  t.tm_hour  = temp[23];
  t.tm_min   = temp[24];
  t.tm_sec   = temp[25]; 
  btime = mktime(&t);
  ibtime = (int)btime;
  rcode = nc_put_var_int( ncid,varids[BASE_TIME_INDEX],&ibtime);
 
  dvalue = 0.0;
  rcode = nc_put_var_double( ncid,varids[TIME_OFFSET_INDEX],&dvalue);

}/*write_netcdf_variables*/
/************************************************************************/ 
void wcdfdata_(

               int *cunit,float input[MAXPLN],int *nx, int *ny,int *numz,
               int *id)
{
 int rcode,ncid,vid,nplane,index;
 size_t start[5],count[5];
 float *rbuf;



 start[0] = start[2] = start[3] = 0;
 start[1] = *numz-1;
 count[0] = 1;
 count[1] = 1;
 count[2] = *ny;
 count[3] = *nx;

 nplane = *nx * *ny;
 rbuf = (float *)malloc(nplane * sizeof(float));
 if(!rbuf){
    printf("unable to write out data to output file\n");
    exit(-1);
 }

 ncid = *cunit;
 vid  = *id;
 for(index = 0; index < nplane; index++) rbuf[index] = input[index];
 rcode = nc_put_vara_float(ncid,vid,start,count,rbuf);
 if(rcode != NC_NOERR){
    printf("unable to write data to output file: rcode=%d, NC_NOERR=%d\n",rcode,NC_NOERR);
    exit(0);
 }
 free(rbuf);

}/*wcdfdata*/

/************************************************************************/ 
void netcdf_error(char error[10])
{

  printf("ERROR WRITING %s TO THE NETCDF FILE\n");
  exit(0);
}

/************************************************************************/
void open_netcdf_file(const char *name,int *unit)
{
 int nc_status,fid;


    nc_status = nc_open(name, NC_NOWRITE, &fid);
    if (nc_status != NC_NOERR){
	printf("UNABLE TO OPEN NETCDF FILE %s\n",name);
        exit(0);
    }
    *unit = fid;
}/*open_netcdf_file*/


/************************************************************************/
void closecdf_(int *unit)
{
 int ncid,nc_status;

    ncid = *unit;
    nc_status = nc_close(ncid);
}/*close_netcdf*/




