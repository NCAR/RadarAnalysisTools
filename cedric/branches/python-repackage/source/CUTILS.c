#include "cedric.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include "ced_mdv.h"
static struct variables *var;
#define RANK    1
#define YES     1
#define NO      0

/*NOTE: The following two structures are only used in the reading in of 
 *data and the writing out of MDV and NETCDF data files.  They are 
 *not used in the writing out of PURE or COS Cedric data files. 
 *They are also not used in the Cedric interal routines.
 *Some information present in the structures is also present in the
 *Cedric 510 word header.  This is so double or floating point 
 *precision can be used in writing out the data.  

 *The field information structure is used to hold some information that is not
 *present in the Cedric 510 word header.  For MDV format data all data is biased.
 *This bias MUST be applied to the data before writing out because of the way the MDV 
 *write routines writes data.  Also MDV and NETCDF gridded data have both a missing and 
 *and bad data flag.  
 *The scale and field name are present in the Cedric 510 word header they are stored 
 *here also for precision issues. They are only used in reading in and writing out
 *MDV and NETCDF format data.  
 *Everywhere else the Cedric 510 word header is used.
 *This structure is only accesed in the CUTILS.c file. 
 *This structure sets up a doubly linked list.
 */
struct field_information{                              
  int     radar_number;
  int     ifname[8];     /*Integer representation of the field name.*/
  int     file_format;
  double  scale;        /*Used by all data fromats.  Is available in the Cedric 510 header*/
  double  bias;         /*Used by MDV and gridded Netcdf data not in the Cedric 510 header*/
  double  missing;      /*Used by MDV and gridded Netcdf data not in the Cedric 510 header*/

  double  bad;          /*Used by all formats. Is available in the Cedric 510 header*/
  struct field_information *next;
  struct field_information *prior;
};

static struct field_information *start, *last;
  
  

/*
 *This structure is used to store information needed by the C routines.
 *We store lat and lon information as double precision so other programs
 *like CIDD will not have small shifts in the data position due to the fact
 *that they are stored as Integer*2 in the Cedric 510 word header. These values
 *are only used when writing out MDV or Netcdf format data.
 *Times are stored as seconds since 1970 in the MDV and USWRP formats.  Also
 *Zeb needs time in seconds since 1970. 
 *These values are only used in the writing out of MDV and NETCDF formats and reading in of data. 
 * Everywhere else the Cedric 510 word header is used.  
 *This structure is only accesed in the CUTILS.c file.
 */
#define CREADVL 0
#define CTRANSF 1
#define CSYNTHESIS 2
struct cedric_head{
  int    cedric;
  int    data_format;
  int    coord_sys_type;   /*Integer according to definitions in cedric.h*/
  int    num_transf;       /*Integer to hold the number of fields being transfered in*/
  int    num_fields;

  int    forecast_time;     /*Used in the MDV format. This time MUST be present for CIDD*/
  int    centroid_time;     /*Used in the MDV format. This time MUST be present for CIDD*/
  int    start_time;        /*Seconds since 1970 used in both mdv and USWRP gridded data Zeb*/
  int    end_time;          /*Seconds since 1970 used in both mdv and USWRP gridded data*/
  int    synthesis;
  int    syn_radar_num;  
  int    field_names[NFMAX][8];
  int    num_gridded;
  int    grid_number;
  int    level_nyqs;
  int    swap_needed;
  int    read_type;
  int    year;
  double nyquist_vels[MAXZLEV];
  double lat;               /*This allows the storing of lat with double precision*/
  double lon;
  double altitude;
};

  static struct cedric_head *cedhdptr;




/************************************************************************/
void FORTRAN_NAME(icstruct)()
{
    int thisyear;
    /*void init_MDV_struct();*/
    extern int cyear();

    thisyear = cyear();
    cedhdptr = (struct cedric_head *)malloc(sizeof(struct cedric_head));
    if(!cedhdptr){
        printf("THERE IS NOT ENOUGH MEMORY TO RUN CEDRIC\n");
        exit(0);
    } 


    /*init_MDV_struct();*/
    
    start = last = NULL;  /*initialize linked list pointer variables.*/
    cedhdptr->year  = thisyear;
    cedhdptr->num_fields = 0;
    cedhdptr->data_format = CEDPURE;
    cedhdptr->cedric      = 0;
    cedhdptr->synthesis   = 0;
    cedhdptr->syn_radar_num = -1;
    cedhdptr->num_transf  = 0;
    cedhdptr->level_nyqs  = 0;
    cedhdptr->swap_needed = 0;
    cedhdptr->read_type   = -1;
    cedhdptr->num_gridded = -1;
}/*init_csturctures*/

/*********************************************************************/ 
int get_present_year()
{
  extern struct cedric_head *cedhdptr;

  return(cedhdptr->year );

}/*get_pesent_year*/

/*********************************************************************/
void set_format_type(int format,int write_type)
{
  extern struct cedric_head *cedhdptr;

    cedhdptr->data_format = format;
    cedhdptr->cedric  = write_type;
}/*set_format_type*/

/*********************************************************************/
/*
 *Get format type. Format types are defined in cedric.h.
 */
void FORTRAN_NAME(formattp)(int *format,int *write_type)
{
  extern struct cedric_head *cedhdptr;


    *format = cedhdptr->data_format;
    *write_type = cedhdptr->cedric;

}/*get_format_type*/

/*********************************************************************/
/* This procedure cmpscntp *compare scan type*  is called from TRANSF.f.  It 
 *checks the Cedric header word ID(16) with the transfered in 
 *Cedric heademaker ITRHED(16).  It is used with the USWRP gridded
 *files.  In this case we should see an EL for ELEV. 
 */
void FORTRAN_NAME(cmpscntp)(short *first, short *second, short *third ,
			    short *fourth, int *match)
{

 char c1,c2,c3,c4,c5,c6,c7,c8;
 short num1,num2,num3,num4;
 short num5,num6,num7,num8;
 
  num1 = *first/256;
  num2 = *first%256;

  num3 = *second/256;
  num4 = *second%256;

  num5 = *third/256;
  num6 = *third%256;

  num7 = *fourth/256;
  num8 = *fourth%256;


  c1 = (char)num1;
  c2 = (char)num2;

  c3 = (char)num3;
  c4 = (char)num4;
  c5 = (char)num5;
  c6 = (char)num6;
  c7 = (char)num7;
  c8 = (char)num8;  

  if(c1 == 'A' && c2 == 'I' && c5 == 'R'){
     c1 = 'C';
     c2 = 'R';
     c5 = 'T';
  }

  *match = 0;
  printf("TRANSF-CMPSCNTP: %c%c%c   %c%c%c\n",c1,c2,c3,c5,c6,c7);
  if((c1 == c5) && (c2 == c6) && (c3 == c7)){
      *match = 1;
  }
}
/*********************************************************************/
/*The procedure chktname is called from TRANSF.f.
 *This routine determines wheter or not we have gridded 
 *USWRP files from the field name.  It is only called when
 *fields are transfered.
 */

void FORTRAN_NAME(chktname)(int *ntrf,int *gridded,char itraf[NFMAX][8],
			    char gfield[NFMAX][8])
{
   extern struct cedric_head *cedhdptr;    
   int index,uswrp_gridded;
   char temp[4];  


   for(index = 0; index < NFMAX; index++)
   {
         gfield[index][0] = ' ';
         gfield[index][1] = ' ';
         gfield[index][2] = ' ';
         gfield[index][3] = ' ';
         gfield[index][4] = ' ';
         gfield[index][5] = ' ';
         gfield[index][6] = ' ';
         gfield[index][7] = ' '; 
   }


   for(index = 0; index< *ntrf; index++){
       
       temp[0] = itraf[index][2];
       temp[1] = itraf[index][3];
       temp[2] = itraf[index][4];
       temp[3] = itraf[index][5];
       if(strncmp(temp,"grid",4) == 0){
          uswrp_gridded = 1;
          cedhdptr->num_gridded++;
          gfield[index][0] = itraf[index][0];
          gfield[index][1] = itraf[index][1];
          gfield[index][2] = itraf[index][2];
          gfield[index][3] = itraf[index][3];
          gfield[index][4] = itraf[index][4];
          gfield[index][5] = itraf[index][5];
          gfield[index][6] = itraf[index][6];
       }
       else{
          uswrp_gridded = 0;
       }
    }
    *gridded = uswrp_gridded;
}

/*******************************************************************/
/*This set axis label(saxislbl) routine is called from READVL.f and SYNDRV.f
 *It loads the arrays axnam and ctemps which are used in the graphics part(PLOTCH.f)
 *of Cedric to label graphs.
 *id16 and id17 have the output coordinate system from the gridding routines
 *that were run.  It referes to the Cedric 510 word header.
 *The array axnam hold the name of the X,Y and Z axis.
 *The ctemp arrays hold the units of the X and Y axis and the unit of the Z axis.
 */
void FORTRAN_NAME(saxislbl)(
                  char axnam[3][4],char ctemp1[4],char ctemp2[4],char ctemp3[4],
                  int *coord)
{

 extern struct cedric_head *cedhdptr;    
 int swap;
 int coord_type;

  coord_type = cedhdptr->coord_sys_type;
  *coord = 0;
  if(coord_type == 1){
     strcpy(ctemp1,"KM ");
     strcpy(ctemp2,"KM ");
     strcpy(ctemp3,"DEG");
     axnam[0][0] = 'X';
     axnam[0][1] = ' ';
     axnam[0][2] = ' ';
     axnam[0][3] = ' ';

     axnam[1][0] = 'Y';
     axnam[1][1] = ' ';
     axnam[1][2] = ' ';
     axnam[1][3] = ' ';
  
     axnam[2][0] = 'C';
     axnam[2][1] = ' ';
     axnam[2][2] = ' ';
     axnam[2][3] = ' ';
  }
  else if(coord_type == 2){
     strcpy(ctemp1,"KM ");
     strcpy(ctemp2,"KM ");
     strcpy(ctemp3,"DEG");
     axnam[0][0] = 'X';
     axnam[0][1] = ' ';
     axnam[0][2] = ' ';
     axnam[0][3] = ' ';

     axnam[1][0] = 'Y';
     axnam[1][1] = ' ';
     axnam[1][2] = ' ';
     axnam[1][3] = ' ';
  
     axnam[2][0] = 'E';
     axnam[2][1] = ' ';
     axnam[2][2] = ' ';
     axnam[2][3] = ' ';
  }
  else if(coord_type == 0){
     strcpy(ctemp1,"KM ");
     strcpy(ctemp2,"KM ");
     strcpy(ctemp3,"KM ");
     axnam[0][0] = 'X';
     axnam[0][1] = ' ';
     axnam[0][2] = ' ';
     axnam[0][3] = ' ';

     axnam[1][0] = 'Y';
     axnam[1][1] = ' ';
     axnam[1][2] = ' ';
     axnam[1][3] = ' ';
  
     axnam[2][0] = 'Z';
     axnam[2][1] = ' ';
     axnam[2][2] = ' ';
     axnam[2][3] = ' ';
  }
  else if(coord_type == 3){
     *coord = 1;
     strcpy(ctemp1,"DEG");
     strcpy(ctemp2,"DEG");
     strcpy(ctemp3,"DEG");
     axnam[0][0] = 'L';
     axnam[0][1] = 'O';
     axnam[0][2] = 'N';
     axnam[0][3] = ' ';

     axnam[1][0] = 'L';
     axnam[1][1] = 'A';
     axnam[1][2] = 'T';
     axnam[1][3] = ' ';
  
     axnam[2][0] = 'E';
     axnam[2][1] = ' ';
     axnam[2][2] = ' ';
     axnam[2][3] = ' ';
  }
  else if(coord_type == 4){
     *coord = 1;
     strcpy(ctemp1,"DEG");
     strcpy(ctemp2,"DEG");
     strcpy(ctemp3,"KM ");
     axnam[0][0] = 'L';
     axnam[0][1] = 'O';
     axnam[0][2] = 'N';
     axnam[0][3] = ' ';

     axnam[1][0] = 'L';
     axnam[1][1] = 'A';
     axnam[1][2] = 'T';
     axnam[1][3] = ' ';
  
     axnam[2][0] = 'Z';
     axnam[2][1] = ' ';
     axnam[2][2] = ' ';
     axnam[2][3] = ' ';
  }
  else if(coord_type == 5){
     strcpy(ctemp1,"KM ");
     strcpy(ctemp2,"KM ");
     strcpy(ctemp3,"KM ");
     axnam[0][0] = 'X';
     axnam[0][1] = ' ';
     axnam[0][2] = ' ';
     axnam[0][3] = ' ';

     axnam[1][0] = 'Y';
     axnam[1][1] = ' ';
     axnam[1][2] = ' ';
     axnam[1][3] = ' ';
  
     axnam[2][0] = 'Z';
     axnam[2][1] = ' ';
     axnam[2][2] = ' ';
     axnam[2][3] = ' ';
  }
  else if(coord_type == 6){
     strcpy(ctemp1,"KM ");
     strcpy(ctemp2,"KM ");
     strcpy(ctemp3,"KM ");
     axnam[0][0] = 'X';
     axnam[0][1] = ' ';
     axnam[0][2] = ' ';
     axnam[0][3] = ' ';

     axnam[1][0] = 'Y';
     axnam[1][1] = ' ';
     axnam[1][2] = ' ';
     axnam[1][3] = ' ';
  
     axnam[2][0] = 'Z';
     axnam[2][1] = ' ';
     axnam[2][2] = ' ';
     axnam[2][3] = ' ';
  }
  else if(coord_type == 7){
     strcpy(ctemp1,"DEG");
     strcpy(ctemp2,"DEG");
     strcpy(ctemp3,"mb ");
     axnam[0][0] = 'L';
     axnam[0][1] = ' ';
     axnam[0][2] = ' ';
     axnam[0][3] = ' ';

     axnam[1][0] = 'L';
     axnam[1][1] = ' ';
     axnam[1][2] = ' ';
     axnam[1][3] = ' ';
  
     axnam[2][0] = 'P';
     axnam[2][1] = ' ';
     axnam[2][2] = ' ';
     axnam[2][3] = ' ';
  }    
  else if(coord_type == 8){
     strcpy(ctemp1,"GPT");
     strcpy(ctemp2,"GPT");
     strcpy(ctemp3,"GPT");
     axnam[0][0] = 'X';
     axnam[0][1] = ' ';
     axnam[0][2] = ' ';
     axnam[0][3] = ' ';

     axnam[1][0] = 'Y';
     axnam[1][1] = ' ';
     axnam[1][2] = ' ';
     axnam[1][3] = ' ';

     axnam[2][0] = 'Z';
     axnam[2][1] = ' ';
     axnam[2][2] = ' ';
     axnam[2][3] = ' ';
  }
  else{
     printf("UNKNOWN COORDINATE SYSTEM :\n");
     exit(-1);
     }

  }/*get_coordinate_system*/
  
/***********************************************************************
*This routine is called from  REMAP.f.
*This routine takes the input CTEMP1 which is read from the REMAP card 
*command and compares it with what is in the 510 word Cedric header, ID(16)
*and ID(17).  Ctemp1 represents the input coordinate system before the REMAP
*code is executed. This must match whats in the Cedric id words 16 and 17.
*Ctemp3 is the name of the new coordinate system input on the first line of
*the REMAP card in parameter number 9.
*
*/
void FORTRAN_NAME(remapcrd)(
                         char ctemp1[8],short int *id16, short int *id17, 
                         int *icord, int *cerror,char ctemp3[8],
                         int *change)
{
 char  c1,c2,c3,c4;
 char  string[4];
 short num1,num2,num3,num4;
 int   old_coord_sys,new_coord_sys;
 int   swap;

/*
 *Convert the id words to characters.
 */
  num1 = *id16/256;
  num2 = *id16%256;
  num3 = *id17/256;
  num4 = *id17%256;

  c1 = (char)num1;
  c2 = (char)num2;
  c3 = (char)num3;
  c4 = (char)num4;


/*
 *Make sure that the input coordinate system matches the coordinate system
 *That is in the Cedric id header.
 */
  swap = 0;
  if((ctemp1[0] != c1) && (ctemp1[1] != c2) && (ctemp1[2] != c3)){
      swap = 1;
  }
  if(swap == 1){
     if((ctemp1[0] != c2)  && (ctemp1[1] != c1) && (ctemp1[2] != c4)){      
      *cerror = 575;
      return; 
    }
  }

/*
 *Determine the old coordinate system type.
 */
  if(ctemp1[0] == 'C' && ctemp1[1] == 'R' && ctemp1[2] == 'T'){
     old_coord_sys = XYZ;
     printf("OLD REMAP COORD SYSTEM = CRT(XYZ)\n");
  }
  else if(ctemp1[0] == 'R' && ctemp1[1] == 'C' && ctemp1[2] == ' '){
     old_coord_sys = XYZ;
     printf("OLD REMAP COORD SYSTEM = CRT(XYZ)\n");
  }
  else if(ctemp1[0] == 'C' && ctemp1[1] == 'A' && ctemp1[2] == 'R'){
     old_coord_sys = XYZ;
     printf("OLD REMAP COORD SYSTEM = CART(XYZ)\n");
  }
  else if(ctemp1[0] == 'R' && ctemp1[1] == 'C' && ctemp1[2] == 'T'){
     old_coord_sys = XYZ;
     printf("OLD REMAP COORD SYSTEM = CART(XYZ)\n");
  }
  else if(ctemp1[0] == 'X' && ctemp1[1] == 'Y' && ctemp1[2] == 'Z'){
     old_coord_sys = XYZ;
     printf("OLD REMAP COORD SYSTEM = CRT(XYZ)\n");
  } 
  else if(ctemp1[0] == 'Y' && ctemp1[1] == 'X' && ctemp1[2] == ' '
     && ctemp1[3] == 'Z'){
     printf("OLD REMAP COORD SYSTEM = CRT(XYZ)\n");
     old_coord_sys = XYZ;
  } 
  else if(ctemp1[0] == 'C' && ctemp1[1] == 'O' && ctemp1[2] == 'P'){
     old_coord_sys = XYC;
     printf("OLD REMAP COORD SYSTEM = COPLANE(XYC)\n");
  } 
  else if(ctemp1[0] == 'O' && ctemp1[1] == 'C' && ctemp1[2] == 'L'){
     old_coord_sys = XYC;
     printf("OLD REMAP COORD SYSTEM = COPLANE(XYC)\n");
  } 
  else if(ctemp1[0] == 'E' && ctemp1[1] == 'L' && ctemp1[2] == 'E'){
     old_coord_sys = XYE;
     printf("OLD REMAP COORD SYSTEM = ELEV(XYE)\n");
  }
  else if(ctemp1[0] == 'L' && ctemp1[1] == 'E' && ctemp1[2] == 'V'){
     old_coord_sys = XYE;
     printf("OLD REMAP COORD SYSTEM = ELEV(XYE)\n");
  }
  else if(ctemp1[0] == 'X' && ctemp1[1] == 'Y' && ctemp1[2] == 'E'){
     old_coord_sys = XYE;
     printf("OLD REMAP COORD SYSTEM = ELEV(XYE)\n");
  }
  else if(ctemp1[0] == 'Y' && ctemp1[1] == 'X' && ctemp1[2] == ' '
	  && ctemp1[3] == 'E'){
     old_coord_sys = XYE;
     printf("OLD REMAP COORD SYSTEM = ELEV(XYE)\n");
  }
  else if(ctemp1[0] == 'L' && ctemp1[1] == 'L' && ctemp1[2] == 'E'){
     old_coord_sys = LLE;
     printf("OLD REMAP COORD SYSTEM = LON LAT ELEV\n");
  }
  else if(ctemp1[0] == 'L' && ctemp1[1] == 'L' && ctemp1[2] == ' '
          && ctemp1[3] == 'E'){
     old_coord_sys = LLE;
     printf("OLD REMAP COORD SYSTEM = LON LAT ELEV\n");
  }
  else if(ctemp1[0] == 'L' && ctemp1[1] == 'L' && ctemp1[2] == 'Z'){
     old_coord_sys = LLZ;
     printf("OLD REMAP COORD SYSTEM = LON LAT Z\n");
  }
  else if(ctemp1[0] == 'L' && ctemp1[1] == 'L' && ctemp1[2] == ' '
          && ctemp1[3] == 'Z'){
     old_coord_sys = LLZ;
     printf("OLD REMAP COORD SYSTEM = LON LAT ELEV\n");
  }
  else if(ctemp1[0] == 'A' && ctemp1[1] == 'I' && ctemp1[2] == 'R'){
     old_coord_sys = XYZ;
     printf("OLD REMAP COORD SYSTEM = AIR(XYZ)\n");
  }
  else if(ctemp1[0] == 'I' && ctemp1[1] == 'A' && ctemp1[2] == ' '){
     old_coord_sys = XYZ;
     printf("OLD REMAP COORD SYSTEM = AIR(XYZ)\n");
  }
  else if(ctemp1[0] == 'L' && ctemp1[1] == 'L' && ctemp1[2] == 'P'){
     old_coord_sys = LLP;
     printf("OLD REMAP COORD SYSTEM = LAT LON Pressure(LLP)\n");
  }
  else if(ctemp1[0] == 'L' && ctemp1[1] == 'L' && ctemp1[2] == ' '
          && ctemp1[3] == 'P'){
     old_coord_sys = LLP;
     printf("OLD REMAP COORD SYSTEM = LON LAT Pressure\n");
  }
  else{
     printf("UNKNOWN INPUT COORDINATE SYSTEM IN REMAP: %c%c%c",c1,c2,c3);
     exit(-1);
  }

/*
 *Determine the new coordinate system type.
 */
  if(ctemp3[0] == 'C' && ctemp3[1] == 'R' && ctemp3[2] == 'T'){
     new_coord_sys = XYZ;
     printf("NEW REMAP COORD SYSTEM = CRT(XYZ)\n");
  }
  else if(ctemp3[0] == 'R' && ctemp3[1] == 'C' && ctemp3[2] == ' '){
     new_coord_sys = XYZ;
     printf("NEW REMAP COORD SYSTEM = CRT(XYZ)\n");
  }
  else if(ctemp3[0] == 'C' && ctemp3[1] == 'A' && ctemp3[2] == 'R'){
     new_coord_sys = XYZ;
     printf("NEW REMAP COORD SYSTEM = CART(XYZ)\n");
  }
  else if(ctemp3[0] == 'R' && ctemp3[1] == 'C' && ctemp3[2] == 'T'){
     new_coord_sys = XYZ;
     printf("NEW REMAP COORD SYSTEM = CART(XYZ)\n");
  }
  else if(ctemp3[0] == 'X' && ctemp3[1] == 'Y' && ctemp3[2] == 'Z'){
     new_coord_sys = XYZ;
     printf("NEW REMAP COORD SYSTEM = CRT(XYZ)\n");
  }
  else if(ctemp3[0] == 'Y' && ctemp3[1] == 'X' && ctemp3[2] == ' '
          && ctemp3[3] == 'Z'){  
     new_coord_sys = XYZ;
     printf("NEW REMAP COORD SYSTEM = CRT(XYZ)\n");
  }
  else if(ctemp3[0] == 'C' && ctemp3[1] == 'O' && ctemp3[2] == 'P'){
     new_coord_sys = XYC;
     printf("NEW REMAP COORD SYSTEM = COPLANE(XYC)\n");
  } 
  else if(ctemp3[0] == 'O' && ctemp3[1] == 'C' && ctemp3[2] == ' '){
     new_coord_sys = XYC;
     printf("NEW REMAP COORD SYSTEM = COPLANE(XYC)\n");
  } 
  else if(ctemp3[0] == 'E' && ctemp3[1] == 'L' && ctemp3[2] == 'E'){
     new_coord_sys = XYE;
     printf("NEW REMAP COORD SYSTEM = ELEV(XYE)\n");
  } 
  else if(ctemp3[0] == 'L' && ctemp3[1] == 'E' && ctemp3[2] == 'V'){
     printf("NEW REMAP COORD SYSTEM = ELEV(XYE)\n");
     new_coord_sys = XYE;
  } 
  else if(ctemp3[0] == 'X' && ctemp3[1] == 'Y' && ctemp3[2] == 'E'){
     printf("NEW REMAP COORD SYSTEM = ELEV(XYE)\n");
     new_coord_sys = XYE;
  }
  else if(ctemp3[0] == 'Y' && ctemp3[1] == 'X' && ctemp3[2] == ' '
          && ctemp3[3] == 'E') {
     printf("NEW REMAP COORD SYSTEM = ELEV(XYE)\n");
     new_coord_sys = XYE;
  }
  else if(ctemp3[0] == 'L' && ctemp3[1] == 'L' && ctemp3[2] == 'E'){
     new_coord_sys = LLE;
     printf("NEW REMAP COORD SYSTEM = LON LAT ELEV\n");
  }
  else if(ctemp3[0] == 'L' && ctemp3[1] == 'L' && ctemp3[2] == ' '
     && ctemp3[3] == 'E'){
     new_coord_sys = LLE;
     printf("NEW REMAP COORD SYSTEM = LON LAT ELEV\n");
  }
  else if(ctemp3[0] == 'L' && ctemp3[1] == 'L' && ctemp3[2] == 'Z'){
     new_coord_sys = LLZ;
     printf("NEW REMAP COORD SYSTEM = LON LAT Z\n");
  }
  else if(ctemp3[0] == 'L' && ctemp3[1] == 'L' && ctemp3[2] == 'P'){
     new_coord_sys = LLP;
     printf("NEW REMAP COORD SYSTEM = LON LAT Pressure\n");
  }
  else if(ctemp3[0] == 'L' && ctemp3[1] == 'L' && ctemp3[2] == ' '
     && ctemp3[3] == 'P'){
     new_coord_sys = LLP;
     printf("NEW REMAP COORD SYSTEM = LON LAT Pressure\n");
  }
  else if(ctemp3[0] == 'L' && ctemp3[1] == 'L' && ctemp3[2] == ' '
     && ctemp3[3] == 'Z'){
     new_coord_sys = LLZ;
     printf("NEW REMAP COORD SYSTEM = LON LAT Z\n");
  }
  else if(ctemp3[0] == 'A' && ctemp3[1] == 'I' && ctemp3[2] == 'R'){
     new_coord_sys = XYZ;
     printf("NEW REMAP COORD SYSTEM = AIR(XYZ)\n");
  }
  else if(ctemp3[0] == 'I' && ctemp3[1] == 'A' && ctemp3[2] == ' '){
     new_coord_sys = XYZ;
     printf("NEW REMAP COORD SYSTEM = AIR(XYZ)\n");
  }
  else{     
     new_coord_sys = XYZ;
     printf("NEW REMAP COORD SYSTEM = CART(XYZ)\n");
  }


/*
 *Set the value of icord.
 */
*change = 0;  
if(old_coord_sys != new_coord_sys) *change = 1;

if(old_coord_sys == XYZ && new_coord_sys == XYZ){  /*cartesian to cartesian*/
   *icord = 0;
}
else if(old_coord_sys == XYC && new_coord_sys == XYZ){ /*coplane to cartesian*/
   *icord = 1;
}
else if(old_coord_sys == XYE && new_coord_sys == XYZ){ /*xy ele to xy height*/
   *icord = 2;
}
else if(old_coord_sys == LLE && new_coord_sys == LLZ){ /*lon lat ele to lon lat height*/
   *icord = 3;
}
else if(old_coord_sys == XYE && new_coord_sys == LLE){ /*xy ele to lon lat ele*/
   *icord = 4;
}
else if(old_coord_sys == XYZ && new_coord_sys == LLZ){ /*xy height to lon lat height*/
   *icord = 5;
}
else if(old_coord_sys == LLE && new_coord_sys == XYE){ /*lon lat ele to xy ele*/
   *icord = 6;
}
else if(old_coord_sys == LLZ && new_coord_sys == XYZ){ /*lon lat height to xy height*/
   *icord = 7;
}
else if(old_coord_sys == XYE && new_coord_sys == LLZ){ /*cartesian to lon lat height*/
   printf("Cedric is unable to go from XYE to LLZ directly at this time\n");
   exit(0);
}
else{
    printf("CEDRIC IS UNABLE TO TRANSFORM from %c%c%c to %c%c%c\n",
           ctemp1[0],ctemp1[1],ctemp1[2],ctemp3[0],ctemp3[1],ctemp3[2]);
    exit(-1);
}



}/*chk_remap_coord_sys*/




/*****************************************************************
 *This procedure is called from REMAP.f.  
 *It updates the Plotting variable AXNAM with the new axis names input
 *from the user.
 *Icord is the new type of coordinate system. Definitions for icord are
 *at the beginning of this file.
 */
void update_axis_names_(
                        char axnam[3][4], char ctemp3[8], char xyunits[3],
                        char zunits[3])
{

  if(strncmp(ctemp3,"XYZ",3) == 0)
  {
         axnam[0][0] = 'X';
         axnam[0][1] = ' ';
         axnam[0][2] = ' ';
         axnam[0][3] = ' ';

         axnam[1][0] = 'Y';
         axnam[1][1] = ' ';
         axnam[1][2] = ' ';
         axnam[1][3] = ' ';
  
         axnam[2][0] = 'Z';
         axnam[2][1] = ' ';
         axnam[2][2] = ' ';
         axnam[2][3] = ' ';

         xyunits[0] = 'K';
         xyunits[1] = 'M';
         xyunits[2] = ' ';
         zunits[0]  = 'K';
         zunits[1]  = 'M';
         zunits[2]  = ' ';
         
   }
   else if(strncmp(ctemp3,"XYC",3) == 0)
   {
         axnam[0][0] = 'X';
         axnam[0][1] = ' ';
         axnam[0][2] = ' ';
         axnam[0][3] = ' ';

         axnam[1][0] = 'Y';
         axnam[1][1] = ' ';
         axnam[1][2] = ' ';
         axnam[1][3] = ' ';
  
         axnam[2][0] = 'C';
         axnam[2][1] = ' ';
         axnam[2][2] = ' ';
         axnam[2][3] = ' ';

         xyunits[0] = 'K';
         xyunits[1] = 'M';
         xyunits[2] = ' ';
         zunits[0]  = 'K';
         zunits[1]  = 'M';
         zunits[2]  = ' ';
   }
   else if(strncmp(ctemp3,"XYE",3) == 0)
   {
         axnam[0][0] = 'X';
         axnam[0][1] = ' ';
         axnam[0][2] = ' ';
         axnam[0][3] = ' ';

         axnam[1][0] = 'Y';
         axnam[1][1] = ' ';
         axnam[1][2] = ' ';
         axnam[1][3] = ' ';
  
         axnam[2][0] = 'E';
         axnam[2][1] = ' ';
         axnam[2][2] = ' ';
         axnam[2][3] = ' ';

         xyunits[0] = 'K';
         xyunits[1] = 'M';
         xyunits[2] = ' ';
         zunits[0]  = 'D';
         zunits[1]  = 'E';
         zunits[2]  = 'G';
   }
   else if(strncmp(ctemp3,"LLE",3) == 0)      
   {
         axnam[0][0] = 'L';
         axnam[0][1] = 'O';
         axnam[0][2] = 'N';
         axnam[0][3] = ' ';

         axnam[1][0] = 'L';
         axnam[1][1] = 'A';
         axnam[1][2] = 'T';
         axnam[1][3] = ' ';
  
         axnam[2][0] = 'E';
         axnam[2][1] = ' ';
         axnam[2][2] = ' ';
         axnam[2][3] = ' ';

         xyunits[0] = 'D';
         xyunits[1] = 'E';
         xyunits[2] = 'G';
         zunits[0]  = 'D';
         zunits[1]  = 'E';
         zunits[2]  = 'G';
  }   
  else if(strncmp(ctemp3,"LLZ",3) == 0) 
  {
         axnam[0][0] = 'L';
         axnam[0][1] = ' ';
         axnam[0][2] = ' ';
         axnam[0][3] = ' ';

         axnam[1][0] = 'L';
         axnam[1][1] = ' ';
         axnam[1][2] = ' ';
         axnam[1][3] = ' ';
  
         axnam[2][0] = 'Z';
         axnam[2][1] = ' ';
         axnam[2][2] = ' ';
         axnam[2][3] = ' ';

         xyunits[0] = 'D';
         xyunits[1] = 'E';
         xyunits[2] = 'G';
         zunits[0]  = 'K';
         zunits[1]  = 'M';
         zunits[2]  = ' ';
  }
  else{
         printf("********* ERROR: Input to CUTILS.c ********\n" );
         printf("UNRECOGNIZED COORDINATE SYSTEM FOR REMAPing\n");
	 printf("OPTIONS ARE XYZ, XYC, XYE, LLE, OR LLZ\n"); 
         exit(0);
  }
}



/************************************************************************/
void
get_radar_location_(float *radar_lat,float *radar_lon,float *radar_alt,
		    char radarName[4])
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

  found = 0;
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
          found = 1;
          break;
	  }
   }/*end while*/

   if(found == 0){
     printf("REQUESTED RADAR %s NOT FOUND IN RADAR SITES FILE\n",radarName);
     exit(0);
   }


   fclose(fs);
   *radar_lat = (float)latd + latm/60. + lats/3600.;  
   if(lond < 0){
      lonm = - lonm;
      lons = - lons;
   }
   *radar_lon = (float)lond + lonm/60. + lons/3600.;
   *radar_alt = altitude;

}/*end get_radar_location*/
/********************************************************************************/
void convert_to_char(int number,char char_string[4])
{
  int one,two,three,four;
  int temp,save;
  
  if(number < 10){
    one = number;
    char_string[0] = '0';
    char_string[1] = (char)one + 48;
  }
  else if(number >= 10 && number < 100){
    one  = number/10;
    two  = number%10; 
    char_string[0] = (char)one + 48;
    char_string[1] = (char)two + 48;
  }
  else{
    temp = number/10;
    one  = number - (temp * 10);
    char_string[3] = (char)one + 48;
    number = temp;
    temp = number/10;
    two  = number - (temp * 10);
    char_string[2] = (char)two + 48;
    number = temp;
    temp = number/10; 
    three = number - (temp * 10);   
    char_string[1] = (char)three + 48;
    char_string[0] = (char)temp + 48;
  }
}/*end convert_to_char*/

/*********************************************************************/
void convert_string_int(char ctemp[100],int tarray[9],int adate)

{

  char str[4];

  str[0] = ctemp[0];
  str[1] = ctemp[1];
  str[2] = '\0';
  tarray[0] = atoi(str);

  str[0] = ctemp[3];
  str[1] = ctemp[4];
  str[2] = '\0';
  tarray[1] = atoi(str);

  if(adate == 1){
    str[0] = ctemp[6];
    str[1] = ctemp[7];
    str[2] = ctemp[8];
    str[3] = ctemp[9];
    tarray[2] = atoi(str);
  }
  else{
    str[0] = ctemp[6];
    str[1] = ctemp[7]; 
    str[2] = '\0';   
    tarray[2] = atoi(str);
  }

}/*convert_string_int*/
/*******************************************************************************/
/*
 *Convert lat and lon to degree minutes and seconds.
 *This routine is called from CDFPOSN.
 */

 void convert_to_degminsec(int *deg,int *min,double *sec,double var)
{

  int  ivar,minutes;
  float rlat, rfract, rtemp; 
  double seconds;

  ivar = (int)var;
  rtemp = var - (float)ivar;
  rfract = rtemp*3600.;
  minutes = (int)rfract/60;
  seconds = rfract - 60. * (double)minutes;
  *deg = ivar;
  *min = minutes;
  *sec =  seconds;
} /* convert_to_degminsec*/ 
/*******************************************************************************/
/*
 *Convert lat and lon to degree minutes and seconds.
 *This routine is called from CEDCDF.f and CEDMDV.f
 */
void FORTRAN_NAME(degminsc)(
                   short *deg, short *min, float *sec, double *var)
{

  int  ivar,minutes;
  float rlat, rfract, rtemp, seconds;

  ivar = (int)*var;
  rtemp = *var - (float)ivar;
  rfract = rtemp*3600.;
  minutes = (int)rfract/60;
  seconds = rfract - 60. * (float)minutes;
  *deg = (short)ivar;
  *min = (short)minutes;
  *sec =  seconds;
}/*degminsec*/

/***********************************************************************/
int FORTRAN_NAME(crand)(int *the_seed)
{

  unsigned int seed;
  int random;
  extern void srand();

  seed = *the_seed;
  srand(seed);
  random = rand();
  return(random);
}
  
/***********************************************************************/
void FORTRAN_NAME(gm_time)(int *date_string,int tarray[9])
{
      
  struct tm *lt;
  time_t stime = *date_string;

  lt = gmtime (&stime);
  tarray[0] = lt->tm_sec;
  tarray[1] = lt->tm_min;
  tarray[2] = lt->tm_hour;
  tarray[3] = lt->tm_mday;
  tarray[4] = lt->tm_mon;
  tarray[5] = lt->tm_year;
  tarray[6] = lt->tm_wday;
  tarray[7] = lt->tm_yday;
  tarray[8] = lt->tm_isdst;
  return;
}
/***********************************************************************/
/*The following routines are used to store and retrieve some variables that 
 *are used in the writing out of the MDV and CDF header files.  A few of
 *the variables are located in the 510 word header but are stored in the
 *cedric_head structure for more resolution.  Most of the variables used
 *in these routines are not available in the Cedric 510 word header but 
 *are necessary for the NETCDF and MDV formats.
 */
/***********************************************************************/
/*
 *Reset lat lon.
 */
void FORTRAN_NAME(reset_ll)()
{
/*
 *This routine is called from FIXIDS.  It is used to tell the MDV
 *and Netcdf write routines that the users has specificed the latitude
 *and longitude.  The lat and lon in the Cedric 510 word header will
 *be used to write the MDV and Netcdf files.
 */
extern struct cedric_head *cedhdptr;
  
  cedhdptr->lat = -32768.;
  cedhdptr->lon = -32768.;
}/*reset_latlon*/
/***********************************************************************/
void save_latlonalt(double lat,double lon,double alt)
{
extern struct cedric_head *cedhdptr;

   if(cedhdptr->synthesis > 1) return;
   cedhdptr->lat = lat;
   cedhdptr->lon = lon;
   cedhdptr->altitude = alt;

}/*save_latlonalt*/
/***********************************************************************/
void get_latlonalt(double *lat,double *lon,double *alt)
{
extern struct cedric_head *cedhdptr;

   *lat = cedhdptr->lat;
   *lon = cedhdptr->lon;
   *alt = cedhdptr->altitude;

}/*get_input_latlon*/

/***********************************************************************/
void set_global_swap_flag()
{

extern struct cedric_head *cedhdptr;

  cedhdptr->swap_needed = 1;
}/*set_global_swap_flag*/

/***********************************************************************/
int  check_global_swap_flag()
{

extern struct cedric_head *cedhdptr;

  if(cedhdptr->swap_needed == 1) {
     return(1);
  }
  else{
     return(0);
  }

}/*set_global_swap_flag*/
/***********************************************************************/
/*
 *Save the coordinate system.
 */
int FORTRAN_NAME(savcrdsy)(char name[2][2])
{

int found;
char tempc[4];
char c1,c2,c3,c4;
extern struct cedric_head *cedhdptr;

   c1  = name[0][0];
   c2  = name[0][1];
   c3  = name[1][0];
   c4  = name[1][1];

  printf(" IN savcrdsy: %c%c%c%c\n",c1,c2,c3,c4);
  if(c1 == 'C'  && c2 == 'R' && c3 == 'T'){
     cedhdptr->coord_sys_type = 0;
     found = 1;
  }
  if(c1 == 'S' && c2 == 'U' && c3 == 'R'){
      found = 1;
     cedhdptr->coord_sys_type = 0;
  }
  else if(c1 == 'X'  && c2 == 'Y' && c3 == 'Z'){
     cedhdptr->coord_sys_type = 0;
     found = 1;
  }
  else if(c1 == 'X'  && c2 == 'Y' && c3 == 'E'){
     cedhdptr->coord_sys_type = 2;
     found = 1;
  }
  else if(c1 == 'C'  && c2 == 'A' && c3 == 'R'){
     cedhdptr->coord_sys_type = 1;
     found = 1;
  }
  else if(c1 == 'C' && c2 == 'O' && c3 == 'P'){
     cedhdptr->coord_sys_type = 1;
     found = 1;
  }
  else if(c1 == 'E' && c2 == 'L' && c3 == 'E'){
     cedhdptr->coord_sys_type = 2;
     found = 1;
  }
  else if(c1 == 'L' && c2 == 'L' && c3 == 'E'){
     cedhdptr->coord_sys_type = 3;
     found = 1;
  }
  else if(c1 == 'L' && c2 == 'L' && c3 == 'P'){
     cedhdptr->coord_sys_type = 7;
     found = 1;
  }
  else if(c1 == 'L' && c2 == 'L' && c3 == 'Z'){
     cedhdptr->coord_sys_type = 4;
     found = 1;
  }
  else if(c1 == 'A' && c2 == 'I' && c3 == 'R'){
     cedhdptr->coord_sys_type = 0;
     found = 1;
  }
  else if(c1 == 'P' && c2 == 'P' && c3 == 'I'){
     cedhdptr->coord_sys_type = 5;
     found = 1;
  }
  else if(c1 == 'R' && c2 == 'H' && c3 == 'I'){
     cedhdptr->coord_sys_type = 6;
     found = 1;
  }
  else if(c1 == 'W' && c2 == 'R' && c3 == 'F'){
     cedhdptr->coord_sys_type = 8;
     found = 1;
  }


  else{
     found = 0;
  }

  if(found == 1) return;


  c1  = name[0][1];
  c2  = name[0][0];
  c3  = name[1][1];
  c4  = name[1][0];

  if(c1 == 'O' && c2 == 'C' && c3 == ' '){
      cedhdptr->coord_sys_type = 1;
  }
  if(c1 == 'U' && c2 == 'S' && c3 == ' ' && c4 == 'R'){
     cedhdptr->coord_sys_type = 0;
  }
  else if(c1 == 'Y' && c2 == 'X' && c3 == ' '){
      cedhdptr->coord_sys_type = 0;
  }
  else if(c1 == 'A' && c2 == 'C' && c3 == 'T'){
      cedhdptr->coord_sys_type = 1;
  }
  else if(c1 == 'L' && c2 == 'E' && c3 == 'V'){
      cedhdptr->coord_sys_type = 2;
  }
  else if(c1 == 'R'  && c2 == 'C' && c3 == ' '){
      cedhdptr->coord_sys_type = 0;
  }
  else if(c1 == 'L' && c2 == 'L' && c3 == ' ' && c4 == 'E'){
      cedhdptr->coord_sys_type = 3;
  }
  else if(c1 == 'L' && c2 == 'L' && c3 == ' ' && c4 == 'P'){
      cedhdptr->coord_sys_type = 7;
  }
  else if(c1 == 'L' && c2 == 'L' && c3 == ' ' && c4 == 'Z'){
      cedhdptr->coord_sys_type = 4;
  }
  else if(c1 == 'I' && c2 == 'A' && c3 == ' '){
      cedhdptr->coord_sys_type = 0;
  }
  else if(c1 == 'P' && c2 == 'P' && c3 == ' '){
      cedhdptr->coord_sys_type = 5;
  }
  else if(c1 == 'H' && c2 == 'R' && c3 == ' '){
      cedhdptr->coord_sys_type = 6;
  }
  else if(c1 == 'H' && c2 == 'R' && c3 == ' '){
      cedhdptr->coord_sys_type = 7;
  }

  else{
      printf("UNKNOWN COORDINATE SYSTEM IN SAVE COORDINATE SYSTEM: %c%c%c%c\n",
                c1,c2,c3,c4);
      exit(0);
  }


  set_global_swap_flag();
}/*save the coordinate system type*/

/***********************************************************************/
void output_coord_system(int *value)
{
extern struct cedric_head *cedhdptr;

  *value = cedhdptr->coord_sys_type;

}/*output_coord_system*/
/***********************************************************************/
/* THE FOLLOWING ROUTINES MANAGE THE LINKED LIST FOR FIELD INFORMATION*/
/***********************************************************************/
void FORTRAN_NAME(purella)(
              int *lat1,int *lat2,int *lat3, int *lon1, int *lon2,
              int *lon3, float *alt, int *scale)
{
extern struct cedric_head *cedhdptr;

  cedhdptr->lat = *lat1 + *lat2/60. + (*lat3/3600.)/(double)*scale;
  cedhdptr->lon = *lon1 + *lon2/60. + (*lon3/3600.)/(double)*scale;
  cedhdptr->altitude = (double)*alt;

}/*pure_latlonalt*/
/***********************************************************************/
void FORTRAN_NAME(printll)(int *badll)
{

extern struct cedric_head *cedhdptr;

  if(cedhdptr->lat == -32768) *badll = 1;
  *badll = 0;
  printf("\n");
  printf("ORIGIN   LATITUDE:  %f\n",cedhdptr->lat);
  printf("        LONGITUDE:  %f\n",cedhdptr->lon);
}/*print lat lon*/
/***********************************************************************/

int get_Cfield_info(int *format,double *the_scale, double *the_bias, 
                    double *dmiss,double *dbad,char name[8])
{
   int  iname[8];
   int  index;

}/*get_Cfield_info*/
/************************************************************************
                   ROUTINES USED FOR USWRP GRIDDED DATA ONLY
************************************************************************/
/*
 *save gridded field names. Called from READVL.f  We want to save the names
 *of the fields input on the READVL line on the input KRD.
 */
void FORTRAN_NAME(svgdfldn)(char name1[8],char name2[8])
{
  int index;
  extern struct cedric_head *cedhdptr;
   
   cedhdptr->grid_number = -1;
   if(name1[0] == ' ' || name1[0] == '\0') return;
   for(index = 0; index < 8; index++){
     cedhdptr->field_names[0][index] = -1;
     if(name1[index] != ' ' && name1[index] != '\0'){
        cedhdptr->field_names[0][index] = (int)name1[index];
     }
   }


   cedhdptr ->num_gridded = 1;
   cedhdptr->grid_number = (int)name1[6] - 48;

   if(name2[0] == ' ' || name1[0] == '\0') return;
   for(index = 0; index < 8; index++){
     cedhdptr->field_names[1][index] = -1;
     if(name2[index] != ' ' && name2[index] != '\0'){
        cedhdptr->field_names[1][index] = (int)name2[index];
     }
   }
   cedhdptr->num_gridded = 2;

}/*svgdfldn*/
/************************************************************************/
/*
 *uswrp grid number.
 */
void FORTRAN_NAME(uswprgn)()
{
  extern struct cedric_head *cedhdptr;

  cedhdptr->grid_number = -1;

}/*reset_grid_number*/
/************************************************************************/
void FORTRAN_NAME(gridresl)(int *number)
{
  extern struct cedric_head *cedhdptr;

  
  *number = cedhdptr->grid_number;
}/*get_grid_number*/

/************************************************************************/
int  compare_griddednames(char name[10])
{
 
  int index,gindex,iname[8];

  extern struct cedric_head *cedhdptr;  
  
  if(cedhdptr->num_gridded == 0){
     printf("NO USWRP GRIDDED FIELD NAMES EXITS AT THIS TIME\n");
     exit(-1);
  }

  for(index = 0; index < 8; index++){
      iname[index] = -1;
      if(name[index] != ' ' && name[index] != '\0'){
         iname[index] = (int)name[index];
      }
  }


  for(gindex = 0; gindex < cedhdptr->num_gridded; gindex++){
    /*printf("%d %d %d %d %d %d %d %d\n",iname[0],iname[1],iname[2],
                                     iname[3],iname[4],iname[5],
                                     iname[6],iname[7]);
				     sleep(2);*/
      if(cedhdptr->field_names[gindex][0] == iname[0] &&
         cedhdptr->field_names[gindex][1] == iname[1] &&
         cedhdptr->field_names[gindex][2] == iname[2] &&
         cedhdptr->field_names[gindex][3] == iname[3] && 
         cedhdptr->field_names[gindex][4] == iname[4] &&
         cedhdptr->field_names[gindex][5] == iname[5] &&
         cedhdptr->field_names[gindex][6] == iname[6] && 
         cedhdptr->field_names[gindex][7] == iname[7]){
         return(1);
      }
  }/*gindex*/

  return(0);
}/*check_gridnames*/
/************************************************************************/
int get_numgridded()
{
   extern struct cedric_head *cedhdptr;

   return(cedhdptr->num_gridded);
}/*get_numgridded*/
/************************************************************************/
void set_nyquists_flag()
{

   extern struct cedric_head *cedhdptr;

    cedhdptr->level_nyqs = 1;

}/*set_nyquists_flag*/
/************************************************************************/
/*
 *This routine is called from gcdfgeni in cdf_utils.  It is used to
 *save the WSR88D level2 data nyquist velocities since there is usually
 *a nyquist associated with each level and these can differ.  They are
 *saved so they can be written out with a requested output data set.
 */
void save_gridded_nyquists(double grid_nyqs[128],int nlevels)
{
   extern struct cedric_head *cedhdptr;
  int index;

  for(index = 0; index < nlevels; index++){
      cedhdptr->nyquist_vels[index] = grid_nyqs[index];
  }

}/*save_gridded_nyquists*/
/************************************************************************/
/*
 *This routine is called from wcdfmisc in cdf_utils.  It is used to
 *get the saved WSR88D level2 nyquist velocities so they can be written
 *to the netcdf output file.
 */
int get_nyquist_velocities(float vels[128],int levels)
{
  extern struct cedric_head *cedhdptr;
  int index;

  if(cedhdptr->level_nyqs == 0){
     return(0);
  }
  else{
     for(index = 0; index < levels; index++){
        vels[index] = (float)cedhdptr->nyquist_vels[index];
     }
  }

  return(1);
}/*get_gridded_nyquists*/  

/************************************************************************
                 ROUTINES FOR RETURNING MEMORY TO THE SYSTEM
************************************************************************/
void  free_cedhdptr()
{
  extern struct cedric_head *cedhdptr;

  free(cedhdptr);
}/*free_cedhdptr*/
/*************************************************************************/
void initfldn_(char names[25][8])
{

  memset(names,' ',25*8);
}/*initfldn_*/


/************************************************************************/
void znumflds_()
{
extern struct cedric_head *cedhdptr;


cedhdptr->num_fields = 0;
}/*znumflds*/
/************************************************************************/
/*save user field names*/
void susrfldn_(char names[25][8])
{
 extern struct cedric_head *cedhdptr;
 int num,i,j;

  num = 25*8;
  memset(cedhdptr->field_names,' ',num);   
        
  for(i = 0; i < 25; i++){
     if(names[i][0] == '\n') return;
     if(names[i][0] == '\0') return;
     if(names[i][0] != ' '){
        cedhdptr->num_fields++;
        cedhdptr->field_names[i][0] = names[i][0];
        cedhdptr->field_names[i][1] = names[i][1];
        cedhdptr->field_names[i][2] = names[i][2];
        cedhdptr->field_names[i][3] = names[i][3];
        cedhdptr->field_names[i][4] = names[i][4];
        cedhdptr->field_names[i][5] = names[i][5];
        cedhdptr->field_names[i][6] = names[i][6];
        cedhdptr->field_names[i][7] = names[i][7];
     }
  }    
}/*userfldn*/

/************************************************************************/
void fieldnms_(char fldnam[NFMAX][8],int *num)
{  
 extern struct cedric_head *cedhdptr;
 int i;

 *num = cedhdptr->num_fields;
 for(i = 0; i < cedhdptr->num_fields; i++){
      fldnam[i][0] = cedhdptr->field_names[i][0];
      fldnam[i][1] = cedhdptr->field_names[i][1];
      fldnam[i][2] = cedhdptr->field_names[i][2];
      fldnam[i][3] = cedhdptr->field_names[i][3];
      fldnam[i][4] = cedhdptr->field_names[i][4];
      fldnam[i][5] = cedhdptr->field_names[i][5];
      fldnam[i][6] = cedhdptr->field_names[i][6];
      fldnam[i][7] = cedhdptr->field_names[i][7];
 }

}/*fieldnms*/
/************************************************************************/

     
