#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <math.h>
#include "dorade.h"
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
#define MAX(a,b)  ( ((a) > (b)) ? (a) : (b) )


struct dorade_basic{
       int   year;
       int   month;
       int   day;
       int   swap;
       int   file_open;
       int   sweep_file;
       int   radar_type;
       int   radd_cnt;
       int   scan_mode;
       int   data_compression;
       int   total_parameters;
       int   bad;
       int   sweep_num;
       int   num_rays;
       int   fld_typ[2][25];
       int   ngates;
       int   rdatcnt;
       int   fd;
       int   blknum;
       int   blk_read;
       int   ryib_cnt;
       int   swib_cnt;
       int   vold_cnt;
       int   endian_type;
       int   process;
       int   skipping;
       int   correct_rad;
       int   end_beam;
       int   bad_beam;
       int   bad_count;
       int   num_frequencies;
       int   num_ipulse;
       int   asib;
       int   bytesbeam;
       int   num_req_flds;
       int   end_time_reached;
       float radar_constant[2];
       float nyquist_velocity[2];
       float lat;
       float lon;
       float alt;
       float fixed_angle[2];
       float scale[2][25];
       float offset[2][25];
       float gate_spacing;
       float range_first_gate;
       float range_delay;
       char  radar_name[2][8];
       char  current_radar[8];
       char  last_radar[8];
       char  field_name[25][8];
       char  req_fld[25][8];
};

static struct dorade_basic *dorptr;

/************************************************************************/
void init_dorade_struct()
{

    dorptr = (struct dorade_basic *)malloc(sizeof(struct dorade_basic));
    if(!dorptr){
        printf("THERE IS NOT ENOUGH MEMORY\n");
        exit(0);
    } 

    dorptr->swap        = -1;
    dorptr->sweep_file  = 0;
    dorptr->file_open   = 0;
    dorptr->radd_cnt    = -1;
    dorptr->sweep_num   = 0;
    dorptr->blknum      = 0;
    dorptr->skipping    = 0;
    dorptr->swib_cnt    = 0;
    dorptr->vold_cnt    = 0;
    dorptr->num_req_flds = -1;
    dorptr->end_time_reached = NO;
 
}/*idorade*/

/************************************************************************/
void free_dorade_struct()
{ 
 extern struct dorade_basic *dorptr;

 free(dorptr);
}
/************************************************************************/
int doropen(int funit_num)
{

 int  i1,i2,i3,i4;
 int  filed;
 char filename[9];
 extern struct dorade_basic *dorptr;

    if (funit_num < 10 || funit_num > 999) {
      printf("\n +++ DORADE: Invalid unit number %d +++\n",funit_num);
      exit(1);
    }
    i1 = funit_num / 10 ;
    i2 = funit_num % 10 ;
    i3 = i1 / 10 ;
    i4 = i1 % 10 ;

    /* construct filename */
    filename[0] = 'f';
    filename[1] = 'o';
    filename[2] = 'r';
    filename[3] = 't';
    filename[4] = '.';
    if (funit_num <= 99) {
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

  filed = myopen(filename);
  if(filed <= 0){
    printf("UNABLE TO OPEN THE INPUT FILE %s\n",filename);
    exit(0);
  }    
  dorptr->file_open = 1;
  printf("DORADE file %s is open\n",filename);

  return(filed);
}/*doropen*/
/*********************************************************************************/
void julday(juldat, iyr, imon, iday)
     int juldat, iyr;
     int *imon, *iday;
{
  int idymon[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
  int sum, i;

  if ((juldat > 365 && ((iyr%4) != 0)) || juldat > 366) {
    printf("error in julian date:%d\n", juldat);
    exit(1);
  }
  /* check for leap year */
  if ((iyr%4) == 0) idymon[1] = 29;
  
  sum = 0;
  
  for (i = 0; i < 12; i++) {
    sum = sum + idymon[i];
    if (sum >= juldat) break;
  }

  *imon = i + 1;
  *iday = juldat - (sum - idymon[i]);
  

  return;

}
/****************************************************************************************/
/*                            WRITE DESCIPTORS TO FILE                                  */
/*****************************************************************************************/
void print_vold(struct vold_blk vold,FILE *fpw,int iprint)
{
  extern struct dorade_basic *dorptr;


  if(fpw){
        fprintf(fpw,"\n\n           +++  VOLUME HEADER  +++\n");
        fprintf(fpw,"VOLD - Block # %2d\n",dorptr->blknum);
        fprintf(fpw,"VOLD descriptor size     %d\n",vold.length);
        fprintf(fpw,"DATE                     %d/%d/%d\n",dorptr->month,dorptr->day,dorptr->year);
        fprintf(fpw,"TIME                     %d:%d:%d\n",vold.hour,vold.min,vold.sec);
        fprintf(fpw,"Flight number        =  %8s\n",vold.flt_number);    
        fprintf(fpw,"Generating facility  =  %8s\n",vold.gen_fac);    
        fprintf(fpw,"Project  name        =  %8s\n",vold.proj_name);
        fprintf(fpw,"version=%8d   volnum=%8d   maxbyt=%8d  num_sens=%8d\n",vold.ver,
                vold.volnum, vold.maxbyt,vold.num_sens);
        fprintf(fpw,"\n");
     }
  else{
        printf("\n\n           +++  VOLUME HEADER  +++\n");
        printf("VOLD - Block # %2d\n",dorptr->blknum);
        printf("VOLD descriptor size    %d\n",vold.length);
        printf("DATE                    %d/%d/%d\n",dorptr->month,dorptr->day,dorptr->year);
        printf("TIME                    %d:%d:%d\n",vold.hour,vold.min,vold.sec);
        printf("Flight number        =  %8s\n",vold.flt_number);    
        printf("Generating facility  =  %8s\n",vold.gen_fac);    
        printf("Project  name        =  %8s\n",vold.proj_name);
        printf("version=%8d   volnum=%8d   maxbyt=%8d  num_sens=%8d\n",vold.ver,
             vold.volnum, vold.maxbyt,vold.num_sens);
        printf("\n");
}     

}/*print vold*/
/*****************************************************************************************/
void print_radd(struct radd_blk radd,FILE *fpw, int iprint)
{
  extern struct dorade_basic *dorptr;
  int    index;


  if(fpw){
      if(iprint == SUMM){
          fprintf(fpw,"RADD - # %d   Name  ",dorptr->blknum);
          for(index = 0; index < 8; index++){
	    if(radd.radnam[index] != ' ' && radd.radnam[index] != '\0') 
               fprintf(fpw,"%c",radd.radnam[index]);
	  }
          fprintf(fpw,"   Parms  %d   Lat %f   Lon %f   alt %f   Nyq  %f   Radar const.   %f\n",
                   radd.tot_par,radd.rad_lat,radd.rad_long,radd.rad_alt,radd.nyq,radd.radcon);  

     }
     if (iprint == FULL) { /* print out info */  
         fprintf(fpw,"\n          +++ Radar Descriptor Block +++\n");
         fprintf(fpw,"RADD - Block # %2d\n",dorptr->blknum);
         fprintf(fpw,"RADD descriptor size   %d\n",radd.length);
         fprintf(fpw,"Radar name             ");
         for(index = 0; index < 8; index++){
	    if(radd.radnam[index] != ' ' && radd.radnam[index] != '\0') 
               fprintf(fpw,"%c",radd.radnam[index]);
	 }
         fprintf(fpw,"\n");
         fprintf(fpw,"Radar constant         %f\n",radd.radcon);
         fprintf(fpw,"Nominal Peak Power     %f\n",radd.nompkpw);
         fprintf(fpw,"Nominal Noise Power    %f\n",radd.nomnspw);
         fprintf(fpw,"Receiver gain          %f\n",radd.rcvgain);
         fprintf(fpw,"Anetnna gain           %f\n",radd.antgain);
         fprintf(fpw,"Radar system gain      %f\n",radd.radgain);
         fprintf(fpw,"Horizontal beam width  %f\n",radd.horbmwth);
         fprintf(fpw,"Vertical beam width    %f\n",radd.verbmwth);
         fprintf(fpw,"Nominal scan rate      %f\n",radd.scan_rate);
         fprintf(fpw,"Nominal start angle    %f\n",radd.start_ang);
         fprintf(fpw,"Nominal stop angle     %f\n",radd.stop_ang);
         fprintf(fpw,"Total Parameters       %d\n",radd.tot_par);
         fprintf(fpw,"Total descriptors      %d\n",radd.tot_des);
         fprintf(fpw,"Radar latitude         %f\n",radd.rad_lat);
         fprintf(fpw,"Radar longitude        %f\n",radd.rad_long);
         fprintf(fpw,"Radar altitude         %f\n",radd.rad_alt);
         fprintf(fpw,"Nyquist velocity       %f\n",radd.nyq);
         fprintf(fpw,"Unambiguous range      %f\n",radd.max_range);
         fprintf(fpw,"Number of frequencies  %f\n",radd.num_freq);
         fprintf(fpw,"Num inter-pulse freq   %f\n",radd.num_ip);
         fprintf(fpw,"Data Compression Flag  %d\n",radd.data_comp);
         fprintf(fpw,"Frequency #1           %f\n",radd.freq1);
         fprintf(fpw,"Frequency #2           %f\n",radd.freq2);
         fprintf(fpw,"Frequency #3           %f\n",radd.freq3);
         fprintf(fpw,"Frequency #4           %f\n",radd.freq4);
         fprintf(fpw,"Frequency #5           %f\n",radd.freq5);
         fprintf(fpw,"Interpulse period  #1  %f\n",radd.ipp1);
         fprintf(fpw,"Interpulse period  #2  %f\n",radd.ipp2);
         fprintf(fpw,"Interpulse period  #3  %f\n",radd.ipp3);
         fprintf(fpw,"Interpulse period  #4  %f\n",radd.ipp4);
         fprintf(fpw,"Interpulse period  #5  %f\n",radd.ipp5);

         switch(dorptr->radar_type){
            case 0:{
                    fprintf(fpw,"RADAR TYPE: GROUND\n");
                    break;
            }
            case 1:{
                   fprintf(fpw,"RADAR TYPE: AIRBORNE FOR\n");
                   break;
            }
            case 2:{
                   fprintf(fpw,"RADAR TYPE: AIRBORNE AFT\n");
                   break;
            }
            case 3:{
                   fprintf(fpw,"RADAR TYPE: AIRBORNE TAIL\n");
                   break;
            }
            case 4:{
                   fprintf(fpw,"RADAR TYPE: AIRBORNE LF\n");
                   break;
            }
            case 5:{
                   fprintf(fpw,"RADAR TYPE: SHIPBORNE\n");
                   break;
            }   
            default:{
                 fprintf(fpw,"UNKNOWN RADAR TYPE\n");
                 exit(-1);
            }
        }/*end for switch*/

        if(dorptr->radar_type == 5) dorptr->radar_type = 0;

        if(dorptr->scan_mode < 0 || dorptr->scan_mode > 9){
            fprintf(fpw,"INVALID SCAN MODE\n");
            exit(-1);
         }

         switch(dorptr->scan_mode){
             case 0:{
                    fprintf(fpw,"SCAN MODE IS CALIBRATION\n");
                    break;
             }
             case 1:{
                    fprintf(fpw,"SCAN MODE IS PPI (CONSTANT ELEVATION)\n");
                    break;
             }
             case 2:{
                    fprintf(fpw,"SCAN MODE IS COPLANE\n");
                    break;
             }
             case 3:{
                    fprintf(fpw,"SCAN MODE IS RHI (CONSTANT AZIMUTH)\n");
                    break;
             }
             case 4:{
                    fprintf(fpw,"SCAN MODE IS VERTICAL POINTING");
                    break;
             }
             case 5:{
                    fprintf(fpw,"SCAN MODE IS TARGET (STATIONARY)\n");
                    break;
             }
             case 6:{
                    fprintf(fpw,"SCAN MODE IS MANUAL\n");
                    break;
             }  
             case 7:{
                    fprintf(fpw,"SCAN MODE IS IDLE (OUT OF CONTROL) :-(\n");
                    break;
             }  
             case 8:{
                    fprintf(fpw,"SCAN MODE IS FULL SWEEP(SURVEILLANCE)\n");
                    break;
             }  
             case 9:{
                    fprintf(fpw,"SCAN MODE IS PARTIAL SWEEP (AIRBORNE?)\n");
                    break;
             }   
             default:{
                   fprintf(fpw,"UNKNOWN SCAN MODE\n");
                   exit(-1);
             }
      }/*end for switch*/
     }/*if FULL*/
  }/*if*/
  else{
     if(iprint == SUMM)
          printf("RADD - # %d    Name  ",dorptr->blknum);
          for(index = 0; index < 8; index++){
	    if(radd.radnam[index] != ' ' && radd.radnam[index] != '\0') 
               printf("%c",radd.radnam[index]);
	  }
          printf("   Parms  %d   Lat %f   Lon %f   alt %f   Nyq  %f   Radar const. %f\n",
                   radd.tot_par,radd.rad_lat,radd.rad_long,radd.rad_alt,radd.nyq,radd.radcon);  


     if (iprint == FULL) { /* print out info */ 
         printf("\n          +++ Radar Descriptor Block +++\n");
         printf("RADD - Block # %d\n",dorptr->blknum); 
         printf("Radar name             ");
          for(index = 0; index < 8; index++){
	    if(radd.radnam[index] != ' ' && radd.radnam[index] != '\0') 
               printf("%c",radd.radnam[index]);
	  }
         printf("\n");
         printf("LENGTH OF RADAR DESCRIPTOR BLOCK %d\n",radd.length);
         printf("Radar constant         %f\n",radd.radcon);
         printf("Nominal Peak Power     %f\n",radd.nompkpw);
         printf("Nominal Noise Power    %f\n",radd.nomnspw);
         printf("Receiver gain          %f\n",radd.rcvgain);
         printf("Anetnna gain           %f\n",radd.antgain);
         printf("Radar system gain      %f\n",radd.radgain);
         printf("Horizontal beam width  %f\n",radd.horbmwth);
         printf("Vertical beam width    %f\n",radd.verbmwth);
         printf("Nominal scan rate      %f\n",radd.scan_rate);
         printf("Nominal start angle    %f\n",radd.start_ang);
         printf("Nominal stop angle     %f\n",radd.stop_ang);
         printf("Total Parameters       %d\n",radd.tot_par);
         printf("Total descriptors      %d\n",radd.tot_des);
         printf("Radar latitude         %f\n",radd.rad_lat);
         printf("Radar longitude        %f\n",radd.rad_long);
         printf("Radar altitude         %f\n",radd.rad_alt);
         printf("Nyquist velocity       %f\n",radd.nyq);
         printf("Unambiguous range      %f\n",radd.max_range);
         printf("Number of frequencies  %f\n",radd.num_freq);
         printf("Num inter-pulse freq   %f\n",radd.num_ip);
         printf("Frequency #1           %f\n",radd.freq1);
         printf("Frequency #2           %f\n",radd.freq2);
         printf("Frequency #3           %f\n",radd.freq3);
         printf("Frequency #4           %f\n",radd.freq4);
         printf("Frequency #5           %f\n",radd.freq5);
         printf("Interpulse period  #1  %f\n",radd.ipp1);
         printf("Interpulse period  #2  %f\n",radd.ipp2);
         printf("Interpulse period  #3  %f\n",radd.ipp3);
         printf("Interpulse period  #4  %f\n",radd.ipp4);
         printf("Interpulse period  #5  %f\n",radd.ipp5);
          switch(dorptr->radar_type){
            case 0:{
                    printf("RADAR TYPE: GROUND");
                    break;
            }
            case 1:{
                   printf("RADAR TYPE: AIRBORNE FOR");
                   break;
            }
            case 2:{
                   printf("RADAR TYPE: AIRBORNE AFT");
                   break;
            }
            case 3:{
                   printf("RADAR TYPE: AIRBORNE TAIL");
                   break;
            }
            case 4:{
                   printf("RADAR TYPE: AIRBORNE LF");
                   break;
            }
            case 5:{
                   printf("RADAR TYPE: SHIPBORNE");
                   break;
            }   
            default:{
                 printf("UNKNOWN RADAR TYPE\n");
                 exit(-1);
            }
        }/*end for switch*/

        if(dorptr->scan_mode < 0 || dorptr->scan_mode > 9){
            printf("INVALID SCAN MODE\n");
            exit(-1);
         }

         switch(dorptr->scan_mode){
             case 0:{
                    printf("  SCAN MODE IS CALIBRATION\n");
                    break;
             }
             case 1:{
                    printf("  SCAN MODE IS PPI (CONSTANT ELEVATION)\n");
                    break;
             }
             case 2:{
                    printf("  SCAN MODE IS COPLANE\n");
                    break;
             }
             case 3:{
                    printf("  SCAN MODE IS RHI (CONSTANT AZIMUTH)\n");
                    break;
             }
             case 4:{
                    printf("  SCAN MODE IS VERTICAL POINTING");
                    break;
             }
             case 5:{
                    printf("  SCAN MODE IS TARGET (STATIONARY)\n");
                    break;
             }
             case 6:{
                    printf("  SCAN MODE IS MANUAL\n");
                    break;
             }  
             case 7:{
                    printf("  SCAN MODE IS IDLE (OUT OF CONTROL)\n");
                    break;
             }  
             case 8:{
                    printf("  SCAN MODE IS FULL SWEEP(SURVEILLANCE)\n");
                    break;
             }  
             case 9:{
                    printf("  SCAN MODE IS PARTIAL SWEEP\n");
                    break;
             }   
             default:{
                   printf("  UNKNOWN SCAN MODE\n");
                   exit(-1);
             }
      }/*end for switch*/
    }

  }

}/*print_radd*/

/*****************************************************************************************/
void print_parm(struct parm_blk parm,FILE *fpw,int iprint)
{
  extern struct dorade_basic *dorptr;
  int    index;

  if(fpw)
  {
    if(iprint == SUMM){
         fprintf(fpw,"PARM - # %2d   ",dorptr->blknum);
         for(index = 0; index < 8; index++){ 
            if(parm.name[index] != ' ') fprintf(fpw,"%c",parm.name[index]);
         }
         fprintf(fpw,"\n");
     }

     if (iprint == FULL) {
         fprintf(fpw,"\n           +++Parameter Descriptor Block+++\n");
         fprintf(fpw,"PARM - Block  # %d \n",dorptr->blknum);
         fprintf(fpw,"PARM descriptor size     %d\n",parm.length);
         fprintf(fpw,"Parameter name           ");
         for(index = 0; index < 8; index++){ 
            if(parm.name[index] != ' ') fprintf(fpw,"%c",parm.name[index]);
         }
         fprintf(fpw,"\n");
         fprintf(fpw,"Parameter description    %8s\n",parm.desc); 
         fprintf(fpw,"Parameter units          %8s\n",parm.units);
         fprintf(fpw,"Inter pulse period(s)    %d\n",parm.ipp);
         fprintf(fpw,"Transmitted frequencies  %d\n",parm.tran_freq);
         fprintf(fpw,"Receiver bandwidth       %f\n",parm.rec_band);
         fprintf(fpw,"Pulse width              %d\n",parm.pul_wid);
         if(parm.polar == 0)
               fprintf(fpw,"Polarization is Horizontal\n");
         else if(parm.polar == 1)
               fprintf(fpw,"Polarization is Vertical\n"); 
         else if(parm.polar == 2)
               fprintf(fpw,"Polarization is Right handed Circular\n");   
         else if(parm.polar == 3)
               fprintf(fpw,"Polarization is Elliptical\n");
         else if(parm.polar == 4)
               fprintf(fpw,"Polarization is Left handed Circular\n");
         else if(parm.polar == 5)
               fprintf(fpw,"Polarization is Dual\n"); 
         else
               fprintf(fpw,"UNKNOWN POLARIZATION %d\n",parm.polar);

         fprintf(fpw,"Threshold field          %8s    Threshold value %f\n",parm.thr_field,parm.thr_value);  
         fprintf(fpw,"Field scale              %f\n",parm.scale);
         fprintf(fpw,"Field bias               %f\n",parm.offset);
         fprintf(fpw,"Bad data flag            %d\n",parm.bad);
      }
  }
  else{
     if(iprint == SUMM){
         printf("PARM - # %2d   ",dorptr->blknum);
         for(index = 0; index < 8; index++){ 
            if(parm.name[index] != ' ') printf("%c",parm.name[index]);
         }
         printf("\n");
     }

     if (iprint == FULL) {
         printf("\n           +++Parameter Descriptor Block+++\n");
         printf("PARM - Block  # %d \n",dorptr->blknum);
         printf("PARM descriptor size     %d\n",parm.length);
         printf("Parameter name           ");
         for(index = 0; index < 8; index++){ 
            if(parm.name[index] != ' ') printf("%c",parm.name[index]);
         }
         printf("\n");
         printf("Parameter description    %8s\n",parm.desc); 
         printf("Parameter units          %8s\n",parm.units);
         printf("Inter pulse period(s)    %d\n",parm.ipp);
         printf("Transmitted frequencies  %d\n",parm.tran_freq);
         printf("Receiver bandwidth       %f\n",parm.rec_band);
         printf("Pulse width              %d\n",parm.pul_wid);
         if(parm.polar == 0)
               printf("Polarization is Horizontal\n");
         else if(parm.polar == 1)
               printf("Polarization is Vertical\n"); 
         else if(parm.polar == 2)
               printf("Polarization is Right handed Circular\n");   
         else if(parm.polar == 3)
               printf("Polarization is Elliptical\n");
         else if(parm.polar == 4)
               printf("Polarization is Left handed Circular\n");
         else if(parm.polar == 5)
               printf("Polarization is Dual\n"); 
         else
              printf("UNKNOWN POLARIZATION %d\n",parm.polar);

         printf("Threshold field          %8s    Threshold value %f\n",parm.thr_field,parm.thr_value);  
         printf("Field scale              %f\n",parm.scale);
         printf("Field bias               %f\n",parm.offset);
         printf("Bad data flag            %d\n",parm.bad);
      }
  }


}
/*print_parm*/

/*****************************************************************************************/
void print_celv(struct celv_blk celv,FILE *fpw, int iprint,float spacing)
{
  extern struct dorade_basic *dorptr;
  char printstr[1000];


  printstr[0]='\0';

  if(fpw){
     if(iprint == SUMM)
        fprintf(fpw,"CELV - #  %d\n",dorptr->blknum);

     if(iprint == FULL) {
        fprintf(fpw,"\n           +++Cell Range Vector Block+++\n");
        fprintf(fpw,"CELV Block # %d\n",dorptr->blknum);
        fprintf(fpw,"CELV descriptor size        %d\n",celv.length);
        fprintf(fpw,"num_cells= &d   dist2first= %f   spacing= %f\n",
                 celv.num_cells, celv.dist_to_fir,spacing);
     }
  }
  else{

     if(iprint == SUMM)
        printf("CELV - #  %d\n",dorptr->blknum);

     if(iprint == FULL) {
        printf("\n           +++Cell Range Vector Block+++\n");
        printf("CELV Block # %d\n",dorptr->blknum);
        printf("CELV descriptor size        %d\n",celv.length);
        printf("num_cells= &d   dist2first= %f   spacing= %f\n",
                 celv.num_cells, celv.dist_to_fir,spacing);
     }
  }
}/*print celv*/
/*****************************************************************************************/
void print_cfac(struct cfac_blk cfac,FILE *fpw, int iprint)
{
  extern struct dorade_basic *dorptr;


  if(fpw){
     if(iprint == SUMM)
         fprintf(fpw,"CFAC - #  %d\n",dorptr->blknum);

     if (iprint == FULL){
         fprintf(fpw,"\n          +++Correction Factor Block+++\n");
         fprintf(fpw,"CFAC - Block # %d\n",dorptr->blknum);
         fprintf(fpw,"CFAC descriptor size  %d\n",cfac.length);
         fprintf(fpw,"Azimuth               %f\n",cfac.azim);
         fprintf(fpw,"Elevation             %f\n",cfac.elev);
         fprintf(fpw,"Range delay           %f\n",cfac.rng_del);
         fprintf(fpw,"Longitude             %f\n",cfac.lon);
         fprintf(fpw,"Latitude              %f\n",cfac.lat);
         fprintf(fpw,"Pressure altitude     %f\n",cfac.prs_alt);
         fprintf(fpw,"Altitude              %f\n",cfac.alt);
         fprintf(fpw,"E-W ground speed      %f\n",cfac.gspd_ew);
         fprintf(fpw,"N-S ground speed      %f\n",cfac.gspd_ns);
         fprintf(fpw,"Vertical velocity     %f\n",cfac.ver_vel);
         fprintf(fpw,"Heading               %f\n",cfac.heading);
         fprintf(fpw,"Roll                  %f\n",cfac.roll);
         fprintf(fpw,"Pitch                 %f\n",cfac.pitch);
         fprintf(fpw,"Drift                 %f\n",cfac.drift);
         fprintf(fpw,"Rotation angle        %f\n",cfac.rot_ang);
         fprintf(fpw,"Tilt                  %f\n",cfac.tilt);
     }
  }
  else{
     if(iprint == SUMM)
         printf("CFAC - #  %d\n",dorptr->blknum);

     if (iprint == FULL){
         printf("\n          +++Correction Factor Block+++\n");
         printf("CFAC - Block # %d\n",dorptr->blknum);
         printf("CFAC descriptor size  %d\n",cfac.length);
         printf("Azimuth               %f\n",cfac.azim);
         printf("Elevation             %f\n",cfac.elev);
         printf("Range delay           %f\n",cfac.rng_del);
         printf("Longitude             %f\n",cfac.lon);
         printf("Latitude              %f\n",cfac.lat);
         printf("Pressure altitude     %f\n",cfac.prs_alt);
         printf("Altitude              %f\n",cfac.alt);
         printf("E-W ground speed      %f\n",cfac.gspd_ew);
         printf("N-S ground speed      %f\n",cfac.gspd_ns);
         printf("Vertical velocity     %f\n",cfac.ver_vel);
         printf("Heading               %f\n",cfac.heading);
         printf("Roll                  %f\n",cfac.roll);
         printf("Pitch                 %f\n",cfac.pitch);
         printf("Drift                 %f\n",cfac.drift);
         printf("Rotation angle        %f\n",cfac.rot_ang);
         printf("Tilt                  %f\n",cfac.tilt);
     }
  }
}/*print_cfac*/ 
/*****************************************************************************************/
void print_swib(struct swib_blk swib,FILE *fpw, int iprint)
{
  extern struct dorade_basic *dorptr;


  if(fpw){
    /*     fprintf(fpw,"\nREADING A NEW SWEEP\n");*/

     if(iprint == SUMM)
        fprintf(fpw,"SWIB - # %d  :name  %8s  Fixed angle %f  Num rays %d\n",
              dorptr->blknum,dorptr->current_radar,swib.fix_ang,swib.nray);

     if (iprint == FULL) { 
         fprintf(fpw,"\n        +++Sweep Info Block+++\n");
         fprintf(fpw,"SWIB - Block #   %d\n",dorptr->blknum);
         fprintf(fpw,"SWIB descriptor size                  %d\n",swib.length);
         fprintf(fpw,"Radar name for this sweep             %8s\n",dorptr->current_radar);
         fprintf(fpw,"Sweep number from beginnig of volume  %d\n",swib.swpnum);
         fprintf(fpw,"Number of rays for sweep              %d\n",dorptr->num_rays);
         fprintf(fpw,"Sweep start angle                     %f\n",swib.start_ang);
         fprintf(fpw,"Sweep stop angle                      %f\n",swib.stop_ang);
         fprintf(fpw,"Fixed angle for this sweep            %f\n",dorptr->fixed_angle[dorptr->radd_cnt]);
         fprintf(fpw,"Sweep filter flag                     %d\n",swib.filt);
    
      }
  }

  else{
     if(iprint == SUMM)
        printf("SWIB - # %d  :name  %8s  Fixed angle %f  Num rays %d\n",
              dorptr->blknum,dorptr->current_radar,swib.fix_ang,swib.nray);

     if (iprint == FULL) { 
         printf("\n        +++Sweep Info Block+++\n");
         printf("SWIB - Block #   %d\n",dorptr->blknum);
         printf("SWIB descriptor size                  %d\n",swib.length);
         printf("Radar name for this sweep             %8s\n",dorptr->current_radar);
         printf("Sweep number from beginnig of volume  %d\n",swib.swpnum);
         printf("Number of rays for sweep              %d\n",dorptr->num_rays);
         printf("Sweep start angle                     %f\n",swib.start_ang);
         printf("Sweep stop angle                      %f\n",swib.stop_ang);
         printf("Fixed angle for this sweep            %f\n",dorptr->fixed_angle[dorptr->radd_cnt]);
         printf("Sweep filter flag                     %d\n",swib.filt);
      }
 }

}/*print_swib*/


/*****************************************************************************************/
void  print_ryib(struct ryib_blk ryib,FILE *fpw, int iprint)
{
  int  imon,iday,len;
  extern struct dorade_basic *dorptr;


  if(fpw){
     if(iprint == SUMM ){
        fprintf(fpw,"\n+++++++++BEAM %d IN SWEEP %d++++++++++\n",dorptr->ryib_cnt,dorptr->swib_cnt);
        julday(ryib.day,dorptr->year,&imon,&iday);
        fprintf(fpw,"RYIB - #  %d  :date  %d/%d/%d  time %d:%d:%d:%d  az= %f   elev= %f\n",
             dorptr->blknum,imon,iday,dorptr->year,ryib.hour,ryib.min,ryib.sec,ryib.msec,ryib.az,ryib.elev);
     }


     if (iprint == FULL) { /* print out the info */
        fprintf(fpw,"\n+++++++++BEAM %d IN SWEEP %d++++++++++\n",dorptr->ryib_cnt,dorptr->swib_cnt);
        fprintf(fpw,"\n        +++Ray Info Block+++\n");
        fprintf(fpw,"RYIB Block # %d\n",dorptr->blknum);
        fprintf(fpw,"RYIB descriptor size    %d\n",ryib.length);
        fprintf(fpw,"Sweep number            %d\n",ryib.swpnum);
        fprintf(fpw,"Julian day              %d\n",ryib.day);
        fprintf(fpw,"Beam time               %d/%d/%d/%d\n",ryib.hour,ryib.min,ryib.sec,ryib.msec);
        fprintf(fpw,"Azimuth                 %f\n",ryib.az);
        fprintf(fpw,"Elevation               %f\n",ryib.elev);
        fprintf(fpw,"Peak transmitted power  %f\n",ryib.pkpwr);
        fprintf(fpw,"Radar true scan rate    %f\n",ryib.rate);
        fprintf(fpw,"Beam status             %d\n",ryib.status);
     }
  }
  else{
     if(iprint == SUMM ){
        printf("\n+++++++++BEAM %d IN SWEEP %d++++++++++\n",dorptr->ryib_cnt,dorptr->swib_cnt);
        julday(ryib.day,dorptr->year,&imon,&iday);
        printf("RYIB - #  %d  :date  %d/%d/%d  time %d:%d:%d:%d  az= %f   elev= %f\n",
             dorptr->blknum,imon,iday,dorptr->year,ryib.hour,ryib.min,ryib.sec,
             ryib.msec,ryib.az,ryib.elev);
     }


     if (iprint == FULL) { /* print out the info */
        printf("\n+++++++++BEAM %d IN SWEEP %d++++++++++\n",dorptr->ryib_cnt,dorptr->swib_cnt);
        printf("\n        +++Ray Info Block+++\n");
        printf("RYIB Block # %d\n",dorptr->blknum);
        printf("RYIB descriptor size    %d\n",ryib.length);
        printf("Sweep number            %d\n",ryib.swpnum);
        printf("Julian day              %d\n",ryib.day);
        printf("Beam time               %d/%d/%d/%d\n",ryib.hour,ryib.min,ryib.sec,ryib.msec);
        printf("Azimuth                 %f\n",ryib.az);
        printf("Elevation               %f\n",ryib.elev);
        printf("Peak transmitted power  %f\n",ryib.pkpwr);
        printf("Radar true scan rate    %f\n",ryib.rate);
        printf("Beam status             %d\n",ryib.status);
     }     
  }


}/*print_ryib*/
/*****************************************************************************************/
void  print_asib(struct asib_blk asib,FILE *fpw, int iprint)
{


  if(fpw){
     if(iprint == SUMM) 
        fprintf(fpw,"ASIB - #  %d  :Lat %f  Lon %f  Alt(msl) %f\n",
                dorptr->blknum,asib.lat,asib.lon,asib.pres_alt);


     if(iprint == FULL) { /* print out info */
        fprintf(fpw,"        \n+++Platform Info Block+++\n");
        fprintf(fpw,"ASIB - Block # %d\n",dorptr->blknum);
        fprintf(fpw,"ASIB descriptor size                      %d\n",asib.length);
        fprintf(fpw,"Radar latitude                            %f\n",asib.lat);
        fprintf(fpw,"Radar longitue                            %f\n",asib.lon);
        fprintf(fpw,"Radar pressure altitude(MSL)(km)          %f\n",asib.pres_alt);
        fprintf(fpw,"Radar altitude above ground level(km)     %f\n",asib.alt_gnd);
        fprintf(fpw,"Platform ground speed(East-West)(m/sec)   %f\n",asib.gndspd_ew);
        fprintf(fpw,"Platform ground speed(North-South)(m/sec) %f\n",asib.gndspd_ns);
        fprintf(fpw,"Platform vertical velocity(m/sec)         %f\n",asib.ver_vel);
        fprintf(fpw,"Platform heading(deg)                     %f\n",asib.heading);
        fprintf(fpw,"Platform roll(deg)                        %f\n",asib.roll);  
        fprintf(fpw,"Platform pitch(deg)                       %f\n",asib.pitch);
        fprintf(fpw,"Platform drift(deg)                       %f\n",asib.drift);
        fprintf(fpw,"Radar rotation angle(deg)                 %f\n",asib.rot_ang);
        fprintf(fpw,"Radar tilt angle(deg)                     %f\n",asib.tilt_ang);   
        fprintf(fpw,"Horizontal wind speed at radar(E-W)(m/sec)%f\n",asib.wndspd_ew);
        fprintf(fpw,"Horizontal wind speed at radar(N-S)(m/sec)%f\n",asib.wndspd_ns);
        fprintf(fpw,"Vertical wind speed at radar(m/sec)       %f\n",asib.verspd);
        fprintf(fpw,"Heading change rate(deg/sec)              %f\n",asib.head_rate);
        fprintf(fpw,"Pitch change rate(deg/sec)                %f\n",asib.ptch_rate);
     }
  }
  else{
      if(iprint == SUMM) 
        printf("ASIB - #  %d  :Lat %f  Lon %f  Alt(msl) %f\n",
                dorptr->blknum,asib.lat,asib.lon,asib.pres_alt);


     if(iprint == FULL) { /* print out info */
        printf("        \n+++Platform Info Block+++\n");
        printf("ASIB - Block # %d\n",dorptr->blknum);
        printf("ASIB descriptor size                      %d\n",asib.length);
        printf("Radar latitude                            %f\n",asib.lat);
        printf("Radar longitue                            %f\n",asib.lon);
        printf("Radar pressure altitude(MSL)(km)          %f\n",asib.pres_alt);
        printf("Radar altitude above ground level(km)     %f\n",asib.alt_gnd);
        printf("Platform ground speed(East-West)(m/sec)   %f\n",asib.gndspd_ew);
        printf("Platform ground speed(North-South)(m/sec) %f\n",asib.gndspd_ns);
        printf("Platform vertical velocity(m/sec)         %f\n",asib.ver_vel);
        printf("Platform heading(deg)                     %f\n",asib.heading);
        printf("Platform roll(deg)                        %f\n",asib.roll);  
        printf("Platform pitch(deg)                       %f\n",asib.pitch);
        printf("Platform drift(deg)                       %f\n",asib.drift);
        printf("Radar rotation angle(deg)                 %f\n",asib.rot_ang);
        printf("Radar tilt angle(deg)                     %f\n",asib.tilt_ang);   
        printf("Horizontal wind speed at radar(E-W)(m/sec)%f\n",asib.wndspd_ew);
        printf("Horizontal wind speed at radar(N-S)(m/sec)%f\n",asib.wndspd_ns);
        printf("Vertical wind speed at radar(m/sec)       %f\n",asib.verspd);
        printf("Heading change rate(deg/sec)              %f\n",asib.head_rate);
        printf("Pitch change rate(deg/sec)                %f\n",asib.ptch_rate);
     }
  }      

}/*print_asib*/

/**********************************************************************************
                ROUTINES TO READ DORADE VOLUME HEADER DESCRIPTORS
**********************************************************************************/

int  read_sswb(int fd,FILE *fpw,int len, int iprint)
{

  int   rval;
  char array[MXBYTES];

  dorptr->blknum++;

  if(iprint != 0){ 
     if(dorptr->process == YES){
        if(iprint == FULL){ 
          if(fpw){
             fprintf(fpw,"SSWB - #  %d\n",dorptr->blknum);
             fprintf(fpw,"SSWB descriptor size  %d\n",len);
             fprintf(fpw,"Reading a Dorade sweep file\n");
          }
          else{
             printf("SSWB - #  %d\n",dorptr->blknum);
             printf("SSWB descriptor size  %d\n",len);
             printf("Reading a Dorade sweep file\n");
         }       
       }
       else{
         if(fpw)
           fprintf(fpw,"SSWB - #  %d\n",dorptr->blknum);
         else
           printf("SSWB - #  %d\n",dorptr->blknum);
      }
    }/*YES*/
  }


  if(dorptr->process == BYBLOCK){
     if(fpw) 
        fprintf(fpw,"skipping SSWB Block # %d\n",dorptr->blknum);
     else
         printf("skipping SSWB Block # %d\n",dorptr->blknum);
  }

  if(dorptr->process == BYTIME){
     if(fpw) 
        fprintf(fpw,"SSWB Block # %d\n",dorptr->blknum);
     else
         printf("SSWB Block # %d\n",dorptr->blknum);
  }


  if(dorptr->process == BYBEAM ){
     if(fpw) 
        fprintf(fpw,"SSWB Block # %d\n",dorptr->blknum);
     else
         printf("SSWB Block # %d\n",dorptr->blknum);
  }

  dorptr->sweep_file  = 1;

  rval = read(fd,array,len-8);
  if(rval <= 0) 
      return(-1);
  else 
     return(1);

}/*sswb*/
/************************************************************************/

int  read_comm(int fd,FILE *fpw,int len,int iprint)
{

  int   rval;
  char  array[MXBYTES];

  extern struct dorade_basic *dorptr;

  dorptr->blknum++;
  if(iprint == SUMM){
     if(fpw)
        fprintf(fpw,"COMM - Block # %d\n",dorptr->blknum);
     else
        printf("COMM - Block # %d\n",dorptr->blknum);
  }


  if(iprint == FULL){
     if(fpw)
        fprintf(fpw,"COMM - Block # %d  Block length = %d\n",dorptr->blknum,len);
     else
        printf("COMM - Block # %d  Block length = %d\n",dorptr->blknum,dorptr->blknum,len);
  }

  rval = read(fd,array,len-8);
  if(rval <= 0) 
      return(-1);
  else 
     return(1);
}/*comm*/

/************************************************************************/
int read_vold(int fd,FILE *fpw,int iprint,int len)
{

  int rval,i;
  char printstr[1000];
  char array[MXBYTES];
  unsigned short vold_value;
  struct vold_blk vold;

  extern struct dorade_basic *dorptr;


  printstr[0]='\0';

  rval = read(fd,array,len-8);
  if(rval <= 0){
      printf("UNABLE TO READ VOLD DESCRIPTOR\n");
     return(-1);
  }     

/* now unpack volume descriptor into discrete parts */

  vold.length = len;
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

  if(dorptr->swap == NO){
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
  dorptr->year  = vold.year;
  dorptr->month = vold.month;
  dorptr->day   = vold.day;
  dorptr->blknum++;
  dorptr->vold_cnt++;
  dorptr->radd_cnt = -1;

  if(iprint != 0)  print_vold(vold,fpw,iprint);
  return(1);
}/*read_vold*/
/************************************************************************/
/********************************************************************************/

/* The following function reads in the radar descriptor block and prints
 * out what it read in.
 */
int read_radd(int fd,FILE *fpw,int iprint,int len)

{
  int rval,i;
  char array[MXBYTES];
  int radd_cnt;
  long  tlong;
  struct radd_blk radd;

  float four_bytes_2_flt();
  int two_bytes_2_int();

  extern struct dorade_basic *dorptr;


  rval = read(fd,array,len-8);
  if(rval <= 0){
      printf("UNABLE TO READ RADD DESCRIPTOR\n");
     return(-1);
  }

  radd.length = len;
  dorptr->blknum++;
  dorptr->radd_cnt++;


/* unpack array into its discrete parts (variables) */
  for(i = 0; i < 8; i++) {
    radd.radnam[i] = ' '; 
    if(array[i] != ' ' && array[i] != '\0') 
       radd.radnam[i] = array[i];
       dorptr->radar_name[dorptr->radd_cnt][i] = array[i];
  }

  if(dorptr->swap == 0){
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


  dorptr->radar_constant[dorptr->radd_cnt]   = radd.radcon;
  dorptr->nyquist_velocity[dorptr->radd_cnt] = radd.nyq;
  if(dorptr->radd_cnt == 0){
     dorptr->radar_type       = radd.rad_type;
     dorptr->scan_mode        = radd.scan_mode;
     dorptr->total_parameters = radd.tot_par;
     dorptr->data_compression = radd.data_comp;
     dorptr->lat              = radd.rad_lat;
     dorptr->lon              = radd.rad_long;
     dorptr->alt              = radd.rad_alt;
     dorptr->num_frequencies  = radd.num_freq;
     dorptr->num_ipulse       = radd.num_ip;
  }
 
  if(radd.tot_par > MXFLD){
     printf("MAX NUMBER OF SPRINT ALLOWED PARAMETERS IS %d.\n",MXFLD);
     printf("NUMBER OF PARAMETERS ACCORDING TO RADD DESCRIPTOR IS %d\n",radd.tot_par);
     exit(0);
  }


  if(iprint == FULL && dorptr->process == BYBLOCK){
     if(fpw)
        fprintf(fpw,"Skipping RADD block # %d\n",dorptr->blknum);
     else
        printf("Skipping RADD block # %d\n",dorptr->blknum);
  }
  else{
     if(iprint != 0) 
         print_radd(radd,fpw,iprint);
  }


  return (1);
}/*read_radd*/
/************************************************************************/
/* The following function reads in the dorade parameter descriptor
 * block and prints out what it read in.
 */
int read_parm(int fd ,FILE *fpw,int iprint,int parm_cnt,int len)
{
  char array[MXBYTES];
  int rval, i, numflds;
  float four_bytes_2_flt();
  char printstr[1000];
  struct parm_blk parm;

  extern struct dorade_basic *dorptr;


  printstr[0]='\0';

  rval = read(fd,array,len-8);
  if(rval <= 0){
      printf("UNABLE TO READ PARAMETER DESCRIPTOR\n");
     return(-1);
  }    

  dorptr->blknum++;

  parm.length = len;
/* now unpack all the elements of this block into the structure */
  i = 0;
  dorptr->field_name[parm_cnt][8] = '\0';
  while(i < 8){
    dorptr->field_name[parm_cnt][i]= ' ';
    parm.name[i] = ' ';
    if(array[i] != ' ' && array[i] != '\0'){
       parm.name[i] = array[i];
       dorptr->field_name[parm_cnt][i]= array[i];
    }
    i++;
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

  if(dorptr->swap == 0){
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
     
  if(parm_cnt == 0) dorptr->bad = parm.bad;
  dorptr->scale[dorptr->radd_cnt][parm_cnt] = parm.scale;
  dorptr->offset[dorptr->radd_cnt][parm_cnt] = parm.offset;
  dorptr->fld_typ[dorptr->radd_cnt][parm_cnt] = parm.parm_type;
  
  if(iprint == FULL && dorptr->process == BYBLOCK){
     if(fpw)
        fprintf(fpw,"Skipping PARM block # %d\n",dorptr->blknum);
     else
        printf("Skipping PARM block # %d\n",dorptr->blknum);
  }
  else{
     if(iprint != 0) print_parm(parm,fpw,iprint);
  }

  return (1);
}/*read_parm*/

/********************************************************************************/
int read_celv(int fd,FILE *fpw,int iprint,int len)
{
  char array[MXBYTES];
  int rval,i, slen;
  struct celv_blk celv;

  float dist,spacing;
  float four_bytes_2_flt();


  extern struct dorade_basic *dorptr;


  rval = read(fd,array,len-8);
  if(rval <= 0){
      printf("UNABLE TO READ CELL SPACING DESCRIPTOR\n");
     return(-1);
  }    

  celv.length = len;
  dorptr->blknum++;
  /* now unpack the elements of this block into the structure */
  if(dorptr->swap == 0){
     celv.num_cells   = four_bytes_2_int(array[0],array[1],array[2],array[3]);
     celv.dist_to_fir = four_bytes_2_flt(array[4],array[5],array[6],array[7]);
     dist             = four_bytes_2_flt(array[8],array[9],array[10],
                        array[11]);
     spacing     = dist - celv.dist_to_fir;
  }
  else{
     celv.num_cells   = four_bytes_2_int(array[3],array[2],array[1],array[0]);
     celv.dist_to_fir = four_bytes_2_flt(array[7],array[6],array[5],array[4]);
     dist             = four_bytes_2_flt(array[11],array[10],array[9],
                        array[8]);
     spacing     = dist - celv.dist_to_fir;
  }     
  /* print out the info now */

  if(iprint == FULL && dorptr->process == BYBLOCK){
     if(fpw)
        fprintf(fpw,"Skipping CELV block # %d\n",dorptr->blknum);
     else
        printf("Skipping CELV block # %d\n",dorptr->blknum);
  }
  else{
     if(iprint != 0) print_celv(celv,fpw,iprint,spacing);
  }

  if(dorptr->radd_cnt  == 0){
      dorptr->gate_spacing = spacing;
      dorptr->range_first_gate = celv.dist_to_fir;
  }
     

  return (1);
}/*read_celv*/

/******************************************************************************************/
/* The following function reads in the correction factor descriptor
 * block and prints out what it read in.
 */

int read_cfac(int fd, FILE *fpw,int iprint,int len)
{
  int rval;
  int index;
  char printstr[1000];
  char array[MXBYTES];
  float four_bytes_2_flt();
  float swap_floats();
  struct cfac_blk cfac;


  printstr[0]='\0';

  rval = read(fd,array,len-8);
  if(rval <= 0){
      printf("UNABLE TO READ CORRECTION FACTOR  DESCRIPTOR\n");
     return(-1);
  }    
    
  cfac.length = len;
  dorptr->blknum++;

  if(dorptr->swap == NO){
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

  if(iprint == FULL && dorptr->process == BYBLOCK){
     if(fpw)
        fprintf(fpw,"Skipping CFAC block # %d\n",dorptr->blknum);
     else
        printf("Skipping CFAC block # %d\n",dorptr->blknum);
  }
  else{
     if(iprint != 0) print_cfac(cfac,fpw,iprint);
  }

  dorptr->range_delay = cfac.rng_del;
  return(1);
}/*read cfac*/

/************************************************************************
                       DORADE BEAM READING ROUTINES            
************************************************************************/
/* The following function reads in the sweep info block and prints
 * it out.
 */
struct swib_blk read_swib(struct swib_blk swib, int fd, FILE *fpw,
                          int iprint,int len, int *error)
{

  float four_bytes_2_flt();
  char printstr[1000];
  char array[MXBYTES];
  int  i;
  int  rval;

  extern struct dorade_basic *dorptr;

  /*end declarations*/

  *error = 0;
  rval = read(fd,array,len-8);
  if(rval <= 0){
      printf("UNABLE TO READ SWIB DESCRIPTOR\n");
      *error = 1;
      return(swib);
  }    

  swib.length = len;
  dorptr->ryib_cnt = 0;
  dorptr->swib_cnt++;
  dorptr->blknum++;
  dorptr->bad_count = 0;
  if(dorptr->current_radar[0] == '\0') 
       dorptr->last_radar[0] = '\0';
  else {
       dorptr->last_radar[0] = dorptr->current_radar[0];
       dorptr->last_radar[1] = dorptr->current_radar[1];
       dorptr->last_radar[2] = dorptr->current_radar[2];
       dorptr->last_radar[3] = dorptr->current_radar[3];
       dorptr->last_radar[4] = dorptr->current_radar[4];
       dorptr->last_radar[5] = dorptr->current_radar[5];
       dorptr->last_radar[6] = dorptr->current_radar[6];
       dorptr->last_radar[7] = dorptr->current_radar[7];       
  }

  i = 0;
  while(array[i] != ' ' && array[i] != '\0' && i < 8){
     dorptr->current_radar[i] = array[i];
     swib.radname[i] = array[i];
     i++;
  }

  if(dorptr->swap == 0){ 
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


  dorptr->sweep_num++;
  dorptr->num_rays    =  swib.nray;
  dorptr->fixed_angle[dorptr->radd_cnt] =  swib.fix_ang;
  if(iprint != 0) print_swib(swib,fpw,iprint);
  return(swib);

}/*read_swib*/
/************************************************************************/
/* the following function reads in the ray info block and prints out
 * what it read in.
 */
struct ryib_blk read_ryib(struct ryib_blk ryib, 
                          int fd, 
                          FILE *fpw,
                          int iprint, 
                          int len, 
                          int *error,
                          double reqtime)
{
  char   array[MXBYTES];
  int    rval,imon,iday;
  int    printinfo;
  double seconds,beam_time,usert;
  extern struct dorade_basic *dorptr;

  float four_bytes_2_flt();

  *error = 0;
  ryib.length = len;
  dorptr->bytesbeam = dorptr->bytesbeam + len;
  strcpy(ryib.name,"RYIB");
  rval = read(fd,array,len-8);
  if(rval <= 0){
      printf("UNABLE TO READ RYIB DESCRIPTOR\n");
      *error = 1;
      return;
  }   

  dorptr->ryib_cnt++;

  if(dorptr->correct_rad == NO && iprint != 0){
     printf("Skipping beam %d wrong radar\n",dorptr->ryib_cnt);
     return(ryib);
  }

  dorptr->blknum++;
  dorptr->bad_beam = NO;


/* unpack array into its discrete parts (variables) */
  if(dorptr->swap == 0){
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

  
  if(ryib.msec < 10) seconds = ryib.sec + ryib.msec/10.;
  else if(ryib.msec >= 10 && ryib.msec < 100) ryib.sec + ryib.msec/100.;
  else ryib.sec + ryib.msec/1000.;
  seconds = ryib.sec + ryib.msec/1000.;
  beam_time = ryib.hour*10000. + ryib.min*100. + seconds;

  if(dorptr->correct_rad == YES){
     if(beam_time < reqtime){
        if(fpw)
           fprintf(fpw,"\nBeam %d  Time %d:%d:%d:%d \n",
               dorptr->ryib_cnt,ryib.hour,ryib.min,ryib.sec,ryib.msec);
        else
           printf("\nBeam %d  Time %d:%d:%d:%d\n",
               dorptr->ryib_cnt,ryib.hour,ryib.min,ryib.sec,ryib.msec);
     }
     else{
        if(dorptr->process != BYBEAM) 
           print_ryib(ryib,fpw,iprint);
        else{
          if(fpw){
            if(iprint == SUMM) fprintf(fpw,"\nSkipping Beam %d\n",dorptr->ryib_cnt);
            if(iprint == FULL) fprintf(fpw,"\nSkipping RYIB Block # %d\n",dorptr->blknum);
	  }
          else{
            if(iprint == SUMM) printf("\nSkipping Beam %d\n",dorptr->ryib_cnt); 
            if(iprint == FULL) printf("Skipping RYIB Block # %d\n",dorptr->blknum);
	  }
        }
     }
  }/*end if for correct_rad*/
 
 /*
  *Since we have read a ryib set the rdatcnt to 0 and end beam = no
  */
  dorptr->rdatcnt     = 0;
  dorptr->end_beam    = NO;


  if((beam_time >= reqtime) && (dorptr->process == BYTIME) && (dorptr->correct_rad == YES)) 
      dorptr->process = YES;


  dorptr->asib = 0;
  return ryib;
}/*read_ryib*/
/************************************************************************/
/* The following function reads in the platform (aircraft/ship) info
 * block and prints it out.
 */
struct asib_blk read_asib(struct asib_blk asib,int fd,FILE *fpw,int iprint, 
                          int len,int *error)
{
  int rval,printtype;
  char array[MXBYTES];
  float four_bytes_2_flt();
  extern struct dorade_basic *dorptr;

  void print_asib();


  *error = 0;
  asib.length = len;
  rval = read(fd,array,len-8);
  if(rval <= 0){
      printf("UNABLE TO READ SWIB DESCRIPTOR\n");
      *error = 1;
      return;
  }    


  if(dorptr->correct_rad == NO) return(asib);
  dorptr->bytesbeam = dorptr->bytesbeam + len;

  dorptr->asib = 1;
/* unpack array into its parts */
  if(dorptr->swap == NO){
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


  if(dorptr->end_beam == YES){
     dorptr->blknum++;
     dorptr->bad_count++;
     dorptr->bad_beam = YES;
     if(fpw){
        fprintf(fpw,"\n");
        fprintf(fpw,"++++WARNING++++: We have an un-readable RYIB descriptor in block # %d.\n",dorptr->blknum);
        fprintf(fpw,"The azimuth,elevation and time associated with the following beam is not available++++++++\n");
        fprintf(fpw,"\n");
     }
     else{
        printf("++++WARNING++++: We have an un-readable RYIB descriptor in block # %d.\n",dorptr->blknum);
        printf("The azimuth,elevation and time associated with the following beam is not available++++++++\n");
     }

  }

  dorptr->blknum++;
  dorptr->end_beam  = NO;


  if(iprint == FULL && dorptr->process == BYBLOCK){
     if(fpw)
       fprintf(fpw,"Skipping ASIB Block # %d\n",dorptr->blknum);
     else
       printf("Skipping ASIB Block # %d\n",dorptr->blknum);
  }
  if(dorptr->process == YES){
     if(iprint != 0) print_asib(asib,fpw,iprint);
  }

 /*
  *Since we have read an asib set the rdatcnt to 0.
  */
  dorptr->rdatcnt = 0;

	       

  return asib;
}/*read_asib*/

/************************************************************************/
int read_rdat(int fd,
              FILE *fpw,
              int iprint,
              int iunpck,
              int len,
              float flddat[MXFLD][MXGAT])
{
  int    index,cindex,fnum,num_bytes,type;
  int    ngates,nlen,rval,i,j,n;
  int    final_bad_count = 0;
  int    bad,it1;
  int    radnum,printtype;
  int    requested_field = NO;
  char   name[8];
  short  sdata[MXGAT],cdata[MXGAT];
  char   array[MXBYTES];


  extern struct dorade_basic *dorptr;

  if(dorptr->process != YES) printtype = 0;
  else printtype = iprint;


  /*
   *Now read in the rest of the radar descriptor.
   */
   rval = read(fd,array,len-8);
   if (rval <= 0) {
      return(-1);
   }

  if(dorptr->correct_rad == YES) dorptr->blknum++;
  if(dorptr->correct_rad == NO)  return(1);
  if(dorptr->process != YES)     return(1);


  dorptr->bytesbeam = dorptr->bytesbeam + len;

  nlen = 0;
  for(index = 0; index < 8; index++){
    if(array[index] != ' ' && array[index] != '\0'){
         name[index] = array[index];
         nlen++;
	 }
  }
  name[nlen] = '\0';

  if(iprint == FULL && dorptr->process == YES){
     if(fpw){
       fprintf(fpw,"\n        +++Radar Data Block+++\n");
       fprintf(fpw,"length of rdat descriptor for %s %d\n",name,len);
     }
     else{
       printf("\n        +++Radar Data Block+++\n");
       printf("length of rdat descriptor for %s %d\n",name,len);
     }
  }
  if(iprint == FULL && dorptr->process == NO){
     if(fpw){
         fprintf(fpw,"skipping RDAT Block # %d\n",dorptr->blknum);
     }
     else{
         printf("skipping RDAT Block # %d\n",dorptr->blknum);
     }
  }

  if(iprint == FULL && dorptr->process == BYBLOCK){
     if(fpw){
         fprintf(fpw,"skipping RDAT Block # %d\n",dorptr->blknum);
     }
     else{
         printf("skipping RDAT Block # %d\n",dorptr->blknum);
     }
  }

  /*
   *Make sure that the field is on the tape.
   */
  fnum = -1;
  for (index = 0; index < dorptr->total_parameters; index++) {
    if(strncmp(name,dorptr->field_name[index],nlen) == 0){
             fnum = index;
             break;
    }
  }

  if (fnum == -1) {
    printf("\n   +++Error. Could not find field %s in list+++\n",
	   name);
    exit(1);
  }     
  
  for(index = 0; index < MXGAT; index++) flddat[fnum][index] = -32768.;

  if(dorptr->num_req_flds != -1){
     for(index = 0; index < dorptr->total_parameters; index++){     
        if(strncmp(name,dorptr->req_fld[index],nlen) == 0) requested_field = YES;
     }
     if(requested_field == NO) return;
  }
  

  if(iprint == SUMM && dorptr->process == YES){
     if(fpw)
        fprintf(fpw,"RDAT - #  %d  :Parm name  %8s\n",dorptr->blknum,name);
     else
        printf("RDAT - #  %d  :Parm name  %8s\n",dorptr->blknum,name);
  }


  nlen = 0;
  for(index = 0; index < 8; index++){
    if(dorptr->current_radar[index] != ' ' &&  dorptr->current_radar[index] != '\0'){
         nlen++;
    }
  }


  if(dorptr->radd_cnt == 0) radnum = 0;
  else{
      if(strncmp(dorptr->current_radar,dorptr->radar_name[0],nlen) == 0) radnum = 0;
      if(strncmp(dorptr->current_radar,dorptr->radar_name[1],nlen) == 0) radnum = 1;
  }

  type = dorptr->fld_typ[radnum][fnum];
  switch (type)
    {
    case 1:{
      num_bytes = 1;
      break;
    }
    case 2:{
      num_bytes = 2;
      break;
    }
    case 3:{
      num_bytes = 3;
      break;
    }
    case 4:{
      num_bytes = 4;
      break;
    }
    default:{
      printf("\n   Error: unrecognized field length. type = %d\n", type);
      exit(-1);
      break;
    }
    }/*end switch*/

   ngates = (len-16)/num_bytes;
   dorptr->ngates = ngates;
   dorptr->rdatcnt++;
   if (dorptr->rdatcnt <= dorptr->total_parameters){
       dorptr->ngates = MAX(dorptr->ngates, ngates);
   }
   else {
       printf("TOO MANY DATA DESCRIPTORS ARE PRESENT\n");
       printf("We have read in %d according to the RADD descriptor there are only suppose to be %d\n",
               dorptr->rdatcnt,dorptr->total_parameters);
       exit(-1);
   }



  if(iprint == FULL){
     if(fpw){
        fprintf(fpw,"RDAT block #        %d\n",dorptr->blknum);
        fprintf(fpw,"RDAT count for this beam = %d   . Should not be more than %d\n",
                dorptr->rdatcnt,dorptr->total_parameters);
     }
     else{
        printf("RDAT block #        %d\n",dorptr->blknum);
        printf("RDAT count for this beam = %d   . Should not be more than %d\n",
                dorptr->rdatcnt,dorptr->total_parameters);
     }
  }   
  /*If the data compression flag is turned on then we need to uncompress
   *the data. sdata is a short array which will contain the uncompressed data.
   *cdata is the character array that contains the original compressed data.
   */

  if(dorptr->data_compression == 1){
    if(iprint == FULL) printf("Data compression is ON for this rdat\n");

    for(j = 0; j < len-8; j++){
        cdata[j] = two_bytes_2_int(array[8+j*2], array[9+j*2]);
    }


    bad = dorptr->bad;
    if(dorptr->swap == NO){
        n = uncompress(cdata,sdata,bad,&final_bad_count,MXGAT);
        if(n == -1){
               printf("WARNING DATA COMPRESSION FLAG IS ON BUT WE ARE UNABLE TO UNCOMPRESS THE DATA\n");
	}
    } 
    else{
        n = uncompress(cdata,sdata,bad,&final_bad_count,MXGAT);
        if(n == -1){
               printf("WARNING DATA COMPRESSION FLAG IS ON BUT WE ARE UNABLE TO UNCOMPRESS THE DATA\n");
	}
    }
    
    if(n != -1){
      dorptr->ngates = n;
      ngates = n;
    }
    else dorptr->data_compression = 0;

  }/*dorptr->data_compression*/


  if(dorptr->ngates > MXGAT){
     dorptr->ngates = MXGAT-1;
     ngates = MXGAT-1;
  } 
  if(iprint == FULL){
     if(fpw)
       fprintf(fpw,"Number of gates calculated in read_rdat  %d\n",dorptr->ngates);
     else
       printf("Number of gates calculated in read_rdat  %d\n",dorptr->ngates);
  }


  /*
   *We can now put the data into the flddat array.  If the data has gone through
   *the uncompression routines it will be in sdata if it has not it will still
   *be in array.
   *iunpck is a flag for unpacking(unscaling and unbiasing) the data.  If we 
   *are not at the correct time there is no reason to unpack the data since
   *this takes time.
   */
    iunpck = 2;
    if (iunpck) {
    if (iunpck == 1) { /* leave in scaled format */
      for (j = 0; j < ngates; j++) {
        if(dorptr->data_compression != 1){/*no compression*/
	   flddat[fnum][j] = (float)two_bytes_2_int(array[8+j*2], array[9+j*2]);
        }
        else{
	   flddat[fnum][j] = sdata[j];/*uncompressed data*/
	}
      }
    }
    else { /* unscale and unbias the data */
      for (j = 0; j < ngates; j++) {
        if(dorptr->data_compression != 1){ 
           if(dorptr->swap == NO) it1 = two_bytes_2_int(array[8+j*2], array[9+j*2]);
           if(dorptr->swap == YES) it1 = two_bytes_2_int(array[9+j*2],array[8+j*2]);
	   /*it1 = two_bytes_2_int(array[8+j*2], array[9+j*2]);*/
        }
        else{
           it1 = sdata[j];
	}
	if (dorptr->scale[dorptr->radd_cnt][fnum] != 0.0 && it1 != bad) 
	  flddat[fnum][j] = ((float)it1 - dorptr->offset[dorptr->radd_cnt][fnum])/dorptr->scale[dorptr->radd_cnt][fnum];

      }/*end of the for j loop*/
     }/*end of else*/

    }

    if(iprint == FULL){ 
      if(fpw){
         fprintf(fpw,"\n        +++Radar Data Block+++\n");
         fprintf(fpw,"\n\n    field=%8s\n",name);
         fprintf(fpw,"data values = \n");
         for (j = 0 ; j < ngates; j++) {
	      fprintf(fpw,"%9.2f ",flddat[fnum][j]);
	      if (((j+1) % 10) == 0) fprintf(fpw,"\n");   
         }
       if(iprint == FULL) fprintf(fpw,"\n");
      }
      else{
         printf("\n        +++Radar Data Block+++\n");
         printf("\n\n    field=%8s\n",name);
         printf("data values = \n");
         for (j = 0 ; j < ngates; j++) {
	      printf("%9.2f ",flddat[fnum][j]);
	      if (((j+1) % 10) == 0) printf("\n");   
         }
       if(iprint == FULL) printf("\n");
      }
    }
  return(1);
      
}/*read_rdat*/
     
/**********************************************************************************/
/*
 * The following function reads in blocks, determines the type (name) and
 * calls a function for further processing, depending on the name. This
 * function assumes that the input stream is positioned at the start of
 * a dorade block.
 */
void read_block(int    fd,
                FILE   *fpw,
                struct swib_blk *swib,
                struct ryib_blk *ryib,
                struct asib_blk *asib,
                int    iprint, 
                float  flddat[MXFLD][MXGAT],
                int    *istat,
                char   cfldnam[MXFLD][8],
                int    *block,
                int    parm_cnt,
                int    process,
                double reqtime)
{

  char ch,array[8],junk[8];
  int  rval,test_char;
  int  found_descriptor = 0;
  int  byte_count = 0;
  int  error,index,chars_to_read;
  int  len;
  int  process_beam,iunpck;

  extern struct dorade_basic *dorptr;

  int get_descriptor_length();

  /*END DECLARATIONS*/

  while(found_descriptor == 0){
     rval = read(fd,&ch,1);
     if(rval <= 0){
        *istat = 3; /*error reading file*/
         rval = read(fd,&ch,1);
         if(rval < 0){
            *istat = 3; /*end of file*/
            printf("PHYSICAL END OF FILE MARKER REACHED\n");
	}
        return;
     }
     byte_count = byte_count + 4;
     if(byte_count >= MXBYTES){
        printf("UNABLE TO LOCATE A DORADE DESCRIPTOR\n");
        *istat = 4;
        return;
     }
     test_char = (int)ch;
     if(test_char >= 65 && test_char <= 90){
           rval = read(fd,junk,3);
           array[0] = ch;
           array[1] = junk[0];
           array[2] = junk[1];
           array[3] = junk[2];
           /*printf("%c%c%c%c\n",array[0],array[1],array[2],array[3]);*/
           if(array[0] == 'S' && array[1] == 'S' && array[2] == 'W' && array[3] == 'B'){
	     /*              printf("READING A DORADE SWEEP FILE\n");*/
              rval = read(fd,junk,4);
              array[4] = junk[0];
              array[5] = junk[1];
              array[6] = junk[2];
              array[7] = junk[3];
              len = get_descriptor_length(array);

              dorptr->process = process;
              if(read_sswb(fd,fpw,len,iprint) == -1){
                 printf("UNABLE TO READ SSWB BLOCK\n");
                 *istat = 3;
	      }
              *block = SSWB;
              found_descriptor = 1;
              dorptr->sweep_file = 1;
           }
           else if(array[0] == 'V' && array[1] == 'O' && array[2] == 'L' && array[3] == 'D'){
              rval = read(fd,junk,4);
              array[4] = junk[0];
              array[5] = junk[1];
              array[6] = junk[2];
              array[7] = junk[3];
              len = get_descriptor_length(array);
             dorptr->process = process;
             if(read_vold(fd,fpw,iprint,len) == -1){
                 printf("UNABLE TO READ VOLD BLOCK\n");
                 *istat = 3;
	     }            
             *block = VOLD;
             found_descriptor = 1;
           }

           else if(array[0] == 'C' && array[1] == 'O' && array[2] == 'M' && array[3] == 'M'){
              rval = read(fd,junk,4);
              array[4] = junk[0];
              array[5] = junk[1];
              array[6] = junk[2];
              array[7] = junk[3];
              len = get_descriptor_length(array);
             if(read_comm(fd,fpw,len,iprint) == -1){
                 printf("UNABLE TO READ COMM BLOCK\n");
                 *istat = 3;
	     }
             *block = COMM;
             found_descriptor = 1;
           }

           else if(array[0] == 'R' && array[1] == 'A' && array[2] == 'D' && array[3] == 'D'){
              rval = read(fd,junk,4);
              array[4] = junk[0];
              array[5] = junk[1];
              array[6] = junk[2];
              array[7] = junk[3];
              len = get_descriptor_length(array);
             dorptr->process = process;
             if(read_radd(fd,fpw,iprint,len) == -1){
                 printf("UNABLE TO READ RADD BLOCK\n");
                 *istat = 3;
	    }
             *block = RADD;
             found_descriptor = 1;
           }

           else if(array[0] == 'P' && array[1] == 'A' && array[2] == 'R' && array[3] == 'M'){
              rval = read(fd,junk,4);
              array[4] = junk[0];
              array[5] = junk[1];
              array[6] = junk[2];
              array[7] = junk[3];
              len = get_descriptor_length(array);
              dorptr->process = process;
              if(read_parm(fd,fpw,iprint,parm_cnt,len) == -1){
                 printf("UNABLE TO READ PARAMETER BLOCK\n");
                 *istat = 3;
	    }
              *block = PARM;
              found_descriptor = 1;
           }   

           else if(array[0] == 'C' && array[1] == 'E' && array[2] == 'L' && array[3] == 'V'){
              rval = read(fd,junk,4);
              array[4] = junk[0];
              array[5] = junk[1];
              array[6] = junk[2];
              array[7] = junk[3];
              len = get_descriptor_length(array);
              dorptr->process = process;
              if(read_celv(fd,fpw,iprint,len) == -1){
                 printf("UNABLE TO READ CELL VECTOR BLOCK\n");
                 *istat = 3;
	    }      
              *block = CELV;
              found_descriptor = 1;
           }   
 
           else if(array[0] == 'C' && array[1] == 'F' && array[2] == 'A' && array[3] == 'C'){
              rval = read(fd,junk,4);
              array[4] = junk[0];
              array[5] = junk[1];
              array[6] = junk[2];
              array[7] = junk[3];
              len = get_descriptor_length(array);
             dorptr->process = process;
              if(read_cfac(fd,fpw,iprint,len) == -1){
                 printf("UNABLE TO READ CELL VECTOR BLOCK\n");
                 *istat = 3;
	      }                    
              *block = CFAC;
              found_descriptor = 1;
           }   

           else if(array[0] == 'C' && array[1] == 'S' && array[2] == 'P' && array[3] == 'D'){
              rval = read(fd,junk,4);
              array[4] = junk[0];
              array[5] = junk[1];
              array[6] = junk[2];
              array[7] = junk[3];
              len = get_descriptor_length(array);
              printf("cspd block\n");
              found_descriptor = 1;
           }   

           else if(array[0] == 'S' && array[1] == 'W' && array[2] == 'I' && array[3] == 'B'){
	     /*              printf("found a swib\n");*/
              rval = read(fd,junk,4);
              array[4] = junk[0];
              array[5] = junk[1];
              array[6] = junk[2];
              array[7] = junk[3];
              len = get_descriptor_length(array);
	     if(dorptr->ryib_cnt != 0 && dorptr->correct_rad == YES && iprint != 0){
	        if(fpw){
                   fprintf(fpw,"Total good beams in sweep %d\n",dorptr->ryib_cnt);
                   fprintf(fpw,"Total bad beams in sweep  %d\n",dorptr->bad_count);
	        }  
		else{
                   printf("Total good beams in sweep %d\n",dorptr->ryib_cnt);
                   printf("Total bad beams in sweep  %d\n",dorptr->bad_count);
		}   
	     }     
              *swib = read_swib(*swib,fd,fpw,iprint,len,&error);
              if(error  == 1){
                 printf("UNABLE TO READ SWEEP INFORMATION BLOCK\n");
                 *istat = 3;
	      }  
              if(process == BYBLOCK  && iprint == FULL){
                 if(fpw)
                    fprintf(fpw,"Skipping SWIB  Block # %d\n",dorptr->blknum);
                 else
                    printf("Skipping SWIB Block # %d\n",dorptr->blknum);
	      }
              *block = SWIB;
              found_descriptor = 1;
           }   

           else if(array[0] == 'R' && array[1] == 'Y' && array[2] == 'I' && array[3] == 'B'){
              rval = read(fd,junk,4);
              array[4] = junk[0];
              array[5] = junk[1];
              array[6] = junk[2];
              array[7] = junk[3];
              len = get_descriptor_length(array);
              dorptr->process = process;
              *ryib = read_ryib(*ryib,fd,fpw,iprint,len,&error,reqtime);
              if(error == 1){
                 printf("UNABLE TO READ RYIB INFORMATION BLOCK\n");
                 *istat = 3;
	      }
              *block = RYIB;
              found_descriptor = 1;
           } 
           else if(array[0] == 'A' && array[1] == 'S' && array[2] == 'I' && array[3] == 'B'){
              rval = read(fd,junk,4);
              array[4] = junk[0];
              array[5] = junk[1];
              array[6] = junk[2];
              array[7] = junk[3];
              len = get_descriptor_length(array);
              *asib = read_asib(*asib,fd,fpw,iprint,len,&error);
              *block = ASIB;
              found_descriptor = 1;
           } 

           else if(array[0] == 'R' && array[1] == 'D' && array[2] == 'A' && array[3] == 'T'){
              rval = read(fd,junk,4);
              array[4] = junk[0];
              array[5] = junk[1];
              array[6] = junk[2];
              array[7] = junk[3];
              len = get_descriptor_length(array);
              iunpck = 0;
              if(process == YES) iunpck = 1;
              if(read_rdat(fd,fpw,iprint,iunpck,len,flddat) == -1){
                 printf("UNABLE TO READ RADAR DATA BLOCK\n");
                 *istat = 3;
	      }        
              *block = RDAT;
              found_descriptor = 1;
           } 

           else if(array[0] == 'F' && array[1] == 'R' && array[2] == 'A' && array[3] == 'D'){
              rval = read(fd,junk,4);
              array[4] = junk[0];
              array[5] = junk[1];
              array[6] = junk[2];
              array[7] = junk[3];
              len = get_descriptor_length(array);
              *block = RDAT;
              found_descriptor = 1;
           } 

           else if(array[0] == 'R' && array[1] == 'K' && array[2] == 'T' && array[3] == 'B'){
              rval = read(fd,junk,4);
              array[4] = junk[0];
              array[5] = junk[1];
              array[6] = junk[2];
              array[7] = junk[3];
              len = get_descriptor_length(array);
              *block = END;
              printf("END OF SWEEP FILE HAS BEEN REACHED\n");
              found_descriptor = 1;
              *istat = ENDSWEEP;
           } 

           else if(array[0] == 'N' && array[1] == 'U' && array[2] == 'L' && array[3] == 'L'){
              rval = read(fd,junk,4);
              array[4] = junk[0];
              array[5] = junk[1];
              array[6] = junk[2];
              array[7] = junk[3];
              len = get_descriptor_length(array);
              *block = END;
              found_descriptor = 1;
              *istat = ENDSWEEP;
           } 

           else{
             printf("in the else\n");
	     /*We have encountered an unknown Dorade descriptor*/
             if(fpw){ 
                fprintf(fpw,"Unknown Dorade descriptor located %c%c%c%c\n",
                        array[0],array[1],array[2],array[3]);
	     }
             else
                printf("Unknown Dorade descriptor located %c%c%c%c\n",
                        array[0],array[1],array[2],array[3]);
                
             if(array[1] == 'R' && array[2] == 'Y' && array[3] == 'I'){
                 rval = read(fd,junk,5); 
                 array[0] = 'R';
                 array[1] = 'Y';
                 array[2] = 'I';
                 array[3] = 'B';
                 array[4] = junk[1];
                 array[5] = junk[2];
                 array[6] = junk[3];
                 array[7] = junk[4];
                 len = get_descriptor_length(array);            
                 read_ryib(*ryib,fd,fpw,iprint,len,&error,reqtime);
                 if(error == 1){
                    printf("UNABLE TO READ RYIB INFORMATION BLOCK\n");
                    *istat = 3;
	         }
                 *block = RYIB;
                 found_descriptor = 1;
                 dorptr->end_beam = NO;
	     }
	     else if(array[2] == 'R' && array[3] == 'Y'){
                 rval = read(fd,junk,6); 
                 array[0] = 'R';
                 array[1] = 'Y';
                 array[2] = 'I';
                 array[3] = 'B';
                 array[4] = junk[2];
                 array[5] = junk[3];
                 array[6] = junk[4];
                 array[7] = junk[5];
                 len = get_descriptor_length(array);            
                 read_ryib(*ryib,fd,fpw,iprint,len,&error,reqtime);
                 if(error == 1){
                    printf("UNABLE TO READ RYIB INFORMATION BLOCK\n");
                    *istat = 3;
	         }
                 *block = RYIB;
                 found_descriptor = 1;
                 dorptr->end_beam = NO;
             }
             else if(array[3] == 'R'){
                 rval = read(fd,junk,6); 
                 array[0] = 'R';
                 array[1] = 'Y';
                 array[2] = 'I';
                 array[3] = 'B';
                 array[4] = junk[2];
                 array[5] = junk[3];
                 array[6] = junk[4];
                 array[7] = junk[5];
                 len = get_descriptor_length(array);            
                 read_ryib(*ryib,fd,fpw,iprint,len,&error,reqtime);
                 if(error == 1){
                    printf("UNABLE TO READ RYIB INFORMATION BLOCK\n");
                    *istat = 3;
	         }
                 *block = RYIB;
                 found_descriptor = 1;
                 dorptr->end_beam = NO;
	     }
	 }
     }/*end if for test_char*/   
  }/*end the while*/

}/*read_block*/

/************************************************************************/
int assemble_beam(fd, fpw, swib, ryib, asib, iprint, flddat, istat, 
                  cfldnam, process, reqtime, reqrad)
     int    fd;
     FILE   *fpw;
     struct swib_blk *swib;
     struct ryib_blk *ryib;
     struct asib_blk *asib;
     int    iprint;
     float  flddat[MXFLD][MXGAT];
     int    *istat;
     char   cfldnam[MXFLD][8];
     int    process;
     double reqtime;
     char   reqrad[9];
{

  char array[MXBYTES];
  extern struct dorade_basic *dorptr;
  int rval,error,done_reading = 0;
  int data_cnt;
  int radnum,j;
  int number_of_rdats = 0;
  int read_the_header = 0;
  int block = 0;
  int parm_cnt = 0;
  int rdat_cnt = 0;
  int index,lenname;
  int blocks_read = 0;
  int read_a_swib = NO;
  
  void check_radar_name();



 read_block(fd, fpw, swib, ryib, asib,
            iprint,flddat, istat, cfldnam,
            &block, parm_cnt, process,
            reqtime);


 if(block == VOLD) *istat = NEWVOL;

 if(block == SWIB){
           read_a_swib = YES;
           if(reqrad[0] == ' ' || reqrad[0] == '\0'){
              dorptr->correct_rad = YES;
	   }
           else{
              lenname = 0;
              for(index = 0; index < 8; index++){
                  if(reqrad[index] != ' ' && reqrad[index] != '\0') lenname++;
              }
              if (strncmp(swib->radname, reqrad, lenname) == 0){
                 dorptr->correct_rad = YES;
	      }
              else{
                 dorptr->correct_rad = NO; 
	      }
	   }
  }/*SWIB*/

 /*
  *Read until we find a beam.
  */ 
  while(block < END){
        read_block(fd, fpw, swib, ryib, asib, 
                   iprint,flddat, istat, cfldnam, 
                   &block, parm_cnt, process, reqtime);

        blocks_read++;
        if(blocks_read > MXBLOCKS){
           printf("UNABLE TO FIND A DORADE BEAM\n");
           *istat = 3;
           return;
	}

        if(block == VOLD) *istat = NEWVOL;

        if(block == SWIB){
           read_a_swib = YES;
           if(reqrad[0] == ' ' || reqrad[0] == '\0'){
              dorptr->correct_rad = YES;
	   }
           else{
              lenname = 0;
              for(index = 0; index < 8; index++){
                  if(reqrad[index] != ' ' && reqrad[index] != '\0') lenname++;
              }
              if (strncmp(swib->radname, reqrad, lenname) == 0){
                 dorptr->correct_rad = YES;
	      }
              else{
                 check_radar_name(reqrad,lenname);
                 if(dorptr->sweep_file == 1){
                    dorptr->correct_rad = YES; 
		 }
                 else
                    dorptr->correct_rad = NO; 
	      }
	   }
	}/*SWIB*/


        if(block == PARM) 
           parm_cnt++;
        else 
           parm_cnt = 0;
 

        if(block == ASIB){
           rdat_cnt = 0;
	}

        if(block == RYIB){
           rdat_cnt = 0;
	}

        if(block == RDAT) rdat_cnt++;


        if(block == END){
           *istat = ENDSWEEP;
           return(1);
	}

        if(rdat_cnt == dorptr->total_parameters && block > SWIB){
           dorptr->end_beam = YES;
           block = END;

           if(*istat != NEWVOL){
	       if(read_a_swib == YES)
                    *istat = ENDSWEEP;
  	       else
                    *istat = ABEAM;
	   }
	}

  }/*end while*/


 /*
  *In some cases the four character RYIB is unreadable.  If this is the case we 
  *cannot get azimuth, elevation and time information for a beam.  So the bad
  *beam flag is set.  
  */
  if(dorptr->bad_beam == YES){
    ryib->status = 1;
  }

  
  return(1);
}/*assemblebeam*/

/**************************************************************************************/
#if defined (IBMRISC) || defined (HP)
void rdbeam(inunit, irewnd, istat, ivolnum, iyr, mon,iday,
             ihr,min,isec,msec,numrads,iscnmode,nflds,
             numfreq,numipp,ngates,iswpnum,julianday,irystat,
             radar_type,reqtime,radcon,scanrate,radlong,radlat,
             vnyq,rngmx,frstgat,gatspac,azim,elev,altmsl,presalt,altgnd,
             gndspdew,gndspdns,vervel,heading,roll,pitch,drift,
             rotang,tilt,uair, vair, wair,hedchgrt,pitchgrt, flddat,
             bad,fxang,radnam,fldnam,proj_name,fltnum,swapping)

#elif defined (CRAY)
void RDBEAM(inunit, irewnd, istat, ivolnum, iyr, mon,iday,
             ihr,min,isec,msec,numrads,iscnmode,nflds,
             numfreq,numipp,ngates,iswpnum,julianday,irystat,
             radar_type,reqtime,radcon,scanrate,radlong,radlat,
             vnyq,rngmx,frstgat,gatspac,azim,elev,altmsl,presalt,altgnd,
             gndspdew,gndspdns,vervel,heading,roll,pitch,drift,
             rotang,tilt,uair, vair, wair,hedchgrt,pitchgrt, flddat,
             bad,fxang,radnam,fldnam,proj_name,fltnum,swapping)

#else
void rdbeam_(inunit, irewnd, istat, ivolnum, iyr, mon,iday,
             ihr,min,isec,msec,numrads,iscnmode,nflds,
             numfreq,numipp,ngates,iswpnum,julianday,irystat,
             radar_type,reqtime,radcon,scanrate,radlong,radlat,
             vnyq,rngmx,frstgat,gatspac,azim,elev,altmsl,presalt,altgnd,
             gndspdew,gndspdns,vervel,heading,roll,pitch,drift,
             rotang,tilt,uair, vair, wair,hedchgrt,pitchgrt, flddat,
             bad,fxang,radnam,fldnam,proj_name,fltnum,swapping)
#endif

     int  *inunit,*irewnd,*istat,*ivolnum;
     int  *iyr,*mon,*iday,*ihr,*min,*isec,*msec;
     int  *numrads, *iscnmode, *nflds, *numfreq, *numipp, *ngates;
     int   *iswpnum, *julianday, *irystat,*radar_type;
     float *reqtime;
     float *radcon, *scanrate, *radlong, *radlat, *vnyq, *rngmx;
     float *frstgat, *gatspac, *azim, *elev, *altmsl, *presalt, *altgnd;
     float *gndspdew,*gndspdns,*vervel,*heading,*roll,*pitch,*drift;
     float *rotang, *tilt, *uair, *vair, *wair;
     float *hedchgrt,*pitchgrt, flddat[MXFLD][MXGAT];
     float *bad,*fxang;
     char  radnam[8],fldnam[MXFLD][8],proj_name[4],fltnum[8];
     int   *swapping;
{

  int i, j, ij;
  int change_name;
  static char cradnam[9];
  static char cproject[5];
  char cfltnum[9];
  static char cfldnam[MXFLD][8];
  void rdbeam2();


/* convert FORTRAN char. to C */

  change_name = 0;
  cradnam[9]='\0';
  for (i = 0; i < 8; i++) {
      cradnam[i] = radnam[i];
  }
  if(strncmp(cradnam,"NONE",4) == 0) change_name = 1;


  cproject[5]='\0';
  for (i = 0; i < 4; i++) {
      cproject[i] = proj_name[i];
  }

  *istat = 0;

/* go and read next beam */
  rdbeam2(cradnam, inunit, irewnd, istat, ivolnum, iyr,
             mon,iday,ihr,min,isec,msec,numrads,iscnmode,
             nflds,numfreq,numipp,ngates,iswpnum,julianday,irystat,
             radar_type,reqtime,radcon,scanrate,radlong,radlat,
             vnyq,rngmx,frstgat,gatspac,azim,elev,altmsl,presalt,altgnd,
             gndspdew,gndspdns,vervel,heading,roll,pitch,drift,
             rotang,tilt,uair,vair,wair,hedchgrt,pitchgrt,
             flddat,bad,fxang,cfltnum,cfldnam,cproject,swapping);



/* convert C chars to FORTRAN */
  /*printf(" +++ In rdbeam \n");*/

  strncpy(fltnum,"        ",8);
  strncpy(fltnum,cfltnum,8);
 

  if(change_name == 1){
      for (i = 0; i < 8; i++)  radnam[i] = cradnam[i];
  }


  strncpy(proj_name,cproject,4);
  for (i = 0; i < *nflds; i++) {
      for(j = 0; j < 8; j++){
	if(cfldnam[i][j] != ' ' && cfldnam[i][j] != '\0'){
             fldnam[i][j] = cfldnam[i][j];
	}
          else
             fldnam[i][j] = ' ';
       }
  }

  return;

}/*rdbeam*/

/************************************************************************/

/* The following C function initiates the reading of a beam from a
 * DORADE or ELDORA field format data file. 
 *
 * Input:
 * cradnam   - Radar name in C format (as opposed to FORTRAN format)
 * 
 * Output:
 * istat     - Status flag (0: normal, 1: new sweep, 2: new dorade volume, 
 *                          3: EOD, 4: other)
 *
 */

void rdbeam2(cradnam, inunit, irewnd, istat, ivolnum, iyr,
             mon,iday,ihr,min,isec,msec,numrads,iscnmode,
             nflds,numfreq,numipp,ngates,iswpnum,julianday,irystat,
             radar_type,reqtime,radcon,scanrate,radlong,radlat,
             vnyq,rngmx,frstgat,gatspac,azim,elev,altmsl,presalt,altgnd,
             gndspdew,gndspdns,vervel,heading,roll,pitch,drift,
             rotang,tilt,uair,vair,wair,hedchgrt,pitchgrt,
             flddat,bad,fxang,cfltnum,cfldnam,cproject,swapping)
     char cradnam[9];
     int  *inunit,*irewnd,*istat,*ivolnum;
     int  *iyr,*mon,*iday,*ihr,*min,*isec,*msec;
     int *numrads, *iscnmode, *nflds, *numfreq, *numipp, *ngates;
     int *iswpnum, *julianday, *irystat,*radar_type;
     float  *reqtime;
     float *radcon, *scanrate, *radlong, *radlat, *vnyq, *rngmx;
     float *frstgat, *gatspac, *azim, *elev, *altmsl, *presalt, *altgnd;
     float *gndspdew, *gndspdns, *vervel, *heading, *roll, *pitch, *drift;
     float *rotang, *tilt, *uair, *vair, *wair, *hedchgrt;
     float *pitchgrt, flddat[MXFLD][MXGAT];
     float *bad, *fxang; 
     char cfltnum[9], cfldnam[MXFLD][8],cproject[5]; 
     int *swapping;
{
  int i,j,k,i1,i2,i3,i4,ij;
  static FILE  *fpw;
  static int fd,lastfd = -1;
  int iunpck = 2;                        /* unpack the data into meteo. units */
  static int first = 0;
  int rval, rnum,ryibday,ryibmonth;
  char filename[8], radname[9];
  int is_closed,skipping;
  static struct swib_blk swib;
  struct ryib_blk ryib;
  struct asib_blk asib;
  static int lastunit = -1;
  float  rotation_angle,elevation,azimuth,the_tilt;
  static float  range_delay;
  double ftime,beam_time,seconds;
  int    funit_num;
  int    process;
  int    iprint = 0;
  int    cstatus;
  int    len;
  void   radar_angles();

  /*printf(" +++ In rdbeam2 +++\n"); */


  if(first == 0){
     printf("initializing the dorade structure\n");
     init_dorade_struct();
     first = 1;
     dorptr->current_radar[0] = '\0';
  }


  fpw = NULL;

  if(lastunit == *inunit && dorptr->file_open == 0){
     printf("THE CURRENT FORT UNIT IS THE SAME AS THE LAST: RETURNING\n");
     *istat = 3;
     return;
  }


  if(lastunit != *inunit){/* get file pointer for requested unit number */
      lastunit = *inunit;
      if (*inunit < 10 || *inunit > 999) {
         printf("\n +++ DORADE: Invalid unit number %d +++\n",*inunit);
         exit(1);
      }
    i1 = *inunit / 10 ;
    i2 = *inunit % 10 ;
    i3 = i1 / 10 ;
    i4 = i1 % 10 ;
    
    /* construct filename */
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

    fd = myopen(filename);
    if (fd < 0) {
	printf("\n+++ERROR OPENING %s FOR READING+++ \n",filename);
	exit(-1);
    }
    dorptr->file_open = 1;
    /*printf("OPENING FILE  %s\n",filename);*/
    dorptr->fd = fd;
  }
  
  /*-----------------------------READ THE DATA FILE---------------------*/  

  if(dorptr->end_time_reached == YES){
     *istat = 3;
     return;
  }

  process = YES;
  ftime   = (double)*reqtime;
  dorptr->bytesbeam = 0;
  cstatus = 0;
  assemble_beam(fd, fpw, &swib, &ryib, &asib,
                iprint, flddat, &cstatus, cfldnam,
                process, ftime, cradnam);

  *istat = ABEAM;


  if(cstatus == NEWVOL){ 
     dorptr->last_radar[0] = '\0';
     *istat = NEWVOL;
  } 


  if(cstatus == READERROR){
     printf("ERROR READING THE DATA FILE\n");
     *istat = READERROR;
     close(fd);
     dorptr->file_open = 0;
     return;
  }
 
  
  if(dorptr->correct_rad == NO){
     *irystat = WRONGRADAR;
  }


  if(cstatus == ENDSWEEP  && dorptr->sweep_file == 0){
     len = strlen(dorptr->last_radar);
     if(strncmp(dorptr->last_radar,cradnam,len) == 0){
        printf("END OF CURRENT SWEEP\n");
        *istat = ENDSWEEP;
        return;
     }
     else{
        *istat = ABEAM;
        return;
     }
  }

  
  if(cstatus == ENDSWEEP && dorptr->sweep_file == 1){
     *istat = ENDSWEEPFILE;
     close(fd);
     /*     printf("closing file %d\n",lastunit); */
     dorptr->file_open = 0;
     return;
  }


  /*-----------------------------we have a beam from the correct radar---------------------*/
 /* 
  *Information that was read from the Volume header.
  */
  *iyr         = dorptr->year;
  *numrads     = dorptr->radd_cnt;
  *radcon      = dorptr->radar_constant[0];
  *vnyq        = dorptr->nyquist_velocity[0];
  *radar_type  = dorptr->radar_type;
  *nflds       = dorptr->total_parameters;
  *numfreq     = dorptr->num_frequencies;
  *numipp      = dorptr->num_ipulse;
  *frstgat     = dorptr->range_first_gate + dorptr->range_delay;
  *ngates      = dorptr->ngates;
  *gatspac     = dorptr->gate_spacing;
  *iscnmode    = dorptr->scan_mode;
  *bad         = dorptr->bad;



  for (i = 0; i < *nflds ; i++) { /* transfer field names */
	strcpy(cfldnam[i], dorptr->field_name[i]);
  }


  /*
   *Information from the RYIB to be returned.
   */
  *iswpnum = ryib.swpnum;
  /*
   *Ryib.day contains the number of days since Jan. 1st
   *of a given year.  The ryibmonth and ryibday are the
   *month and the day of the ray information block(ryib)
   *for the beam just read.
   */
  *julianday  = ryib.day;
   julday(ryib.day,dorptr->year,&ryibmonth,&ryibday);
  *mon         = ryibmonth;
  *iday        = ryibday;
  *ihr     = ryib.hour;
  *min     = ryib.min;
  *isec    = ryib.sec;
  *msec    = ryib.msec;
  *azim    = ryib.az;
  *elev    = ryib.elev;
  *irystat = ryib.status;
  *fxang   = swib.fix_ang;
  

  if(dorptr->asib == 1 && dorptr->radar_type != 0){ 
    radar_angles(asib,&rotation_angle,&azimuth,&elevation,&the_tilt);
    *radlat   = asib.lat;
    *radlong  = asib.lon;
    *presalt  = asib.pres_alt;
    *altgnd   = asib.alt_gnd;
    *gndspdew = asib.gndspd_ew;
    *gndspdns = asib.gndspd_ns;
    *vervel   = asib.ver_vel;
    *heading  = asib.heading;
    *roll     = asib.roll;
    *pitch    = asib.pitch;
    *drift    = asib.drift;
    *rotang   = rotation_angle;
    *tilt     = the_tilt + .3;
    *uair     = asib.wndspd_ew;
    *vair     = asib.wndspd_ns;
    *wair     = asib.verspd;
    *hedchgrt = asib.head_rate;
    *pitchgrt = asib.ptch_rate;
    *azim     = azimuth;
    *elev     = elevation;
  }
  else{
    *radlat   = -999.0;
    *radlong  = -999.0;
    *presalt  = -999.0;
    *altgnd   = -999.0;
    *gndspdew = -999.0;
    *gndspdns = -999.0;
    *vervel   = -999.0;
    *heading  = -999.0;
    *roll     = -999.0;
    *pitch    = -999.0;
    *drift    = -999.0;
    *rotang   = -999.0;
    *tilt     = -999.0;
    *uair     = -999.0;
    *vair     = -999.0;
    *wair     = -999.0;
    *hedchgrt = -999.0;
    *pitchgrt = -999.0;
  }      

}/*rdbeam2*/

/************************************************************************
                       DORADE UTILITY ROUTINES              
************************************************************************/
int get_descriptor_length(char array[8])
{
  int  len,rval;
  extern struct dorade_basic *dorptr;

 /*
  *If the value of dorptr->swap is -1 then it has not been set yet.
  *This is the first read.
  */

  if(dorptr->swap == -1){
     len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
     if(len < 0 || len > 10000){
        len = four_bytes_2_int(array[7],array[6],array[5],array[4]); 
        if(len < 0 || len > 10000){
           printf("AN INVALID DESCRIPTOR LENGTH HAS BEEN READ %d\n",len);
           return(-1);
        }
        else{
           dorptr->swap = YES;
	}
     }
     else{
        dorptr->swap = NO;
     }
  }

  else if(dorptr->swap == NO){
     len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
  }    
 
  else if(dorptr->swap == YES){
     len = four_bytes_2_int(array[7],array[6],array[5],array[4]);
  }
  
  else{
     printf("Goofy swap flag in get_descriptor_length\n");
  }


  return(len);
}/*get_descriptor_length*/
/************************************************************************/

/* the following function takes 4 bytes as arguments and constructs
 * a 4 byte integer or float out of them. 
 */
int four_bytes_2_int(c1,c2,c3,c4) 
     char c1,c2,c3,c4;
{
  int it1,it2,it3,it4;
  unsigned int ival;

  it1 = c1;
  if (it1 < 0) it1 = it1 + 256;
  it1 = it1 << 24;

  it2 = c2;
  if (it2 < 0) it2 = it2 + 256;
  it2 = it2 << 16;

  it3 = c3;
  if (it3 < 0) it3 = it3 + 256;
  it3 = it3 <<  8;

  it4 = c4;
  if (it4 < 0) it4 = it4 + 256;

  ival = it1 | it2 | it3 | it4;

/* make negative, if necessary, for machines with word sizes bigger than 32 */
/* the first big number is 2**31 and the second is 2**32 */
  if (ival >= 2147483648) ival = ival - 4294967296;

  return ival;
}
/************************************************************************/
/* the following function takes two bytes (chars) as arguments and returns
 * an integer value by combining them.
 */
int two_bytes_2_int(c1,c2)
     char c1,c2;
{
  int it1,it2,ival;

  it1 = c1;
  if (it1 < 0) it1 = it1 + 256;
  it1 = it1 << 8;

  it2 = c2;
  if (it2 < 0) it2 = it2 + 256;

  ival = it1 | it2;

/* make negative, if necessary, for machines with word sizes > 16 bits */
  if (ival >= 32768) ival = ival - 65536; 

  return ival;
}
/************************************************************************/
/* The following function takes four bytes that are parts of a
 * 32-bit IEEE floating point word and builds a floating point
 * word on the current machine. Note, so far, only the Cray requires
 * special action because it's the only one that doesn't use IEEE.
 */
float four_bytes_2_flt(c1,c2,c3,c4)
     char c1,c2,c3,c4;

{
  int it1,it2,it3,it4,ival;
  int type, num, bitoff, stride, ierr;
  float cfval;
  union fltint
    {
      float fval;
      int   ival;
    } mixed;
  

    it1 = c1;
    if (it1 < 0) it1 = it1 + 256;
    it1 = it1 << 24;

    it2 = c2;
    if (it2 < 0) it2 = it2 + 256;
    it2 = it2 << 16;

    it3 = c3;
    if (it3 < 0) it3 = it3 + 256;
    it3 = it3 << 8;

    it4 = c4;
    if (it4 < 0) it4 = it4 + 256;

    mixed.ival = it1 | it2 | it3 | it4;

  if (IEEEFLOAT) {
    return mixed.fval;
  }

  else {
    mixed.ival = mixed.ival << 32;
    type = 2;
    num  = 1;
    bitoff = 0;
    stride = 0;
    ierr = 0;

    /* convert IEEE 32-bit float to Cray 64-bit float */
/*    ierr = IEG2CRAY(&type, &num, &(mixed.ival), &bitoff, &cfval, &stride); */
    return cfval;
  }

}

/************************************************************************/    

#if defined (IBMRISC) || defined (HP)
  void get_mode(int themode[100],int *modevalue)
#elif defined (CRAY)
  void GET_MODE(int themode[100],int *modevalue)
#elif defined (linux)
  void get_mode__(int themode[100],int *modevalue)
#else
  void get_mode_(int themode[100],int *modevalue)
#endif
{

   int i,count;
   int temp;

   count = 0;
   temp = themode[0];

   for(i = 1; i < 1000; i++){
     if(themode[i] > temp){
        temp = themode[i];
        *modevalue = i;
     }
   }
}

/************************************************************************/


/*********************************************************************************
                   ROUTINES FOR UNCOMPRESSING THE DATA
*********************************************************************************/
int uncompress( ss, dd, flag, empty_run, wmax )
  short *ss, *dd;
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

    while(*ss != 1) {		/* 1 is an end of compression flag */
	n = *ss & 0x7fff;	/* nab the 16-bit word count */
	if(wcount+n > wmax) {
	  /*printf("Uncompress failure %d %d %d\n"
	    , wcount, n, wmax);*/
	    mark = 0;
            return(-1);
	}
	else {
	    wcount += n;		/* keep a running tally */
	}
	if( *ss & 0x8000 ) {	/* high order bit set implies data! */
	    *empty_run = 0;
	    ss++;
	    for(; n--;) {
		*dd++ = *ss++;
	    }
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
	 printf("Uncompress data failure %d %d %d\n"
	   , wcount, n, wmax);
	  mark = 0;
          return(-1);
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

/********************************************************************************/
void set_radar(int rightradar)
{

  if(rightradar == YES) dorptr->correct_rad = YES;
  else dorptr->correct_rad = NO;
}/*set_radar*/
   

/********************************************************************************/
void set_process()
{
     dorptr->process = YES;
}/*set_process*/
/********************************************************************************/
void check_radar_name(char reqrad[9],int len)
{

 int index,count;

 extern struct dorade_basic *dorptr;


 count = dorptr->radd_cnt + 1;
 if(strncmp(reqrad,dorptr->radar_name[0],len) == 0) return;
 if(count  == 2) if(strncmp(reqrad,dorptr->radar_name[1],len) == 0) return;
 if(dorptr->sweep_file == 1){
     strncpy(reqrad,dorptr->radar_name[0],8);
     for(index = 0; index < 8; index++){
         if(reqrad[index] == ' ') {
            reqrad[0] = '\0';
            break;
	 }
     }
     return;
 }
 printf("Requested name %8s was not found\n",reqrad);
 printf("Radar(s) present are  ");
 for(index = 0; index < len; index++) printf("%c",dorptr->radar_name[0][index]);
 if(count == 2){
    printf("\n                      ");
    for(index = 0; index < len; index++) printf("%c",dorptr->radar_name[1][index]);
 }
 printf("\n");
 exit(0);
}/*check_radar_name*/
        
/************************************************************************/
/*Dick Oye's routine for calculating the track relative rotation_angle,
 *az,elevation and tilt angles from the values in the ASIB. 
 */
# define TWOPI 6.283185307
# define RADIANS(x)  ((x)*0.017453292) 
# define DEGREES(x)  ((x)*57.29577951)
# define dd_isnanf(x)   (((*(long *)&(x) & 0x7f800000L) == 0x7f800000L) && \
                         ((*(long *)&(x) & 0x007fffffL) != 0x00000000L))
# define SIN(x) (sin((double)(x)))
# define COS(x) (cos((double)(x)))


void radar_angles( asib,rotation_angle,azimuth,elevation,the_tilt)
  struct asib_blk *asib;
  float *rotation_angle;
  float *azimuth;
  float *elevation;
  float *the_tilt;
{
    double R, H, P, D, T, theta_a, tau_a, theta_t, tau_t, lambda_t, phi_t;
    double sinP, cosP, sinT, cosT, sinD, cosD, sinR, cosR;
    double sin_theta_rc, cos_theta_rc, sin_tau_a, cos_tau_a;
    double xsubt, ysubt, zsubt, lambda, Phi;
    double x,y,z;
    double d, bigR, az;
    double sin_el, cos_az, cos_el, cos_psi, cos_phi;
    double sin_pitch, cos_pitch, cos_drift, sin_tilt, cos_tilt;
    double rtilt, rtrack, rphi;
    double azr, azd;
    double lambda_a, phi_a;
    double sin_lambda_a, cos_lambda_a, sin_phi_a, cos_phi_a;


    d = asib->roll;
    R = dd_isnanf(d) ? 0 : RADIANS(d);

    d = asib->pitch;
    P = dd_isnanf(d) ? 0 : RADIANS(d);

    d = asib->heading;
    H = dd_isnanf(d) ? 0 : RADIANS(d);

    d = asib->drift;
    D = dd_isnanf(d) ? 0 : RADIANS(d);

    sinP = sin(P);
    cosP = cos(P);
    sinD = sin(D);
    cosD = cos(D);

    T = H + D;

    sinT = sin(T);
    cosT = cos(T);


    d = asib->rot_ang;
    theta_a = dd_isnanf(d) ? 0 : RADIANS(d);
    d = asib->tilt_ang;
    tau_a = dd_isnanf(d) ? 0 : RADIANS(d);
    sin_tau_a = sin(tau_a);
    cos_tau_a = cos(tau_a);
    sin_theta_rc = sin(theta_a + R); /* roll corrected rotation angle */
    cos_theta_rc = cos(theta_a + R); /* roll corrected rotation angle */

    x = xsubt = (cos_theta_rc * sinD * cos_tau_a * sinP
	     + cosD * sin_theta_rc * cos_tau_a
	     -sinD * cosP * sin_tau_a);
    y = ysubt = (-cos_theta_rc * cosD * cos_tau_a * sinP
	     + sinD * sin_theta_rc * cos_tau_a
	     + cosP * cosD * sin_tau_a);
    z = zsubt = (cosP * cos_tau_a * cos_theta_rc
	     + sinP * sin_tau_a);

    theta_t = atan2(xsubt, zsubt);
    *rotation_angle = DEGREES(theta_t);
    if(*rotation_angle < 0) *rotation_angle += 360.;
    tau_t = asin(ysubt);
    *the_tilt = DEGREES(tau_t);
    lambda_t = atan2(xsubt, ysubt);
    az = fmod(lambda_t + T, TWOPI);
    *azimuth = DEGREES(az);
    phi_t = asin(zsubt);
    *elevation = DEGREES(phi_t);
    return;
}/*radar_angles*/

/************************************************************************************************/
void rewnddor_()
{
 int bytes_beam;
 extern struct dorade_basic *dorptr;

/* 
 *This routine backs up by one beam in the dorade data file.  It is called from
 *DORVOL.f .  
 */

  if(dorptr->sweep_file == 0) return;
  bytes_beam = -dorptr->bytesbeam;
  if(lseek(dorptr->fd,bytes_beam,SEEK_CUR) < 0){
     printf("unable to seek on disk file\n");
     exit(0);
  }
  
 /*
  *Reset the ryib counter and block counters since we are backing up.
  */
  dorptr->ryib_cnt = 0;
  dorptr->blknum   = 0;
}/*rewnddor*/
/************************************************************************************************/
void usrflds_(char name[8],int *j) 
{
 int index;
 extern struct dorade_basic *dorptr;

 dorptr->num_req_flds = *j - 1;
 
 index = 0;
 while(name[index] != ' ' && name[index] != '\0'){
     dorptr->req_fld[dorptr->num_req_flds][index] = name[index];
     index++;
 }
   
}/*usrflds*/
/************************************************************************************************/
void endtime_()
{
 extern struct dorade_basic *dorptr;

 dorptr->end_time_reached = YES;
}/*endtime*/
     
