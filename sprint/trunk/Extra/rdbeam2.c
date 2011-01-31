# include <stdio.h>
# include "dorade.h"
# include <math.h>

struct radd_blk *radds[MXRAD];  /* one for each radar */
struct cspd_blk *cspds[MXRAD];
struct metadata *data_info[MXRAD];
char arr_ryib[MXBYTES];
char arr_asib[MXBYTES];
char arr_frad[MXBYTES];
char arr_rdat[MXBYTES];
char arr_volhead[MXBYTES];
int  len_ryib;
int  len_asib;
int  len_frad;
int  len_rdat;
int  len_volhead;
int skipping = 0;       /* 1 -> in process of skipping; used when writing
			 * out new volumes.
			 */

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
             nflds,numfreq,numipp,ngates,iswpnum,julday,irystat,
             radar_type,reqtime,radcon,scanrate,radlong,radlat,
             vnyq,rngmx,frstgat,gatspac,azim,elev,altmsl,presalt,altgnd,
             gndspdew,gndspdns,vervel,heading,roll,pitch,drift,
             rotang,tilt,uair,vair,wair,hedchgrt,pitchgrt,
             flddat,scale,offset,bad,fxang,cfltnum,cfldnam,
             cproject,swapping)
     char cradnam[9];
     int  *inunit,*irewnd,*istat,*ivolnum;
     int  *iyr,*mon,*iday,*ihr,*min,*isec,*msec;
     int *numrads, *iscnmode, *nflds, *numfreq, *numipp, *ngates;
     int *iswpnum, *julday, *irystat,*radar_type;
     float  *reqtime;
     float *radcon, *scanrate, *radlong, *radlat, *vnyq, *rngmx;
     float *frstgat, *gatspac, *azim, *elev, *altmsl, *presalt, *altgnd;
     float *gndspdew, *gndspdns, *vervel, *heading, *roll, *pitch, *drift;
     float *rotang, *tilt, *uair, *vair, *wair, *hedchgrt;
     float *pitchgrt, flddat[MXFLD][MXGAT], scale[MXFLD], offset[MXFLD];
     float *bad, *fxang; 
     char cfltnum[9], cfldnam[MXFLD][9],cproject[5]; 
     int *swapping;
{
  int i,j,k,i1,i2,i3,i4,ij;
  static FILE  *fpw;
  static int fd,lastfd = -1;
  int iunpck = 2;                        /* unpack the data into meteo. units */
  int rval, rnum;
  int iprinth = 0, iprintr = 0;
  int platformb_found = 0;
  int bytes_beam;
  char lastrad[9], filename[9], radname[9];
  static struct files *open_files = NULL;    /* linked list of open file info */
  static struct files *head = NULL;          /* head of linked list */
  static int lastunit = -1;
  static float  time = 0.0;
  static int the_radar_type = 0;
  static int year, month, day;

  int is_closed;
  struct vold_blk vold;
  struct radd_blk radd;
  struct cfac_blk cfac;
  struct parm_blk parm;
  struct cspd_blk cspd;
  struct celv_blk celv;
  static struct swib_blk swib;
  struct ryib_blk ryib;
  struct asib_blk asib;
  struct frad_blk frad;
  struct rdat_blk rdat;
  static int end_sweep_file = 0;
  static int write_to_file = 0;
  static int sweep_files = 0;
  float  rotation_angle,elevation,azimuth,the_tilt;
  static float  range_delay;
  void   radar_angles();
  
  printf(" +++ In rdbeam2 \n");
  printf(" +++ In rdbeam2 -  inunit  = %d\n",*inunit);
  printf(" +++ In rdbeam2 -  irystat = %d\n",*irystat);
  printf(" +++ In rdbeam2 - swapping = %d\n",*swapping);

  fpw = NULL;
  if (lastunit != *inunit) { /* get file pointer for requested unit number */
    lastrad[0] = '\0';
    lastunit = *inunit;
    time = 0.0;
    if(end_sweep_file == 1){
          end_sweep_file = 0;
          is_closed = close(lastfd);
          if(is_closed == -1){
             printf(" +++ In rdbeam2 - unable to close lastfd= %d\n",lastfd);
          }
    }
    
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
    /*printf("inunit=%d  i1=%d  i2=%d  i3=%d  i4=%d  filename=%s \n",*inunit,i1,i2,i3,i4,filename);*/

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
    if (open_files->unit != *inunit) { /* file is not already in list;open it*/
      fd = myopen(filename);
      /* printf(" +++ In rdbeam2 opening filename,fd = %s %d \n",filename,fd);*/
      if (fd < 0) {
	printf("\n+++ERROR OPENING %s FOR READING+++ \n",filename);
	exit(-1);
      }

      open_files->unit = *inunit;
      open_files->fds = fd;
      open_files->next = NULL;
      lastfd = fd;
    }
    else {
      fd = open_files->fds;
    }
    /* printf("unit number %d\n",open_files->fds); */
    /* transfer volume and radar info */
    for (i = 0; i < MXRAD; i++) {
      radds[i] = &(open_files->radds[i]);
      cspds[i] = &(open_files->cspds[i]);
      data_info[i] = &(open_files->data_info[i]);
    }
    *ivolnum = open_files->volnum;
    *iyr     = open_files->year;
    year     = *iyr;
    *mon     = open_files->month;
    month    = *mon;
    *iday    = open_files->day;
    day      = *iday;
    *numrads = open_files->numrads;
    strcpy(cfltnum, vold.flt_number);
    the_radar_type = radd.rad_type;
  }
  
  if (*irewnd) { /* reposition stream to beginning */
    rval = lseek(fd, 0, 0);
    time = 0.0;
    if (rval != 0) {
      *istat = 4;
      return;
    }
  }
  
  /* if a beam from a different radar is being sought, reset time so that
   * time from last radar doesn't screw up search for time for this radar
   */
  if (strcmp(cradnam, lastrad) != 0) time = 0.0;
   

  /* 
   *Read the first beam so we get a beam time.
   */
  assemble_beam(fd, fpw, &vold, &radd, &cfac, &parm, &cspd, &celv, &swib, 
		&ryib, &asib, &frad, &rdat, iprinth, iprintr, &time, 
		reqtime, flddat, istat, iunpck, cfldnam, &write_to_file,
                swapping,&platformb_found,&sweep_files,&bytes_beam);

  if(*istat == 2 && strncmp(cradnam,"NONE",4) == 0){
      for(i = 0; i< 8; i++){
          if(swib.radname[i] != ' ' && swib.radname[i] != '\0'){
             cradnam[i] = swib.radname[i];
	   }
          else
             cradnam[i] = ' ';
	}
    }


  if (*istat > 2) return;
  if(*istat == -3) end_sweep_file = 1;
  if (*istat < 0) return;

  if (time < *reqtime) {
    while (time < *reqtime) { /* skip past beams until we find the time we want */
      assemble_beam(fd, fpw, &vold, &radd, &cfac, &parm, &cspd, &celv,&swib,
		    &ryib, &asib, &frad, &rdat, iprinth, iprintr, &time, 
		    reqtime, flddat, istat, iunpck, cfldnam, &write_to_file,
                    swapping,&platformb_found,&sweep_files,&bytes_beam);
      if (*istat > 2) return;
      if (*istat < 0) return;
      
      if (strncmp(swib.radname, cradnam, 4) == 0 || strcmp(cradnam, "") == 0) {
	/* if right radar */
	time = ryib.hour*10000. + ryib.min*100 + ryib.sec + ryib.msec/1000. ;
      }
    }
  }
  else{
      if(*istat == 2 && sweep_files == 1){
          bytes_beam = -bytes_beam;
          if(lseek(fd,bytes_beam,SEEK_CUR) < 0){
             printf("unable to seek on disk file\n");
             exit(0);
	  }
      }         
  }

  /* At this point, we presumably have our beam. If a new volume header was
   * read, update all the volume info. 
   */
  if (*istat == 2) {/* update volume info since a new volume header was found */
     lastrad[0] = '\0';
     open_files->volnum  = vold.volnum;
     open_files->year    = vold.year;
     open_files->month   = vold.month;
     open_files->day     = vold.day;
     open_files->numrads = vold.num_sens;
     cproject[0]         = vold.proj_name[0];
     cproject[1]         = vold.proj_name[1]; 
     cproject[2]         = vold.proj_name[2];
     cproject[3]         = vold.proj_name[3]; 
     strcpy(open_files->cfltnum, vold.flt_number);
     *ivolnum  = vold.volnum;
     *iyr      = vold.year;
     year      = *iyr;
     *mon      = vold.month;
     month     = *mon;
     *iday     = vold.day;
     day       = *iday;
     *numrads  = vold.num_sens;
     strcpy(cfltnum, vold.flt_number);
     range_delay = cfac.rng_del;
  }
  
  /* see if the radar info we're returning needs to be updated (in case
   * the current beam is from a different radar than the previous one).
   */
    rnum = -1;
    for (i = 0; i < MXRAD; i++) {
      if (strncmp(swib.radname, radds[i]->radnam,4) == 0) rnum = i;
    }
    if (rnum == -1) {
      printf("\n   +++Error locating radar %s in data set.+++\n",cradnam);
      printf("      Radars in dataset:\n");
      for (i = 0; i < *numrads; i++) {
	printf("      '%s'\n", radds[i]->radnam);
      }
      *istat = 4;
      return;
    }
    else { /* found radar, transfer data */
       strcpy(lastrad, swib.radname);
      *radcon   = radds[rnum]->radcon;
      *iscnmode = radds[rnum]->scan_mode;
      *nflds    = radds[rnum]->tot_par;
      *radlong  = radds[rnum]->rad_long;
      *radlat   = radds[rnum]->rad_lat;
      *vnyq     = radds[rnum]->nyq;
      *rngmx    = radds[rnum]->max_range;
      *numfreq  = radds[rnum]->num_freq;
      *numipp   = radds[rnum]->num_ip;
      the_radar_type = radds[rnum]->rad_type;
  
      
      /* calc. range gate info */
      *ngates  = data_info[rnum]->ngates;
      *frstgat = data_info[rnum]->frstgat + range_delay;
      *gatspac = data_info[rnum]->gatspac;
      *bad     = data_info[rnum]->bad[0];
      
      for (i = 0; i < *nflds ; i++) { /* transfer field names */
	strcpy(cfldnam[i], data_info[rnum]->fields[i]);
      }
      
      *iyr = year;
      *mon = month;
      *iday= day;

    }

  /* transfer beam specific information to variables to be returned */
  *iswpnum = ryib.swpnum;
  *julday  = ryib.day;
  *ihr     = ryib.hour;
  *min     = ryib.min;
  *isec    = ryib.sec;
  *msec    = ryib.msec;
  *azim    = ryib.az;
  *elev    = ryib.elev;
  *irystat = ryib.status;
  *fxang   = swib.fix_ang;

  if(platformb_found == 1 && *radar_type != 0){ 
    radar_angles(asib,cfac,&rotation_angle,&azimuth,&elevation,&the_tilt);
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
  *radar_type = the_radar_type;
  return;
  
}


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


void radar_angles( asib, cfac,rotation_angle,azimuth,elevation,the_tilt)
  struct asib_blk *asib;
  struct cfac_blk *cfac;
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
