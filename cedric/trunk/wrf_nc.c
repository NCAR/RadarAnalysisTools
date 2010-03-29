#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include "/opt/local/netcdf-3/include/netcdf.h"
#include "ced_cdf.h"
#include "cedric.h"

#define TYPE_FLOAT 1
#define TYPE_INT   2
#define TYPE_CHAR  3

#define UWIND 1
#define VWIND 2
#define VERTWIND 3
#define THETA 4
#define RTB 5
#define RTP 6
#define RR  7
#define QVAPOR 8
#define MAPFAC 9
#define DZETADZ 10
#define ZHGT 11
#define HGT 12
#define TC  13
#define PRESSURE  14
#define TD 15
#define RH 16
#define ZTOTAL 17
#define QRAIN 18
#define QSNOW 19
#define QGRAUP 20
#define ZRAIN 21

struct wrf_cdf{
  int dimsizes[NC_MAX_DIMS];
  int varids[NC_MAX_VARS];
  int vardims[NC_MAX_VARS];
  int varlevels[NFMAX];
  int rtp;
  int rr;
  int rtb;
  int qvapor;
  int qrain;
  int qsnow;
  int qgraup;
  int map;
  int dzetadz;
  int rho_u;
  int rho_v;
  int rw;
  int zhgt;
  int hgt;
  int thermox;
  int thermoy;
  int thermoz;
  int gpoint_hgt;
  int ntime;
  int ncid;
  int editncid;
  int intrpncid;
  int nvars;
  int model_version;
};
int reqdate;
static struct wrf_cdf *wrfptr;

/****************************************************************************/
void copenwrf_(int *cunit,int *inunit,int *dimcnt)
{
   int nc_status,id;
   int i1, i2, i3,temp;
   char fname[9];
   int ndims,nvars;
   int ngatts;			/* number of global attributes */
   int xdimid;			/* id of unlimited dimension */
   extern struct wrf_cdf *wrfptr;
   void special_var_ids();

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


   nc_status = nc_open(fname, NC_NOWRITE, &id);
   if (id <= 0){
        printf("UNABLE TO OPEN NETCDF DATA FILE\n");
        exit(0);
   }



    /*
     *Get the number of dimensions and variables in the wrf netcdf file.
     */
    if ( nc_inq(id, &ndims, &nvars, &ngatts, &xdimid) != NC_NOERR){
        printf("unable to get netcdf dimensions\n");
        exit(0);
    }

    *dimcnt  = ndims;

   /*
    *Initialize the wrf structure.
    */
    wrfptr = (struct wrf_cdf *)malloc(sizeof(struct  wrf_cdf));
    if(!wrfptr){
       printf("UNABLE TO MALLOC SPACE FOR WRF STRUCTURE\n");
       exit(-1);
    }
    wrfptr->ncid = id;
    *cunit = id;
    wrfptr->nvars = nvars;

    special_var_ids();

    wrfptr->editncid = -1;
    wrfptr->intrpncid = -1;
    wrfptr->ntime = 0;
}/*copenwrf*/

/*************************************************************************/
void rwrfdims_(int *dimcnt, int *nx, int *ny, int *nz, 
               int *ntimes,int *timestr)
{
 struct ncdim {                        /* dimension */
    char name[NC_MAX_NAME];
    size_t size;
 };

struct ncdim dims[NC_MAX_DIMS]; /* dimensions */
int ndims,dimid;
extern struct wrf_cdf *wrfptr;

/*
 *Get netcdf dimension information.
 */
 ndims = *dimcnt;
 for(dimid = 0; dimid < NC_MAX_DIMS; dimid++) wrfptr->dimsizes[dimid] = -1;
 for(dimid = 0; dimid < ndims; dimid++){
     nc_inq_dim(wrfptr->ncid, dimid, dims[dimid].name, &dims[dimid].size);
     if(strncmp(dims[dimid].name,"south_north",11) == 0){
       if(strncmp(dims[dimid].name,"south_north_stag",16) != 0){
              *ny = dims[dimid].size;
              wrfptr->thermoy = dims[dimid].size;
       }
     }
     else if(strncmp(dims[dimid].name,"west_east",10) == 0){
       if(strncmp(dims[dimid].name,"west_east_stag",15) != 0){
             *nx = dims[dimid].size;
             wrfptr->thermox = dims[dimid].size;
       }
     }
     else if(strncmp(dims[dimid].name,"bot_top",7) == 0){
       if(strncmp(dims[dimid].name,"bot_top_stag",12) != 0){
            *nz = dims[dimid].size;
            wrfptr->thermoz = dims[dimid].size;
       }
     }

     else if(strncmp(dims[dimid].name,"bottom_top",10) == 0){
       if(strncmp(dims[dimid].name,"bottom_top_stag",15) != 0){
            *nz = dims[dimid].size;
            wrfptr->thermoz = dims[dimid].size;
       }
     }

     else if(strncmp(dims[dimid].name,"Time",4) == 0)
       *ntimes = dims[dimid].size;
     else if(strncmp(dims[dimid].name,"DateStrLen",10) == 0)
       *timestr = dims[dimid].size;
     wrfptr->dimsizes[dimid] = dims[dimid].size;
 }

}/*rwrfdims*/
/*************************************************************************/
void rwrfvars_(int *vdims,int *fldnum,float *rbuf2, int *nlev,char name[8])
              
{
  struct ncvar {			/* variable */
    char name[NC_MAX_NAME];
    nc_type type;
    int ndims;
    int dims[MAX_VAR_DIMS];
    int natts;
    boolean has_fillval;
    double fillval;
  };

struct ncatt {			/* attribute */
    int var;
    char name[NC_MAX_NAME];
    nc_type type;
    size_t len;
    char *string;		/* for text attributes (type = NC_CHAR) */
    double *vals;		/* for numeric attributes of all types */
};
  /*-------------------------------------------------------------------*/
 /*
  *The following union and unsigned longs are for traping
  *very small numbers on the Dec assuming a 32 bit machine.
  *that is a sign bit, and 8-bit exponent and a 23 bit mantissa.
  */
  union {float x; unsigned long ix;} u;
  unsigned long signmask = 0x80000000;
  unsigned long expmask = 0x7f800000;
  unsigned long restmask = 0x007fffff;
  unsigned long totalmask = 0xffffffff;
  /*-------------------------------------------------------------------*/
  struct ncvar var;
  struct ncatt att;		/* attribute */
  int    index,nplane,i,j;
  int    rcode,xdim,ydim,level,num;
  int    varlevels,xlimit,ylimit; 
  float  min,max,qvar;
  char   units[10],lname[50],temp[2];
  long   start[5],count[5],icount;
  extern struct wrf_cdf *wrfptr;
  void calc_winds();


  num = *fldnum - 1; 
  for (index = 0; index  < wrfptr->nvars; index++){
      if(index == wrfptr->varids[num]){
         nc_inq_var(wrfptr->ncid, index, var.name, &var.type, &var.ndims,
		    var.dims, &var.natts);
         *vdims = var.ndims;
         if(var.ndims == 3){
            start[0] = wrfptr->ntime;
            start[1] = 0;
            start[2] = 0;
            count[0] = 1;
            ydim = var.dims[1];
            xdim = var.dims[2];
            count[1] = wrfptr->dimsizes[ydim];
            count[2] = wrfptr->dimsizes[xdim];
            xlimit = count[2];
            ylimit = count[1];
	 }
         else{
            ydim = var.dims[2];
            xdim = var.dims[3];
            start[0] = (long)wrfptr->ntime;
            start[1] = (long)*nlev-1;
            start[2] = (long)0;
            start[3] = (long)0;
            count[0] = 1;  
            count[1] = (long)1;  
            count[2] = (long)wrfptr->dimsizes[ydim];
            count[3] = (long)wrfptr->dimsizes[xdim]; 
            xlimit = count[3];
            ylimit = count[2];
	 }                 
         if(var.ndims == 4){
            varlevels = var.dims[1];
            wrfptr->varlevels[num] = wrfptr->dimsizes[varlevels];
            if(wrfptr->dimsizes[varlevels] == *nlev-1) return;
	 }

         nplane = count[1] * count[2];
         rcode = ncvarget(wrfptr->ncid,index,start,count,rbuf2); 
         if(rcode != NC_NOERR){
             printf("unable to get data array\n");
             nc_close(wrfptr->ncid);
             exit(-1);
	 }

         /*Code for traping small numbers.
          *Here we do a bit wise and of the buffer value with the 
          *exponent mask.  Then we shift right by 23.  
          */
         if(name[0] == 'Q'){
            for(nplane = 0; nplane < count[2]*count[3]; nplane++){
                u.x = rbuf2[nplane];
                qvar = (u.ix & expmask) >> 23;
                /*
                 *A value of 5 corresponds to 1e-37.
                 */
                if( qvar < 6) rbuf2[nplane] = 0.0;
	    }
         }/*if we have a Q*/

      }/*if index == wrfptr*/
  }/*end of for loop*/


}/*rwrfvars*/
/*************************************************************************/
void rundate_(char *krd)
{
  char creqdate[9];
  int stuff;

  creqdate[0] = krd[32];
  creqdate[1] = krd[33];
  creqdate[2] = krd[34];
  creqdate[3] = krd[35];
  creqdate[4] = krd[36];
  creqdate[5] = krd[37];
  creqdate[6] = krd[38];
  creqdate[7] = krd[39];
  creqdate[8] = '\0';
  reqdate = atoi(creqdate);
}/*rundate*/
/*************************************************************************/
void wrfdate_(int dttm[6],int *ntimes,int *timestr,int *ibtim,char volname[8])
{

  struct ncvar {                        /* variable */
    char name[NC_MAX_NAME];
    nc_type type;
    int ndims;
    int dims[MAX_VAR_DIMS];
    int natts;
    boolean has_fillval;
    double fillval;
  };

  struct ncvar var;
  int index,rcode,tvar,req_time;
  int tindex,dindex,itime,idate;
  char temp[10],temptwo[5]; 
  char ctemp[100*20],dtime[7];
  size_t start[2],count[2];
  extern struct wrf_cdf *wrfptr;
  extern int reqdate;


  memset(ctemp,' ',100*20);
  req_time = *ibtim;
  tvar = -1;
  for (index = 0; index  < wrfptr->nvars; index++){
       nc_inq_var(wrfptr->ncid, index, var.name, &var.type, &var.ndims,
                    var.dims, &var.natts);
       if(strcmp(var.name,"Times") == 0) tvar = index;
  }
  
  start[0] = 0;
  count[0] = *ntimes;
  rcode = nc_get_var_text(wrfptr->ncid,tvar,ctemp);         
  if(rcode != NC_NOERR){
        printf("UNABLE TO GET DATA TIMES\n");
        exit(0);
  }

  wrfptr->ntime = -1;
  tindex = 11;
  dindex = 0;
  for(index = 0; index < *ntimes; index++){
    dtime[0] = ctemp[tindex];
    dtime[1] = ctemp[tindex+1];
    dtime[2] = ctemp[tindex+3];
    dtime[3] = ctemp[tindex+4];
    dtime[4] = ctemp[tindex+6];
    dtime[5] = ctemp[tindex+7];
    dtime[6] = '\0';
    itime = atoi(dtime);
  
    if(ctemp[dindex] == ' ') temp[0] = '0';
    else temp[0] = ctemp[dindex];

    if(ctemp[dindex+1] == ' ') temp[1] = '0';
    else temp[1] = ctemp[dindex+1];

    if(ctemp[dindex+2] == ' ') temp[2] = '0';
    else temp[2] = ctemp[dindex+2];

    temp[3] = ctemp[dindex+3];
    temp[4] = ctemp[dindex+5];
    temp[5] = ctemp[dindex+6];
    temp[6] = ctemp[dindex+8];
    temp[7] = ctemp[dindex+9];
    temp[8] = '\0';
    idate = atoi(temp);

    tindex = tindex + *timestr;
    dindex = dindex + *timestr;
    if((itime == req_time ) && (idate == reqdate)){
       wrfptr->ntime = index;
       break;
    }
  }

  if(wrfptr->ntime == -1){
     printf("REQUESTED DATE NOT FOUND\n");
     exit(0);
  }

  temptwo[0] = temp[0];
  temptwo[1] = temp[1];
  temptwo[2] = temp[2];
  temptwo[3] = temp[3];
  temptwo[4] = '\0';
  dttm[0] = atoi(temptwo);


  temptwo[0] = temp[4];
  temptwo[1] = temp[5];
  temptwo[2] = '\0';    
  dttm[1] = atoi(temptwo);


  temptwo[0] = temp[6];
  temptwo[1] = temp[7];
  temptwo[2] = '\0';    
  dttm[2] = atoi(temptwo);  

  dttm[3] = itime;
  dttm[4] = itime;

  volname[0] = temp[4];
  volname[1] = temp[5];
  volname[2] = temp[6];
  volname[3] = temp[7];
  volname[4] = dtime[0];
  volname[5] = dtime[1];
  volname[6] = dtime[2];
  volname[7] = dtime[3];
}/*wrfdate*/
/******************************************************/
void rundt_(char thedate[8],char thetime[8])
{

  time_t dtime;
  struct tm *dt; 
  
  char ctemp[10];

  /*
   *Run date.
   */
  time(&dtime);
  strftime(ctemp,9,"%Y%m%d",localtime(&dtime));
 
  thedate[0] = ctemp[0];
  thedate[1] = ctemp[1];
  thedate[2] = ctemp[2];
  thedate[3] = ctemp[3];
  
  thedate[4] = ctemp[4];
  thedate[5] = ctemp[5];

  thedate[6] = ctemp[6];
  thedate[7] = ctemp[7];

  /*
   *Run time.
   */
  strftime(ctemp,9,"%H%M%S",localtime(&dtime));
  thetime[0] = ctemp[0];
  thetime[1] = ctemp[1];


  thetime[2] = ctemp[2];
  thetime[3] = ctemp[3];

  thetime[4] = ctemp[4];
  thetime[5] = ctemp[5];
  thetime[6] = '0';
  thetime[7] = '0';
}/*rundt*/
  
/******************************************************/
void wrfclos_()
{
  extern struct wrf_cdf *wrfptr;

   nc_close(wrfptr->ncid);
   wrfptr->ncid = -1;

}
/******************************************************/
void calc_freq_vars()
{
  int nc_status,rcode,index,nplane;
  int ss,ls,fields,nx,ny,nz,id,ivar,nlev,vid;
  int dim[3],cdfdims[3],fid[4];
  float *rtb,*rtp,*athree,*pressure,*map,*dzetadz;
  float *rhotheta;
  long   start[5],count[5];
  size_t rstart[3],rcount[3];
  extern struct wrf_cdf *wrfptr;
  void wrfusrvar_();
  void wrfsrfvar_();
  void closetmp_();


  /*
   *If we are using VERSION 1 of WRF then we can use this routine to calculate
   *some of the variables that are frequently used in the diagnostic variable
   *calculations.
   *If we are using VERSION 2 of WRF then the variables calculated in this
   *routine are not needed so we just return.
   */
   if(wrfptr->model_version == 2) return;

   wrfptr->editncid = nccreate("temp.cdf", NC_CLOBBER );
   if (wrfptr->editncid <= 0){
	printf("UNABLE TO OPEN WRF TEMPORARY NETCDF FILE in calc freq vars\n");
        exit(0);
   }

   ss = 8;
   rcode = nc_def_dim( wrfptr->editncid, "short_string",ss ,&id);
   fields = 2;
   rcode = nc_def_dim( wrfptr->editncid, "fields",fields ,&id);
   nx = wrfptr->thermox;
   rcode = nc_def_dim( wrfptr->editncid, "numx",nx ,&id);
   if(rcode != NC_NOERR) printf("UNABLE TO WRITE NUMX\n");
   ny = wrfptr->thermoy;
   cdfdims[2] = id;
   rcode = nc_def_dim( wrfptr->editncid, "numy",ny,&id);
   if(rcode != NC_NOERR) printf("UNABLE TO WRITE NUMY\n");
   cdfdims[1] = id;
   nz = wrfptr->thermoz;
   rcode = nc_def_dim( wrfptr->editncid, "levels",nz,&id);
   cdfdims[0] = id;
   if(rcode != NC_NOERR) printf("UNABLE TO WRITE MAX LEVELS\n");
              
    dim[0] = cdfdims[0];
    dim[1] = cdfdims[1];
    dim[2] = cdfdims[2];
    rcode = nc_def_var(wrfptr->editncid,"PRESSURE",NC_FLOAT,3,dim,&id);
    if(rcode != NC_NOERR){
        printf("UNABLE TO WRITE RHOTHETA VARIABLE TO TEMP NETCDF FILE\n");
        exit(0);
    }
    fid[0] = id;

    rcode = nc_def_var(wrfptr->editncid,"RTBRTP",NC_FLOAT,3,dim,&id);
    if(rcode != NC_NOERR){
        printf("UNABLE TO WRITE RTB+RTP VARIABLE TO TEMP NETCDF FILE\n");
        exit(0);
    }
    fid[1] = id;

    rcode = nc_def_var(wrfptr->editncid,"RHOTHETA",NC_FLOAT,3,dim,&id);
    if(rcode != NC_NOERR){
        printf("UNABLE TO WRITE RTB+RTP VARIABLE TO TEMP NETCDF FILE\n");
        exit(0);
    }
    fid[2] = id;


    ncendef(wrfptr->editncid );

    rtb = (float *)malloc(nx*ny*sizeof(float));
    if(!rtb){
            printf("OUT OF MEMORY\n");
            exit(0);
    }
        

    rtp = (float *)malloc(nx*ny*sizeof(float));
    if(!rtp){
            printf("OUT OF MEMORY\n");
            exit(0);
    }

    athree = (float *)malloc(nx*ny*sizeof(float));
    if(!athree){
            printf("OUT OF MEMORY\n");
            exit(0);
    }


   ivar = wrfptr->map;
   map = (float *)malloc(nx*ny * sizeof(float));
   if(!map){
      printf("unable to malloc arrays for calculating pressure\n");
      exit(0);
   }
   wrfsrfvar_(&ivar,map);

   ivar = wrfptr->dzetadz;   
   dzetadz = (float *)malloc(nx*ny * sizeof(float));
   if(!dzetadz){
      printf("unable to malloc arrays for calculating pressure\n");
      exit(0);
   }
   wrfsrfvar_(&ivar,dzetadz);


   pressure = (float *)malloc(nx*ny * sizeof(float));
   if(!pressure){
      printf("unable to malloc arrays for calculating pressure\n");
      exit(0);
   }

   rhotheta = (float *)malloc(nx*ny * sizeof(float));
   if(!rhotheta){
      printf("unable to malloc arrays for calculating pressure\n");
      exit(0);
   }
   
   /*
    *Now get the data.
    */
    for(index = 0; index < nz; index++){
        nlev = index + 1;
        ivar = wrfptr->rtb;
        nlev = index+1;
        wrfusrvar_(&ivar,&nlev,rtb);
 
        ivar = wrfptr->rtp;
        nlev = index+1;
        wrfusrvar_(&ivar,&nlev,rtp);

        for(nplane = 0; nplane < nx*ny; nplane++){
            athree[nplane] = rtb[nplane] + rtp[nplane];
            rhotheta[nplane] = (map[nplane]*dzetadz[nplane])*athree[nplane];
        }
      
        calctmpp_(&nx,&ny,rhotheta,pressure);
        

        rstart[1] = rstart[2] = 0;
        rstart[0] = index;
        rcount[0] = 1;
        rcount[1] = wrfptr->thermoy;
        rcount[2] = wrfptr->thermox;
        vid = fid[0];
        rcode = nc_put_vara_float(wrfptr->editncid,vid,rstart,rcount,pressure);
        if(rcode != NC_NOERR){
          printf("unable to write pressure to temp netcdf file %d\n",index);
          exit(0);
	}

        vid = fid[1];
        rcode = nc_put_vara_float(wrfptr->editncid,vid,rstart,rcount,athree);
        if(rcode != NC_NOERR){
          printf("unable to write data to temp netcdf file %d\n",index);
          exit(0);
        }

        vid = fid[2];
        rcode = nc_put_vara_float(wrfptr->editncid,vid,rstart,rcount,rhotheta);
        if(rcode != NC_NOERR){
          printf("unable to write rhotheta to temp netcdf file %d\n",index);
          exit(0);
        }

    }

    free(rtb);
    free(rtp);
    free(athree);
    free(map);
    free(dzetadz);
    free(rhotheta);
    free(pressure);

}/*calc_freq_vars*/

/******************************************************/
void intrpcls_()
{

  extern struct wrf_cdf *wrfptr;

  nc_close(wrfptr->intrpncid);

}/*intrpcls*/
/******************************************************/
void openint_()
{

  int id,nc_status;
  extern struct wrf_cdf *wrfptr;


   wrfptr->intrpncid = -1;
   nc_status = nc_open("intemp.cdf", NC_NOWRITE, &id);
   if (nc_status != 0){
        printf("UNABLE TO OPEN WRF INTERPOLATED NETCDF FILE\n");
        exit(0);
   }

  wrfptr->intrpncid = id;
}/*openint*/
/******************************************************/
void getintfld_(int *ifld,int *nx,int *ny,int *level,float *fdata)
{

   int rcode,varx,vary,ivar;
   long   start[5],count[5];
   extern struct wrf_cdf *wrfptr;  

  ivar = *ifld - 1;
  start[1] = start[2] = 0;
  start[0] = *level-1;
  count[0] = 1;
  count[1] = *ny;
  count[2] = *nx;
  rcode = ncvarget(wrfptr->intrpncid,ivar,start,count,fdata); 
  if(rcode != NC_NOERR){
        printf("unable to get data array\n");
        nc_close(wrfptr->intrpncid);
        exit(-1);
  }

}/*getintfld*/
/******************************************************/
 void opntemp_()
{
  int id,nc_status;
  extern struct wrf_cdf *wrfptr;

   if(wrfptr->editncid != -1) return;
   nc_status = nc_open("temp.cdf", NC_NOWRITE, &id);
   if (id <= 0){
        printf("UNABLE TO OPEN WRF TEMPORARY NETCDF FILE\n");
        exit(0);
   }

  wrfptr->editncid = id;
}/*opntemp*/
/******************************************************/
 void closetmp_()
{
 extern struct wrf_cdf *wrfptr;  

 nc_close(wrfptr->editncid);

 wrfptr->editncid = -1;
}/*closetemp*/

/******************************************************/
void rmtemp_()
{

 int test;
 char cmd[11];

 strcpy(cmd,"rm temp.cdf");
 system(cmd);

 }/*rmtemp*/
/******************************************************/
void rmintrp_()
{
 int test;
 char cmd[11];

 strcpy(cmd,"rm intemp.cdf");
 system(cmd);

 }/*rmintrp*/
/******************************************************/
/*check field name*/
void chkfldn_(char name[8],int *fldnum, int *iscale)
{

  struct ncvar {			/* variable */
    char name[NC_MAX_NAME];
    nc_type type;
    int ndims;
    int dims[MAX_VAR_DIMS];
    int natts;
    boolean has_fillval;
    double fillval;
  };

  struct ncvar var;
  int found,index,length;
  int num,uservar;
  extern struct wrf_cdf *wrfptr;

  num = *fldnum - 1;
  if(num == 0) calc_freq_vars();

  /*
   *These variables are not in the WRF netcdf file and
   *need to be calculated.
   */
  uservar = 0;
  if(strncmp(name,"THETA",5) == 0){
     uservar = 1;
     *iscale = 10;
  }
  if(strncmp(name,"ZTOTAL",6) == 0){
     uservar = 1;
     *iscale = 10;
  }
  if(strncmp(name,"ZRAIN",5) == 0){
     uservar = 1;
     *iscale = 10;
  }
  if(strncmp(name,"SLVL",4) == 0){
      uservar = 1;
      *iscale = 10;
  }
  if(name[0] == 'P' && name[1] == ' '){
      uservar = 1;
      *iscale = 10;
  }
  if(name[0] == 'P' && name[1] == '\0'){
      uservar = 1;
      *iscale = 10;
  }
  if(name[0] == 'T') {
     if(name[1] == 'C' && name[2] == ' '){
        uservar = 1;
        *iscale = 100;
     }
     if(name[1] == 'D' && name[2] == ' '){
        uservar = 1;
        *iscale = 100;
     }
  }
  if(name[0] == 'T' && name[1] == '\0') uservar = 1;
  if(name[0] == 'U' && name[1] == '\0'){
     uservar = 1;
     *iscale = 100;
  }
  if(name[0] == 'U' && name[1] == ' '){
      uservar = 1;
      *iscale = 100;
  }
  if(name[0] == 'V' && name[1] == '\0'){
      uservar = 1;
      *iscale = 100;
  }
  if(name[0] == 'V' && name[1] == ' '){
      uservar = 1;
      *iscale = 100;
  }
  if(name[0] == 'W' && name[1] == '\0'){
     uservar = 1;
     *iscale = 100;
  }
  if(name[0] == 'W' && name[1] == ' ') {
     uservar = 1;
     *iscale = 100;
  }
  if(name[0] == 'R' && name[1] == 'H' && name[3] == ' '){
       uservar = 1;
       *iscale = 100;
  }
  if(uservar == 1){
     wrfptr->varids[num] = -1;
     return;
  }
  /*------------------------------------------------*/
  if(strncmp(name,"RHO_U",4 ) == 0) *iscale = 100;
  else if(strncmp(name,"RHO_V",4 ) == 0) *iscale = 100;
  else if(strncmp(name,"RW",2 ) == 0) *iscale = 100;
  else if(strncmp(name,"XLAT",4) == 0) *iscale = 100;
  else if(strncmp(name,"XLONG",4) == 0) *iscale = 100;
  else if(strncmp(name,"Z ",2) == 0) *iscale = 1;
  else if(strncmp(name,"RRP",3) == 0) *iscale = 1000;
  else if(strncmp(name,"RRB",3) == 0) *iscale = 100;
  else if(strncmp(name,"RR ",3) == 0) *iscale = 1000;
  else if(strncmp(name,"RTP",3) == 0) *iscale = 100;
  else if(strncmp(name,"RTB",3) == 0) *iscale = 10; 
  else if(strncmp(name,"QVAPOR",6) == 0) *iscale = 1000; 
  else if(strncmp(name,"QCLOUD",6) == 0) *iscale = 1000;
  else if(strncmp(name,"QRAIN",5) == 0) *iscale = 1000;
  else if(strncmp(name,"QICE",4) == 0) *iscale = 1000;
  else if(strncmp(name,"QGRAP",5) == 0) *iscale = 1000;
  else if(strncmp(name,"XLAND",1) == 0) *iscale = 100; 
  else if(strncmp(name,"DZETADZ",7) == 0) *iscale = 100; 
  else if(strncmp(name,"MAPFAC_M",8) == 0) *iscale = 100; 
  else if(strncmp(name,"HGT",3) == 0) *iscale = 1;
  else *iscale = 10;
  found = 0;
  for (index = 0; index  < wrfptr->nvars; index++) {
       nc_inq_var(wrfptr->ncid, index, var.name, &var.type, &var.ndims,
		  var.dims, &var.natts); 
       length = strlen(var.name); 
       if(strncmp(var.name,name,length) == 0){
          if(length == 8){
             found = 1;
             wrfptr->varids[num] = index;
             wrfptr->vardims[num] = var.ndims;
	  }	
          else{
             if(name[length] == ' '){ 
                found = 1;
                wrfptr->varids[num] = index;
                wrfptr->vardims[num] = var.ndims;
	     }
	  }
       }
  }

  if(found == 0){
     printf("%c%c%c%c%c%c%c%c",name[0],name[1],name[2],name[3],
                               name[4],name[5],name[6],name[7]);
     printf(" was not found in this data set\n");
     exit(0);
  }
       
}/*chkfldn*/

/******************************************************/
void wrflevels_(int *level,int *num)
{

  extern struct wrf_cdf *wrfptr;

  if(wrfptr->vardims[*num-1] < 4) *level = 1;
  else *level = wrfptr->varlevels[*num-1];
}/*wrflevels*/

/********************************************************************/
void name_of_var(char varn[12],int *ivar, int numc)
{
  struct ncvar {			/* variable */
    char name[NC_MAX_NAME];
    nc_type type;
    int ndims;
    int dims[MAX_VAR_DIMS];
    int natts;
    boolean has_fillval;
    double fillval;
  };

struct ncatt {			/* attribute */
    int var;
    char name[NC_MAX_NAME];
    nc_type type;
    size_t len;
    char *string;		/* for text attributes (type = NC_CHAR) */
    double *vals;		/* for numeric attributes of all types */
};


  struct ncvar var;
  struct ncatt att;		/* attribute */
  int    index;
  int    rcode;
  int    length;
  extern struct wrf_cdf *wrfptr;

  *ivar = -1;
  length = numc;
  for (index = 0; index  < wrfptr->nvars; index++){
       nc_inq_var(wrfptr->ncid, index, var.name, &var.type, &var.ndims,
		    var.dims, &var.natts);

       if(strncmp(var.name,varn,length) == 0){
	 if(var.name[length] == '\0'){
                  *ivar = index;
                  return;
	 }
       }
  }
}/*name_of_var*/
/**********************************************************************/
void wrfusrvar_(int *ivar,int *nlev,float *fdata)
{
  struct ncvar {			/* variable */
    char name[NC_MAX_NAME];
    nc_type type;
    int ndims;
    int dims[MAX_VAR_DIMS];
    int natts;
    boolean has_fillval;
    double fillval;
  };

  struct ncvar var;
  int level,ydim,xdim;
  int index,rcode,jj,ii;
  long   start[5],count[5],icount;
  int    nplane;
  float qvar;

  /*-------------------------------------------------------------------*/
 /*
  *The following union and unsigned longs are for traping
  *very small numbers on the Dec assuming a 32 bit machine.
  *that is a sign bit, and 8-bit exponent and a 23 bit mantissa.
  */
  union {float x; unsigned long ix;} u;
  unsigned long signmask = 0x80000000;
  unsigned long expmask = 0x7f800000;
  unsigned long restmask = 0x007fffff;
  unsigned long totalmask = 0xffffffff;
  /*-------------------------------------------------------------------*/
  extern struct wrf_cdf *wrfptr;


  nc_inq_var(wrfptr->ncid, *ivar, var.name, &var.type, &var.ndims,
	     var.dims, &var.natts);


  ydim = wrfptr->dimsizes[var.dims[2]];
  xdim = wrfptr->dimsizes[var.dims[3]];
  start[0] = (long)wrfptr->ntime;
  start[1] = (long)*nlev-1;
  start[2] = (long)0;
  start[3] = (long)0;
  count[0] = 1;  
  count[1] = (long)1;  
  count[2] = (long)ydim;
  count[3] = (long)xdim; 

  rcode = ncvarget(wrfptr->ncid,*ivar,start,count,fdata); 
  if(rcode != NC_NOERR){
     printf("unable to get data array\n");
     nc_close(wrfptr->ncid);
     exit(-1);
  }
 
 /*Code for traping small numbers on a 32 bit machines.
  *Here we do a bit wise and of the buffer value with the 
  *exponent mask.  Then we shift right by 23  to get the 
  *number.
  */
   for(nplane = 0; nplane < count[2]*count[3]; nplane++){
       u.x = fdata[nplane];
       qvar = (u.ix & expmask) >> 23;
      /*
       *A value of 5 cooresponds to 1e-37 on the dec.
       */
       if( qvar < 6) fdata[nplane] = 0.0;
   }

}/*wrfusrvar*/
/********************************************************************/
void tmpvar_(int *ivar,int *nlev,float *fdata)
{
 
  int level,ydim,xdim;
  int index,rcode,jj,ii;
  long   start[5],count[5],icount;
  extern struct wrf_cdf *wrfptr;

  start[1] = start[2] = 0;
  start[0] = *nlev-1;
  count[0] = 1;
  count[1] = wrfptr->thermoy;
  count[2] = wrfptr->thermox;


  rcode = ncvarget(wrfptr->editncid,*ivar,start,count,fdata); 
  if(rcode != NC_NOERR){
     printf("unable to get data array\n");
     nc_close(wrfptr->editncid);
     exit(-1);
  }
 
}/*tmpvar*/
/********************************************************************/
void wrfsrfvar_(int *ivar,float *fdata)
{
  struct ncvar {			/* variable */
    char name[NC_MAX_NAME];
    nc_type type;
    int ndims;
    int dims[MAX_VAR_DIMS];
    int natts;
    boolean has_fillval;
    double fillval;
  };

  struct ncvar var;
  int  ydim,xdim;
  int rcode,thevar;
  long   start[5],count[5],icount;
  extern struct wrf_cdf *wrfptr;

  nc_inq_var(wrfptr->ncid, *ivar, var.name, &var.type, &var.ndims,
	     var.dims, &var.natts);

  ydim = wrfptr->dimsizes[var.dims[1]];
  xdim = wrfptr->dimsizes[var.dims[2]];
  
  start[0] = wrfptr->ntime;
  start[1] = 0;
  start[2] = 0;
  count[0] = 1;
  count[1] = ydim;
  count[2] = xdim;

  rcode = ncvarget(wrfptr->ncid,*ivar,start,count,fdata);
  if(rcode != NC_NOERR){
     printf("unable to get data array\n");
     nc_close(wrfptr->ncid);
     exit(-1);
  }

}/*wrfthreevv*/

/******************************************************************/
/*CALCULATION OF SPECIAL VARIABLES NOT IN THE WRF NETCDF DATA SET*/
/******************************************************************/
void special_var_ids(int *nx,int *ny, int *level, int *ifree)
{

  int x,y,z,ivar,dims[3],numc;
  char var[12];
  extern struct wrf_cdf *wrfptr;

  
  strcpy(var,"RTB");
  numc = 3;
  name_of_var(var,&ivar,numc);
  wrfptr->rtb = ivar;

  strcpy(var,"RR");
  numc = 2;
  name_of_var(var,&ivar,numc);
  wrfptr->rr = ivar;
 
  strcpy(var,"RTP");
  numc = 3;
  name_of_var(var,&ivar,numc);
  wrfptr->rtp = ivar;

  strcpy(var,"QVAPOR");
  numc = 6;
  name_of_var(var,&ivar,numc);
  wrfptr->qvapor = ivar;

  strcpy(var,"MAPFAC_M");
  numc = 8;
  name_of_var(var,&ivar,numc);
  wrfptr->map = ivar;  

  strcpy(var,"DZETADZ");
  numc = 7;
  name_of_var(var,&ivar,numc);  
  wrfptr->dzetadz = ivar;  

  strcpy(var,"RHO_U");
  numc = 5;
  name_of_var(var,&ivar,numc);
  wrfptr->rho_u = ivar;

  strcpy(var,"RHO_V");
  numc = 5;
  name_of_var(var,&ivar,numc);
  wrfptr->rho_v = ivar;

  strcpy(var,"RW");
  numc = 2;
  name_of_var(var,&ivar,numc);
  wrfptr->rw = ivar;

  strcpy(var,"HGT");
  numc = 3;
  name_of_var(var,&ivar,numc);
  wrfptr->hgt = ivar;

  strcpy(var,"QRAIN");
  numc = 5;
  name_of_var(var,&ivar,numc);
  wrfptr->qrain = ivar;
  
  strcpy(var,"QSNOW");
  numc = 5;
  name_of_var(var,&ivar,numc);
  wrfptr->qsnow = ivar;
  

  strcpy(var,"QGRAUP");
  numc = 6;
  name_of_var(var,&ivar,numc);
  wrfptr->qgraup = ivar;

  var[0] = 'Z';
  var[1] = ' ';
  numc = 1;
  ivar = -1;
  name_of_var(var,&ivar,numc);
  wrfptr->gpoint_hgt = ivar;
}/*special_var_ids*/
/******************************************************************/

void getdims_(int *x,int *y,int *fldnum)
{
  struct ncvar {                        /* variable */
    char name[NC_MAX_NAME];
    nc_type type;
    int ndims;
    int dims[MAX_VAR_DIMS];
    int natts;
    boolean has_fillval;
    double fillval;
  };
  struct ncvar var;
  int ivar,num;
  extern struct wrf_cdf *wrfptr;

 num = *fldnum - 1;
 ivar = wrfptr->varids[num];

 if(ivar == -1){
    *x = -1;
    *y = 1;
    return;
 }

 nc_inq_var(wrfptr->ncid, ivar, var.name, &var.type, &var.ndims,
             var.dims, &var.natts);


 if(var.ndims == 4){
   *y = wrfptr->dimsizes[var.dims[2]];
   *x = wrfptr->dimsizes[var.dims[3]];
 }
 else{
   *y = wrfptr->dimsizes[var.dims[1]];
   *x = wrfptr->dimsizes[var.dims[2]];
 }  
 
}/*getdims*/
/******************************************************************/
void winddim_(int *x, int *y, int *z, int *wind)
{
  struct ncvar {			/* variable */
    char name[NC_MAX_NAME];
    nc_type type;
    int ndims;
    int dims[MAX_VAR_DIMS];
    int natts;
    boolean has_fillval;
    double fillval;
  };

  struct ncvar var;
  int index;
  char name[5];
  extern struct wrf_cdf *wrfptr;
  
  name[0] = 'R';
  name[1] = 'H';
  name[2] = 'O';
  name[3] = '_';

  if(*wind == 1) name[4] = 'U';
  if(*wind == 2) name[4] = 'V';  
  if(*wind == 3){
     name[0] = 'R';
     name[1] = 'W';
     name[2] = ' ';
     name[3] = ' ';
     name[4] = ' ';
  }

  for (index = 0; index  < wrfptr->nvars; index++){
    nc_inq_var(wrfptr->ncid, index, var.name, &var.type, &var.ndims,
               var.dims, &var.natts);
      if(*wind < 3){
         if(strncmp(var.name,name,5) == 0){
            *z = wrfptr->dimsizes[var.dims[1]];
            *y = wrfptr->dimsizes[var.dims[2]];
            *x = wrfptr->dimsizes[var.dims[3]];
            wrfptr->model_version = 1;
            return;
         }
      }
      else{
	if(strncmp(var.name,name,2) == 0){
            *z = wrfptr->dimsizes[var.dims[1]];
            *y = wrfptr->dimsizes[var.dims[2]];
            *x = wrfptr->dimsizes[var.dims[3]];
            wrfptr->model_version = 1;
            return;
	}
      }         
  } 

  /*
   *RHO_ variable was not found in this data set. So check for U,V or W
   */
   
  if(*wind == 1) name[0] = 'U';
  if(*wind == 2) name[0] = 'V'; 
  if(*wind == 3) name[0] = 'W';   
  for (index = 0; index  < wrfptr->nvars; index++){
    nc_inq_var(wrfptr->ncid, index, var.name, &var.type, &var.ndims,
               var.dims, &var.natts);
    if(*wind == 1){
      if(var.name[0] == 'U'){
	if(var.name[1] == ' '){
            *z = wrfptr->dimsizes[var.dims[1]];
            *y = wrfptr->dimsizes[var.dims[2]];
            *x = wrfptr->dimsizes[var.dims[3]];
            wrfptr->model_version = 2;
            wrfptr->rho_u = index;
            return;
        }
        if(var.name[1] == '\0'){
            *z = wrfptr->dimsizes[var.dims[1]];
            *y = wrfptr->dimsizes[var.dims[2]];
            *x = wrfptr->dimsizes[var.dims[3]];
            wrfptr->model_version = 2;
            wrfptr->rho_u = index;
            return;
        }
      }  
    }


    if(*wind == 2){
      if(var.name[0] == 'V'){
	if(var.name[1] == ' '){
            *z = wrfptr->dimsizes[var.dims[1]];
            *y = wrfptr->dimsizes[var.dims[2]];
            *x = wrfptr->dimsizes[var.dims[3]];
            wrfptr->rho_v = index;
            return;
        }
        if(var.name[1] == '\0'){
            *z = wrfptr->dimsizes[var.dims[1]];
            *y = wrfptr->dimsizes[var.dims[2]];
            *x = wrfptr->dimsizes[var.dims[3]];
            wrfptr->rho_v = index;
            return;
        }
      }  
    }

    if(*wind == 3){
      if(var.name[0] == 'W'){
	if(var.name[1] == ' '){
            *z = wrfptr->dimsizes[var.dims[1]];
            *y = wrfptr->dimsizes[var.dims[2]];
            *x = wrfptr->dimsizes[var.dims[3]];
            return;
        }
        if(var.name[1] == '\0'){
            *z = wrfptr->dimsizes[var.dims[1]];
            *y = wrfptr->dimsizes[var.dims[2]];
            *x = wrfptr->dimsizes[var.dims[3]];
            return;
        }
      }  
    }
  }/*for loop*/

         
  if(*wind == 1) printf("U NOT FOUND IN THIS DATA SET\n");
  if(*wind == 2) printf("V NOT FOUND IN THIS DATA SET\n");
  if(*wind == 3) printf("W NOT FOUND IN THIS DATA SET\n");

  exit(0);
}/*winddim*/
/******************************************************************/
void thermodim_(int *nx, int *ny)
{
  extern struct wrf_cdf *wrfptr;

  *nx = wrfptr->thermox;
  *ny = wrfptr->thermoy;
}/*thermodim*/
/******************************************************************/
void gthermoz_(int *nz)
{
  extern struct wrf_cdf *wrfptr;

  *nz = wrfptr->thermoz;
}/*gthermoz*/
/**********************************************************/
void fvarid_(int *iname,int *ivar)
{
    

  struct ncvar {			/* variable */
    char name[NC_MAX_NAME];
    nc_type type;
    int ndims;
    int dims[MAX_VAR_DIMS];
    int natts;
    boolean has_fillval;
    double fillval;
  };

 struct ncvar var;
 int field,index;
 extern struct wrf_cdf *wrfptr;

 field = *iname;
 switch(field){
    case 1:{/*RHO_U*/
      *ivar = wrfptr->rho_u;
      break;
    }
    case 2:{/*RHO_V*/
      *ivar = wrfptr->rho_v;
      break;
    }
       
     case 3:{/*RW*/
      *ivar = wrfptr->rw;
      break;
    }
          
    case 5:{/*RTB*/
      *ivar = wrfptr->rtb;
      break;
    }
     case 6:{/*RTP*/
      *ivar = wrfptr->rtp;
      break;
    }    
    case 7:{/*RR*/
      *ivar = wrfptr->rr;
      break;
    }
     case 8:{/*QVAPOR*/
      *ivar = wrfptr->qvapor;
      break;
    }  
     case 9:{/*MAPFAC_M*/
      *ivar = wrfptr->map;
      break;
    }  
     case 10:{/*DZETADZ*/
      *ivar = wrfptr->dzetadz;
      break;
    }  
    case 11:{/*Z*/
      *ivar = wrfptr->gpoint_hgt;
      break;
    }
    case 12:{
      *ivar = wrfptr->hgt;
      break;
    }
    case 18:{
      *ivar = wrfptr->qrain;
      break;
    }  
    case 19:{
      *ivar = wrfptr->qsnow;
      break;
    }
    case 20:{
      *ivar = wrfptr->qgraup;
      break;
    }
    default:{
      for (index = 0; index  < wrfptr->nvars; index++){
          nc_inq_var(wrfptr->ncid, index, var.name, &var.type, &var.ndims,
               var.dims, &var.natts);
          if(*iname == 21){
               if(var.name[0] == 'P'){
	          if(var.name[1] == ' ' || var.name[1] == '\0'){
                     *ivar = index;
                     return;
		  }
	       }
	  }
	  if(*iname == 22){
	    if(var.name[0] == 'P' && var.name[1] == 'B'){
	      if(var.name[2] == ' ' || var.name[2] == '\0'){
                 *ivar = index;
                 return;
	      }
	    }             
	  }
      }/*for loop*/
      
      printf("variable not found\n");
      exit(0);
    }
   
 }/*switch*/


}/*fvarindo*/
/**********************************************************/
/* ROUTINES ADDED TO CEDRIC TO ACCOMODATE VERSION 2 OF THE MODEL*/
/***********************************************************/
void wrfversion_(int *version)
{
  extern struct wrf_cdf *wrfptr;

  if(wrfptr->model_version == 1) *version = 1;
  if(wrfptr->model_version == 2) *version = 2;

}/*wrfversion_*/
/***********************************************************/


