# include <stdio.h>
# include "dorade.h"

#if defined (IBMRISC) || defined (HP)
void rdbeam(inunit, irewnd, istat, ivolnum, iyr, mon,iday,
             ihr,min,isec,msec,numrads,iscnmode,nflds,
             numfreq,numipp,ngates,iswpnum,julday,irystat,
             radar_type,reqtime,radcon,scanrate,radlong,radlat,
             vnyq,rngmx,frstgat,gatspac,azim,elev,altmsl,presalt,altgnd,
             gndspdew,gndspdns,vervel,heading,roll,pitch,drift,
             rotang,tilt,uair, vair, wair,hedchgrt,pitchgrt, flddat,
             scale,offset,bad,fxang,radnam,fldnam,proj_name,fltnum,
	     swapping)

#elif defined (CRAY)
void RDBEAM(inunit, irewnd, istat, ivolnum, iyr, mon,iday,
             ihr,min,isec,msec,numrads,iscnmode,nflds,
             numfreq,numipp,ngates,iswpnum,julday,irystat,
             radar_type,reqtime,radcon,scanrate,radlong,radlat,
             vnyq,rngmx,frstgat,gatspac,azim,elev,altmsl,presalt,altgnd,
             gndspdew,gndspdns,vervel,heading,roll,pitch,drift,
             rotang,tilt,uair, vair, wair,hedchgrt,pitchgrt, flddat,
             scale,offset,bad,fxang,radnam,fldnam,proj_name,fltnum,
	     swapping)

#else
void rdbeam_(inunit, irewnd, istat, ivolnum, iyr, mon,iday,
             ihr,min,isec,msec,numrads,iscnmode,nflds,
             numfreq,numipp,ngates,iswpnum,julday,irystat,
             radar_type,reqtime,radcon,scanrate,radlong,radlat,
             vnyq,rngmx,frstgat,gatspac,azim,elev,altmsl,presalt,altgnd,
             gndspdew,gndspdns,vervel,heading,roll,pitch,drift,
             rotang,tilt,uair, vair, wair,hedchgrt,pitchgrt, flddat,
             scale,offset,bad,fxang,radnam,fldnam,proj_name,fltnum,swapping)
#endif

     int  *inunit,*irewnd,*istat,*ivolnum;
     int  *iyr,*mon,*iday,*ihr,*min,*isec,*msec;
     int *numrads, *iscnmode, *nflds, *numfreq, *numipp, *ngates;
     int *iswpnum, *julday, *irystat,*radar_type;
     float  *reqtime;
     float *radcon, *scanrate, *radlong, *radlat, *vnyq, *rngmx;
     float *frstgat, *gatspac, *azim, *elev, *altmsl, *presalt, *altgnd;
     float *gndspdew,*gndspdns,*vervel,*heading,*roll,*pitch,*drift;
     float *rotang, *tilt, *uair, *vair, *wair;
     float *hedchgrt,*pitchgrt, flddat[MXFLD][MXGAT];
     float scale[MXFLD], offset[MXFLD],*bad,*fxang;
     char radnam[8],fldnam[MXFLD][8],proj_name[4],fltnum[8];
     int *swapping;
{

  int i, j, ij;
  int change_name;
  static char cradnam[9];
  static char cproject[5];
  char cfltnum[9];
  static char cfldnam[MXFLD][9];
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
             nflds,numfreq,numipp,ngates,iswpnum,julday,irystat,
             radar_type,reqtime,radcon,scanrate,radlong,radlat,
             vnyq,rngmx,frstgat,gatspac,azim,elev,altmsl,presalt,altgnd,
             gndspdew,gndspdns,vervel,heading,roll,pitch,drift,
             rotang,tilt,uair,vair,wair,hedchgrt,pitchgrt,
             flddat,scale,offset,bad,fxang,cfltnum,cfldnam,
             cproject,swapping);

/* convert C chars to FORTRAN */
printf(" +++ In rdbeam \n");
printf(" +++ In rdbeam -  inunit  = %d\n",*inunit);
printf(" +++ In rdbeam -  irystat = %d\n",*irystat);
printf(" +++ In rdbeam - swapping = %d\n",*swapping);

  strncpy(fltnum,"        ",8);
  strncpy(fltnum,cfltnum,8);
 

  if(change_name == 1){
      for (i = 0; i < 8; i++)  radnam[i] = cradnam[i];
  }


  strncpy(proj_name,cproject,4);
  for (i = 0; i < *nflds; i++) {
      for(j = 0; j < 8; j++){
          if(cfldnam[i][j] != ' ' && cfldnam[i][j] != '\0')
             fldnam[i][j] = cfldnam[i][j];
          else
             fldnam[i][j] = ' ';
       }
  }

  return;

}
