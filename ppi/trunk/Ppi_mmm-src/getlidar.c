/*******************************************************************

 ** Copyright (c) 1993, UCAR
 ** University Corporation for Atmospheric Research(UCAR)
 ** Proprietary and Confidential to UCAR
 ** National Center for Atmospheric Research(NCAR)
 ** Research Applications Program(RAP)
 ** P.O.Box 3000, Boulder, Colorado, 80307, USA
 ** Licensed use only.
 ** Do not copy or distribute without authorization

	Reads lidar data from an FMQ. Reformats the
	data and preprocesses it (clipping, selecting based on frame
	number, fields and so on, scaling ...), and puts it 
	in an output shared memory pipe.

        Note: In the processing we assume the gate size and range 
	are fixed and the same for all fields.

	File: get_lidar.c

*******************************************************************/

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>

#include "gtDefines.h"
#include "cedric.h"
#include "op_sys.h"



#define MAX_N_GATES   512		/* maximum number of gates processed */
#define N_FIELDS_PROC 3			/* number of fields processed */
extern FILE *fp;
unsigned short cswap16();
long cswap32();

/******************************************************************
The main function     
*/
#if defined (IBMRISC) || defined (HP)
     void get_lidar(pow,vel,n_gates,az,el,iyr,imon,iday,itime,r0,dr,scalepow,scalevel,biaspow,
                    biasvel,scantype,ieot,sw,scalesw,biassw,dec)
#elif defined (CRAY)
     void GET_LIDAR(pow,vel,n_gates,az,el,iyr,imon,iday,itime,r0,dr,scalepow,scalevel,biaspow,
                    biasvel,scantype,ieot,sw,scalesw,biassw,dec)
#elif defined (LINUX)
     void get_lidar__(pow,vel,n_gates,az,el,iyr,imon,iday,itime,r0,dr,scalepow,scalevel,biaspow,
                      biasvel,scantype,ieot,sw,scalesw,biassw,dec)
#else
     void get_lidar_(pow,vel,n_gates,az,el,iyr,imon,iday,itime,r0,dr,scalepow,scalevel,biaspow,
                     biasvel,scantype,ieot,sw,scalesw,biassw,dec)
#endif

char *pow;
char *vel;
char *sw;
int *n_gates,*scantype,*r0,*dr;
int *scalepow,*scalevel,*biaspow,*biasvel,*scalesw,*biassw;
int *az,*el;
int *itime,*iyr,*imon,*iday,*ieot;
float *dec;


{
    char fmq_name[80];
    static int cnt = 0;


    int i;
    int ihr,imin,isec;
    int stdlen;
    int len,ldlen;
    int hour,min,sec;
    int rval,num;

    date_time_t  *date;

    static unsigned char ld_ray[10000];

    struct lidar_ray_header *ldhd;

    ldhd = (struct lidar_ray_header *)ld_ray;


    /* the main loop */


	/* read a ray */
	if (fread (ldhd, sizeof (struct lidar_ray_header), 1, fp) != 1) {
	    printf ("EOF reached (cnt = %d)\n", 
			cnt);





	    *ieot=1;
	    fclose (fp);
	    return;
	}



        if(*dec == 1.){
	  ldhd->length=cswap16(ldhd->length);

	  ldhd->time=cswap32(ldhd->time);

	  ldhd->azavg=cswap16(ldhd->azavg);
	  ldhd->elavg=cswap16(ldhd->elavg);
	  ldhd->n_fields=cswap16(ldhd->n_fields);
	  ldhd->r_h_pt=cswap16(ldhd->r_h_pt);
	  for(i=0; i<ldhd->n_fields; i++)
	    ldhd->f_pt[i]=cswap16(ldhd->f_pt[i]);


	}

	ldlen = ldhd->length;

	if (ldlen > 10000) {
	    printf ("Lidar data error: length = %d \n", 
			ldlen);
	    fclose (fp);
	    exit(1);
	}


        if(ldhd->scanmode==0)
	  *scantype=1;
        else if(ldhd->scanmode==1)
	  *scantype=3;
        else
          *scantype=ldhd->scanmode;
    

	*az=ldhd->azavg;
	*el=ldhd->elavg;



        convert_from_unix_time(ldhd->time,iyr,imon,iday,&hour,&min,&sec);
        *itime=10000*(hour)+100*(min)+sec;




	len = ldlen - sizeof (struct lidar_ray_header);
	if (fread (ld_ray + sizeof (struct lidar_ray_header), sizeof (char), 
	    len, fp) != len) {
	    printf ("Failed in reading the lidar ray (len = %d, cnt = %d) \n", 
			ldlen, cnt);
	    *ieot=1;
	    fclose (fp);
            return;
	}





	Lidar_to_rdi (ld_ray, ldlen,r0,dr,n_gates,scalepow,scalevel,biaspow,
                 biasvel,pow,vel,sw,scalesw,biassw,dec);






        return;

}

/**********************************************************************
This function reformats a Lidar ray into rdi format.
If g_size = 0, No resample is performed.
Return values: 0 if the data can not be processed, -1 if fatal error,
               or otherwise the length of the rdi ray.				     
*/


Lidar_to_rdi (ldray,leng,r0,dr,n_gates,scalepow,scalevel,biaspow,biasvel,pow,vel,
              sw,scalesw,biassw,dec)



unsigned char *ldray;
int leng;

int *r0,*dr,*n_gates,*scalepow,*scalevel,*biaspow,*biasvel,*scalesw,*biassw;
char *pow,*vel,*sw;
float *dec;
{



    struct lidar_header *ldhd;		/* lidar header */
    struct processor_header *prchd;	/* processor header */

    struct lidar_ray_header *lhd;	/* lidar ray header */

    static int gate_table[MAX_N_GATES];	/* data resampling look up table */

    static int prf;			/* prf value */
    static int scale[N_FIELDS_PROC];	/* data scaling from the processor header */
    static int offset[N_FIELDS_PROC];	/* data offset from the processor header */
    static unsigned char cpt[1024];




    static int init = 0; 		/* flag: processor header info ready */
    static int rcnt = 0; 		/* processed ray number count */

    static int cr_gsize, cr_range;	/* current gate size and range */
    static int cr_n_data;		/* current number of data samples */

    static int f_cnt = 0;       	/* internally generated frame count */
    static int v_cnt = 0;		/* internally generated volume count */

    char *pp,*pv,*psw;
    unsigned char t1,t2;
    int i;
    int field_mask, this_range, this_g_size; /* for f_cnt increment */

    lhd = (struct lidar_ray_header *) ldray;


    /* check length */
    if (leng < sizeof (struct lidar_ray_header) || leng != lhd->length) {
	printf ("Error in lidar ray length: leng = %d and lhd->length = %d \n",
		leng, lhd->length);
	init = 0;
	return (0);
    }

    /* read information from the optional header */

    if (lhd->r_h_pt > 0) {

	init = 0;

        /* check length */
        if (lhd->r_h_pt < sizeof (struct lidar_ray_header)
	    || lhd->r_h_pt + sizeof (struct lidar_header) 
	       + sizeof (struct processor_header) > leng) {
	    printf ("Error: optional headers out of boundary: leng = %d \n",
		    leng);
	    return (0);
        }

	ldhd = (struct lidar_header *) (ldray + lhd->r_h_pt);
	prchd = (struct processor_header *) (ldray + lhd->r_h_pt +
					     sizeof (struct lidar_header));

	/* store the radar header */
	ldhd->system_name[31] = '\0';
	/* store prf and data scaling info */


	for (i = 0; i < N_FIELDS_PROC; i++) {
	    if(*dec == 1.){
	      prchd->scale[i]=cswap16(prchd->scale[i]);
	      prchd->offset[i]=cswap16(prchd->offset[i]);
	    }

	    scale[i] = prchd->scale[i];

	    if (scale[i] == 0) {
		printf ("Lidar data error: zero data scaling (field %d)\n", i);
		return (0);
	    }
	    offset[i] = prchd->offset[i];
	}


	/* static ray header info */


	/* static field header info */

        if(*dec == 1.){
	  prchd->range=cswap16(prchd->range);

	  prchd->n_gates=cswap16(prchd->n_gates);

	  prchd->g_centers=cswap16(prchd->g_centers);
	  prchd->n_data_points=cswap16(prchd->n_data_points);

	}
	    cr_range = prchd->range;

	    cr_gsize = (int)(prchd->g_centers * ((prchd->n_gates)-1)) / 
				(int)((prchd->n_data_points)-1);


	cr_n_data = prchd->n_data_points;

	*r0=prchd->range;

	*n_gates=prchd->n_gates;

	*dr=prchd->g_centers;

	*scalepow=scale[0];

	*scalevel=scale[1];

	*scalesw=scale[2];

	*biaspow=offset[0];

	*biasvel=offset[1];

	*biassw=offset[2];   

	init = 1;
    }


    /* check if data points out of ray boundary */
    for (i = 0; i < N_FIELDS_PROC; i++) {
        if (lhd->f_pt[i] == 0)
	    continue;

	if ((int)(lhd->f_pt[i] + cr_n_data) > (int)lhd->length) {
	    printf ("Lidar data size is too small (field=%d, off=%d, n_gates=%d, len=%d\n",
		    i, lhd->f_pt[i], cr_n_data, lhd->length);
	    init = 0;     /* we'll wait until next optional header comes */
	    return (0);
        }
    }



    /* create the rdi ray */
    {
	unsigned char *cpt, *dpt;
	int field_len, i, j;
	int fd_cnt;
	int tmp;






	/* set up ray header fields */

	/* trap possible data errors */
/*	if (hd->azi > 36000 || hd->ele <= -18000 || hd->ele > 18000) {
	    printf ("Bad data: azi=%d, ele=%d - %s\n", hd->azi, hd->ele, prog_name);
	    init = 0;
	    return (0);
	}
*/
	if (lhd->n_fields > LD_MAX_N_FLDS || lhd->n_fields > N_FIELDS_PROC) {
	    printf ("Bad data: n_fields = %d \n", lhd->n_fields);
	    init = 0;
	    return (0);
	}

	/* process the field data */
	field_mask = 0;
	fd_cnt = 0;

	    for (i = 0; i < N_FIELDS_PROC; i++) {
	        if (lhd->f_pt[i] == 0)
		    continue;


		this_range = cr_range;
		this_g_size = cr_gsize;
		field_mask |= (1 << i);


	        dpt = (unsigned char *) (ldray + lhd->f_pt[i]);


	        /* put the data */

		if(i==0){
		  pp=pow;
		  for(j=0; j<*n_gates; j++){
		    *pp=dpt[j];
		    pp++;
		  }
                }

		else if(i==1){
		  pv=vel;
		  for(j=0; j<*n_gates; j++){
		    *pv=dpt[j];

		    pv++;
		  }
                }
		else{
		  psw=sw;
		  for(j=0; j<*n_gates; j++){
		    
		    *psw=dpt[j];
		    psw++;
		  }
                }






	        fd_cnt++;



	    }

/* if running on a DEC machine, swap data bytes 4 bytes at a time */

	if(*dec == 1.){
	  pp=pow;
	  pv=vel;
	  psw=sw;
	  for(j=0; j<*n_gates; j=j+4){

	    t1=*(pp+j);
	    t2=*(pp+j+1);
	    *(pp+j)=*(pp+j+3);
	    *(pp+j+1)=*(pp+j+2);
	    *(pp+j+2)=t2;
	    *(pp+j+3)=t1;
	    t1=*(pv+j);
	    t2=*(pv+j+1);
	    *(pv+j)=*(pv+j+3);
	    *(pv+j+1)=*(pv+j+2);
	    *(pv+j+2)=t2;
	    *(pv+j+3)=t1;
	    t1=*(psw+j);
	    t2=*(psw+j+1);
	    *(psw+j)=*(psw+j+3);
	    *(psw+j+1)=*(psw+j+2);
	    *(psw+j+2)=t2;
	    *(psw+j+3)=t1;
	  }
	}

	if (lhd->n_fields != fd_cnt) {
	    printf ("Bad data: n_fields = %d while fd_cnt = %d \n",
		    lhd->n_fields, fd_cnt);
	    init = 0;
	    return (0);
	}




    }

    rcnt++;

    return;

}

/*************************************************************************
 * CONVERT_FROM_UNIX_TIME: Take the unix time in the date_time_t and
 *    calculate the separate year,month,day,hour,min,sec fields.
 *    Return values are in the struct.
 */

convert_from_unix_time(utime,yr,mon,day,hr,min,sec)
    long    utime;
    int *yr,*mon,*day,*hr,*min,*sec;
{
    long    u_day,dy,days;

    u_day = julian_date(1,1,1970);

    dy = (utime / 86400);

    calandar_date((u_day + dy),day,mon,yr);

    dy = (utime % 86400);
    *hr = dy / 3600;
    *min = (dy / 60) - (*hr * 60);
    *sec = dy % 60;


    return 0;
}
 

 
/*************************************************************************
 *    JULIAN_DATE: Calc the Julian calandar Day Number
 *    As Taken from Computer Language- Dec 1990, pg 58
 */

julian_date(day,month,year)
    int    day,month,year;
{
    int    a,b;
    double    yr_corr;

    /* correct for negative year */
    yr_corr = (year > 0? 0.0: 0.75);
    if(month <=2) {
        year--;
        month += 12;
    }
    b=0;

    /* Cope with Gregorian Calandar reform */
    if(year * 10000.0 + month * 100.0 + day >= 15821015.0) {
        a = year / 100;
        b = 2 - a + a / 4;
    }
    
    return ((365.25 * year - yr_corr) + (long) (30.6001 * (month +1)) + day + 1720994L + b);
}

/*************************************************************************
 *    CALANDAR_DATE: Calc the calandar Day from the Julian date
 *    As Taken from Computer Language- Dec 1990, pg 58
 *    Sets day,month,year as return values.
 */

calandar_date(jdate,day,month,year)
    int    jdate;
    int    *day,*month,*year;
{
    long    a,b,c,d,e,z,alpha;

    z = jdate +1;

    /* Gregorian reform correction */
    if (z < 2299161) { 
        a = z; 
    } else {
        alpha = (long) ((z - 1867216.25) / 36524.25);
        a = z + 1 + alpha - alpha / 4;
    }

    b = a + 1524;
    c = (long) ((b - 122.1) / 365.25);
    d = (long) (365.25 * c);
    e = (long) ((b - d) / 30.6001);
    *day = (int) b - d - (long) (30.6001 * e);
    *month = (int) (e < 13.5)? e - 1 : e - 13;
    *year = (int) (*month > 2.5)? (c - 4716) : c - 4715;

     return 0;
}

