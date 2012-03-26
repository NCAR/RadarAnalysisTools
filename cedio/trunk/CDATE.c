#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#include "cedric.h"

/* the following function uses standard C library calls to
 * return the date and time in a format useful to CEDRIC
 */
#if defined (IBMRISC) || defined (HP)
void cdate_(buffer)
#elif defined (CRAY)
void CDATE(buffer)
#else
void cdate_(buffer)
#endif
char buffer[15];
{
	struct tm *tmbuf;
	time_t clock;
	time(&clock);
	strftime(buffer,15,"%Y%m%d%H%M%S",localtime(&clock));
	return;
}

/*******************************************************************************/
#if defined (IBMRISC) || defined (HP)
void convert_to_degminsec_(short *deg,short *min,short *sec,double *var)
#elif defined (CRAY)
CONVERT_TO_DEGMINSEC(short *deg,short *min,short *sec,double *var)
#else
void convert_to_degminsec_(short *deg,short *min,short *sec,double *var)
#endif
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
  *sec = (short)seconds;
}

 /********************************************************************/
int check_leap(int year)
{

  if(year % 4 == 0 && year % 100 != 0 || year % 400 == 0)
      return(1);
  else
      return(0);
}/*end check_leap*/     

/****************************************************************/
void julday_(juldat, iyr, imon, iday)
     int *juldat, *iyr;
     int *imon, *iday;
{
  int idymon[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
  int sum, i;
 
  if ((*juldat > 365 && ((*iyr%4) != 0)) || *juldat > 366) {
    printf("error in julian date:%d\n", *juldat);
    exit(1);
  }
  /* check for leap year */
  if ((*iyr%4) == 0) idymon[1] = 29;
  
  sum = 0;
  
  for (i = 0; i < 12; i++) {
    sum = sum + idymon[i];
    if (sum >= *juldat) break;
  }
 
  *imon = i + 1;
  *iday = *juldat - (sum - idymon[i]);

  return;
}  
/********************************************************************/
int cyear()
{

  char year[5];
  struct tm *ptr;
  time_t lt;
  size_t the_size;
  int thisyear;

  lt = time(NULL);
  ptr = localtime(&lt);
  the_size = 5;   
  strftime(year,the_size,"%Y",ptr);
  thisyear = atoi(year);
  return(thisyear);
}/*cyear*/
int date_to_seconds(int tarray[9])
{


   int hoursecs = 60 * 60;
   int daysecs = 24 * hoursecs;
   int yearsecs = 365 * daysecs;
   int month_days[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
   int total,index,i,days;
 

   if(tarray[2] < 1970) return -1;

  /*
   *Calculate the year seconds since 1970.
   */
   total = (tarray[2] - 1990) * yearsecs;

  /*
   *Now add the day seconds for years since 1970 excluding this year that are 
   *leap years.
   */
   for(index = 1970; index < tarray[2]; index++)
        if(check_leap(index) == 1) total += daysecs;


   i = check_leap(tarray[2]);
   for(index = 1; index < tarray[1]; index++){
       if(i == 1 && index == 2) 
           days = 29;
       else
          days = month_days[index - 1];

       total += days * daysecs;
   }
       

   total += (tarray[0] - 1) * daysecs;
   total += tarray[3] * hoursecs;
   total += tarray[4] * 60;
   total += tarray[5];
   return(total);
}
  
  
