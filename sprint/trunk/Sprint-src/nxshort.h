#ifndef THEDATE
#define THEDATE "<03-Jul-1997 21:52:52><UTC>"
#endif
/*
 * nxshort.h -- Header file for the short version of the NX family of
 *     header files: abbreviated and even Modified.
 */

#ifndef NXSHORT_HEADER
#define NXSHORT_HEADER 1

#define NX_YEAR_CUTOFF 78  /* 78-99 for 1978-1999, 0-77 for 2000-2077 */
#define NX_MAX_RADARS   2  /* for now just TA and LF */
#define NX_MAX_FIELDS   3  /* for now just DZ, VE, SW */
#define NX_MAX_GATES 1024  /* for now limit number of gates */
#define NX_MISSING_GATE_FLAG    (-32768)

/****************************************************************************
 * used for single byte positive integers
 ***************************************************************************/
typedef unsigned char UInt, UByte;

/******************************  Date **************************************** 
 * Stores information pertinent to dates
 *****************************************************************************/
typedef struct _NxDate
{ short int year;                      /* four digit year                    */
  UInt      month;                     /* month                              */
  UInt      day;                       /* day                                */
} NxDate;

/******************************  Time  *************************************** 
 * Stores information pertinent to times
 *****************************************************************************/
typedef struct _NxTime
{ UInt      hour;                      /* hour                               */
  UInt      minute;                    /* minute                             */
  UInt      second;                    /* second                             */
  short int milliSecond;               /* milli second                       */
} NxTime;

/*
 * Stores start & ending dates & times.
 */
typedef struct _NxStartEndDT
{
  NxDate    startDate;
  NxTime    startTime;
  NxDate    endDate;
  NxTime    endTime;
} NxStartEndDT;

/*
 * Abbreviated list of NX errors.
 */
#define NX_FATAL 1
#define NX_CV_CANT_ALLOCATE_BUFFER 100
/*
 * Abbreviated list of Error codes
 */
#endif /* End of "#ifndef NXSHORT_HEADER */
/* do not add after this line */
