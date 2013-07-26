/* header file for CEDRIC on LINUX */
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <endian.h>

#define LINUX
#define FALSE 0
#define TRUE  1

/* ENDIAN = 0 (big Endian), 1 (little Endian)*/
#if __BYTE_ORDER == __LITTLE_ENDIAN
# define ENDIAN 1
#elif __BYTE_ORDER == __BIG_ENDIAN
# define ENDIAN 0
#else
#error "Unknown endian-ness."
#endif

#define WORD_SIZE 32
#define CED "CED1"
#define FIRST_VOL 1540
#define MAXVOL    25
#define MAXCDF    14
#define NFMAX     25
#define MAXZLEV   128
#define MXCRT     20
#define MAXPOINTS 3932160
/* what is MAXPOINTS used for?*/

#define EDIT_NAME ".cededit"
#define REMAP_NAME ".cedremap"
#define MAXX      512
#define MAXY      512
#define MAX_LAND  15
#define MAXPLN    262144 /*512 * 512*/

/*Data formats that Cedric can read.*/
#define CEDPURE   1   /*PURE CEDRIC BINARY FILE*/
#define CDFFMT    2   /*A NETCDF FILE*/
#define USWRPGD   3   /*A USWRP GRIDDED DATA FILE*/
#define MDVFMT    4   /*A MDV DATA FILE*/
#define OLDCEDRIC 5

/*The number of Integer*2's in the Cedric header*/
#define NID     510   /*The size of the Cedric header*/

/*
 *coordinate system definitions for C routines called from REMAP and the
 *MDV and CDF write routines.
 */
#define XYZ 0   /*CARTESIAN*/
#define XYC 1   /*COPLANE*/
#define XYE 2   /*ELEVATION*/
#define LLE 3   /*LON LAT ELEVATION*/
#define LLZ 4   /*LON LAT Z*/
#define PPI 5
#define RHI 6
#define LLP 7   /*LON LAT PRESSURE*/

/*
 *Cedric action types.
 */
#define READF      0
#define TRANSFERF  1
#define REMANEF    2
#define DELETEF    3

/*
 * Define a macro to generate names which can be linked from FORTRAN.  For
 * now assume a single underscore until such time as this needs to be more
 * complicated.
 */
#define FORTRAN_NAME(name) name##_

struct files {
  int unit;
  FILE *fps;
  int curr_vol;
  short swap;
  struct files *next;
} ;

