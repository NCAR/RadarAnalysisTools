/* header file for CEDRIC on the Cray YMP */

#include <stdio.h>

#define ENDIAN 0
#define WORD_SIZE 64
#define CRAY

#define FALSE 0
#define TRUE  1
#define CED "CED1"
#define EDIT_NAME ".cededit"
#define REMAP_NAME ".cedremap"

#define FIRST_VOL 1540
#define MAXVOL    25
#define MAXCOS    25     /* maximum # of cos units that can be open */
#define MAXCDF    14
#define NFMAX     25
#define MAXZLEV   128
#define MAXPOINTS 3932160
#define MAXX      512
#define MAXY      512
#define MAX_LAND  15

struct files {
  int unit;
  FILE *fps;
  int curr_vol;
  short swap;
  struct files *next;
} ;
