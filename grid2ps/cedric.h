/* header file for CEDRIC on the SUN */

#define LINUX
#define FALSE 0
#define TRUE  1
#define BYTE_ORDER 1
#define ENDIAN 1

#define WORD_SIZE 32
#define CED "CED1"
#define FIRST_VOL 1540
#define MAXVOL    25
#define EDIT_NAME ".cededit"
#define REMAP_NAME ".cedremap"
#define MAXCDF 14

struct files {
  int unit;
  FILE *fps;
  int curr_vol;
  short swap;
  struct files *next;
} ;

#define EDIT_SIZE 16   /* size of edit volume divided by 1e+06 */
