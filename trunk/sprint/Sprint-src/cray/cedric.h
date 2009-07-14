/* header file for CEDRIC on the CRAYs */

#define FALSE 0
#define TRUE  1
#define BYTE_ORDER 0

#define WORD_SIZE 64
#define CED "CED1"
#define FIRST_VOL 1540
#define MAXVOL    25 
#define EDIT_NAME ".cededit"
#define REMAP_NAME ".cedremap"

struct files {
  int unit;
  FILE *fps;
  int curr_vol;
  short swap;
  struct files *next;
} ;

#define EDIT_SIZE 16   /* size of edit volume divided by 1e+06 */
