/* Header file borrowed from CEDRIC in order to satisfy PPI_MMM includes     */
/* Commented out defines which are now in op_sys.h and the other defines     */
/* don't matter for PPI_MMM C routines, but the the struct files may matter. */

/* #define FALSE 0 */
/* #define TRUE  1 */
/* #define BYTE_ORDER 1 */
/* #define WORD_SIZE 32 */

#define CED "CED1"
#define FIRST_VOL 1540
#define MAXVOL    25
#define MAXCDF    14
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
