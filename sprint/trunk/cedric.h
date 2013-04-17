/* header file for CEDRIC (Linux) */
/* see also op_sys.h operating system header file */
/* change OP_SYS to be IBMRISC, HP, CRAY, or LINUX */
/* change BYTE_ORDER to 0 (Sun-like, big Endian) */
/*                   or 1 (Dec-like, little Endian) */
/* change WORD_SIZE to be 32 or 64 */

#define FALSE 0
#define TRUE  1
#define BYTE_ORDER 1

#define WORD_SIZE 32
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
