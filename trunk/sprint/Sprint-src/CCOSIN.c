/*
 * COS Block utilities
 * See the Cray man page "blocked(4f)"
 *
 * Written by Dick Oye (ATD)
 * Modified by Bill Anderson (MMM) to be word size and byte order independent
 */
#include <stdio.h>
#include "cedric.h"

#define  MAXREC           68000
# define FOUR_BITS        15
# define SIX_BITS         63
# define NINE_BITS        511
# define FIFTEEN_BITS     32767
# define TWENTY_BITS      1048575
# define TWENTYTHREE_BITS 8388607
# define TWENTYFOUR_BITS  16777215
# define COS_BLOCKED_EOR  8
# define COS_BLOCKED_EOF  14
# define COS_BLOCKED_EOD  15
# define SIZE_CW 8
# define MAX_WORDS 512
# define COS_BLOCK_SIZE (MAX_WORDS*SIZE_CW)
# define NWDS64(x) (((x)-1)/SIZE_CW +1)
# define NULL 0

extern FILE *fp;  /* file pointer */

struct cb_struct {
    char *bp;
    int m;			/* control word */
    int bn;			/* block number */
    int fwi;			/* forward index */
    /* the number of words to the next control word */
    int wcount;			/* block word count */
    /* fields unique to the RCW */
    int ubc;			/* unused bit count */
    /* number of unused low-order bits in last word of prev record */
    int pfi;		/* privious file index */
    /* appears to be the current block number */
    int pri;		/* previous record index */
    /* offset mod 2**15 to block where current record starts */
};

struct cb_struct *cbs=NULL;
char block[COS_BLOCK_SIZE];

/* c------------------------------------------------------------------------ */

init_cbs() {
    if( cbs == NULL ) {
	cbs = (struct cb_struct *)
	      malloc(sizeof(struct cb_struct));
    }
    memset((char *)cbs, 0, sizeof(struct cb_struct));
    cbs->bp = block;
}
/* c------------------------------------------------------------------------ */

int cos_size = 0;
#if defined (IBMRISC) || defined (HP)
     void rdcosrec(rp, ilen)
#elif defined (CRAY)
     void RDCOSREC( rp, ilen )
#else
     void rdcosrec_( rp, ilen )
#endif
     char *rp;			/* record pointer */
     int *ilen;
{
    int i, n, size_rec=0;
    char *cptmp;
    if( cbs == NULL )
	  init_cbs();
    
    cptmp = rp;
    while(1) {
	if( cbs->fwi > 0 ) {
	    n = cbs->fwi*SIZE_CW;
	    size_rec += n;
	    if (size_rec > MAXREC) {
	      printf("\n   +++COS RECORD SIZE TOO BIG+++\n");
	      exit(1);
	    }
	    memcpy( rp, cbs->bp, n );
	    rp += n;
	    cbs->bp += n;
	    cos_size += n;
	    cbs->wcount -= cbs->fwi;
	}
	if( cbs->wcount <= 0 ) { /* read next block */
	    i = fread(block, COS_BLOCK_SIZE, 1, fp);
	    if( i <= 0 ) {
	          *ilen = -COS_BLOCKED_EOD;
		  rp = cptmp;
		  return;
		}
	    cos_size = 0;
	    cbs->wcount = MAX_WORDS;
	    cbs->bp = block;
	}
	crack_RCW();

	if( cbs->m == COS_BLOCKED_EOR ) {
/*	    printf("size_rec = %d\n", size_rec);  */
	    *ilen = size_rec/8;
	    rp = cptmp;
	    return;
	}
	else if( cbs->m == COS_BLOCKED_EOF ) {
	    *ilen = -COS_BLOCKED_EOF;
	    rp = cptmp;
	    return;
	}
	else if( cbs->m == COS_BLOCKED_EOD ) {
	    *ilen = -COS_BLOCKED_EOD;
	    rp = cptmp;
	    return;
	}
	else if (cbs->m == 0) {
	}
	else {
	  printf("ERROR. unknown block type!\n");
	  exit(-1);
	}
    }
}
/* c------------------------------------------------------------------------ */

crack_BCW() 
  /*
   * Block Control Word
   * A word here is defined as 64 bits
   * this is the first word in every 512 word disk block
   */
{
    unsigned long *ul = (unsigned long *)cbs->bp;

    cbs->bn = (*ul & 1) << 23;
    cbs->bp += 4;
    ul = (unsigned long *)cbs->bp;
    cbs->bp += 4;
    cbs->bn |= (*ul >> 9) & TWENTYTHREE_BITS;
    cbs->fwi = *ul & NINE_BITS;
    cbs->wcount--;
    return;
}
/* c------------------------------------------------------------------------ */

crack_RCW() 
  /*
   * Record Control Word
   * A word here is defined as 64 bits
   * At least one RCW occurs at the end of every record
   */
{
    unsigned long ul;
    int i, it0, it1, it2, it3;

    it0 = block[cos_size];
    if (it0 < 0) it0 += 256;
    it0 = it0 << 24;

    it1 = block[cos_size + 1];
    if (it1 < 0) it1 += 256;
    it1 = it1 << 16;

    it2 = block[cos_size + 2];
    if (it2 < 0) it2 += 256;
    it2 = it2 << 8;

    it3 = block[cos_size + 3];
    if (it3 < 0) it3 += 256;

    ul = it0 | it1 | it2 | it3;
    cbs->m = (ul >> 28) & FOUR_BITS;
    cbs->ubc = (ul >> 22) & SIX_BITS;
    cbs->bp += 4;

    it0 = block[cos_size + 4];
    if (it0 < 0) it0 += 256;
    it0 = it0 << 24;

    it1 = block[cos_size + 5];
    if (it1 < 0) it1 += 256;
    it1 = it1 << 16;

    it2 = block[cos_size + 6];
    if (it2 < 0) it2 += 256;
    it2 = it2 << 8;

    it3 = block[cos_size + 7];
    if (it3 < 0) it3 += 256;

    ul = it0 | it1 | it2 | it3;

    cos_size += 8;
    cbs->bp += 4;
    cbs->fwi = ul & NINE_BITS;
    cbs->pri = (ul >> 9) & FIFTEEN_BITS;
    cbs->wcount--;
    

    if( cbs->fwi > 10 ) {
	i = 0;			/* debugging breakpoint */
    }
    return;
}
/*

*/
/* c------------------------------------------------------------------------ */

cb_put_rec( fun, fid, rp, nbytes )
  int fid, nbytes, fun;
  char *rp;			/* record pointer */
{
    int i, n, nw, n64 = (nbytes-1)/SIZE_CW +1;


    if( cbs == NULL )
	  init_cbs();
    
    cb_write(fid);

    if( fun == COS_BLOCKED_EOF ){
	form_RCW( COS_BLOCKED_EOF );
	cb_write( fid );
	return(-COS_BLOCKED_EOF);
    }
    else if( fun == COS_BLOCKED_EOD ) {
	form_RCW( COS_BLOCKED_EOD );
	cbs->wcount = MAX_WORDS;
	cb_write( fid );
	return(-COS_BLOCKED_EOD);
    }
    else if( nbytes < 1 )
	  return(nbytes);

    while( n64 > 0 ) {
	if((cbs->wcount+1+n64) <= MAX_WORDS) {
	    nw = n64;
	}
	else {			/* record crosses a block boundary */
	    nw = MAX_WORDS - cbs->wcount;
	    cbs->pri++;
	}
	cb_put_fwi(nw);
	memcpy(cbs->bp, rp, nw*SIZE_CW );
	cbs->bp += nw*SIZE_CW;
	rp += nw*SIZE_CW;
	n64 -= nw;
	cbs->wcount += nw;
	cb_write( fid );
    }
    if( i = nbytes & 7 )	/* not an even multiple of 8 */
	  cbs->ubc = (8-i)*8;	/* unfilled bit count */

    if( nbytes > 80 ) {
	i = 0;			/* debugging breakpoint */
    }
    form_RCW( COS_BLOCKED_EOR );
    cb_write( fid );

    return(nbytes);
}
/* c------------------------------------------------------------------------ */

form_BCW()			/* Block Control Word! */
{
    unsigned long *ul = (unsigned long *)cbs->bp;

    *ul = (cbs->bn >> 23 ) & 1;
    cbs->bp += 4;
    ul = (unsigned long *)cbs->bp;
    cbs->bp += 4;
    *ul = (cbs->bn & TWENTYTHREE_BITS ) << 9;
    cbs->wcount++;
    return;
}
/* c------------------------------------------------------------------------ */

form_RCW(m)			/* Record Control Word! */
  int m;
{
    unsigned long *ul = (unsigned long *)cbs->bp, *keep;

    cbs->bp += 4;
    *ul = (m & FOUR_BITS ) << 28;
    *ul |= (cbs->ubc & SIX_BITS ) << 22;
    cbs->ubc = 0;
    keep = ul;
    ul = (unsigned long *)cbs->bp;
    cbs->bp += 4;
    *ul = (cbs->pri & FIFTEEN_BITS ) << 9;
    cbs->pri = 0;
    *ul |= (cbs->bn & TWENTY_BITS ) << (9+15); 
    /* undocumented but it seems to be the block count here */
    cbs->wcount++;
    return;
}
/* c------------------------------------------------------------------------ */

cb_write( fid )
  int fid;
{
    int i=0;

    if( cbs->wcount == MAX_WORDS ) {
	i = write( fid, block, COS_BLOCK_SIZE );
	cbs->bn++;
	cbs->wcount = 0;
	cbs->bp = block;
    }
    if( cbs->wcount == 0 ) form_BCW();
    return(i);
}

/* c------------------------------------------------------------------------ */

cb_put_fwi(fwi)
  int fwi;
{
    /* assume last word is to be updated */
    unsigned long *ul = (unsigned long *)(cbs->bp-4);
    *ul |= fwi & NINE_BITS;
}
/* c------------------------------------------------------------------------ */

fb_read( fin, buf, count )
  int fin, count;
  char *buf;
{
    long int size_rec=0, rlen1, rlen2=0;

    /* read the record length */
    rlen1 = read (fin, &size_rec, sizeof(size_rec));
    if( rlen1 < sizeof(size_rec))
	  return(rlen1);

    if( size_rec > 0 ) {
	/* read the record */
	rlen2 = size_rec <= count ? size_rec : count;
	rlen1 = read (fin, buf, rlen2);
	if( rlen1 < 1 )
	      return(rlen1);
	rlen2 = rlen1 < size_rec ?
	      size_rec-rlen1 : 0;
    }
    else
	  rlen1 = 0;
    
    rlen2 += sizeof(size_rec);

    /* skip thru the end of record */
    rlen2 = lseek( fin, rlen2, 1 );
    if( rlen2 < sizeof(size_rec))
	  return(rlen2);
    else
	  return(rlen1);
}
/* c------------------------------------------------------------------------ */

fb_write( fout, buf, count )
  int fout, count;
  char *buf;
{
    long int size_rec=count, rlen1, rlen2=0;

    /* write the record length */
    rlen1 = write (fout, &size_rec, sizeof(size_rec));
    if( rlen1 < sizeof(size_rec))
	  return(rlen1);

    if( size_rec > 0 ) {
	/* write the record */
	rlen1 = write (fout, buf, size_rec);
	if( rlen1 < 1 )
	      return(rlen1);
    }
    /* write the record length */
    rlen2 = write (fout, &size_rec, sizeof(size_rec));
    if( rlen2 < sizeof(size_rec))
	  return(rlen2);
    else
	  return(rlen1);
}
/* c------------------------------------------------------------------------ */
/* c------------------------------------------------------------------------ */
/* c------------------------------------------------------------------------ */
/* c------------------------------------------------------------------------ */


