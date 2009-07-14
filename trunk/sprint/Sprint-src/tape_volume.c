/* 	$Id: to_fb.c,v 1.5 1998/05/21 15:24:49 oye Exp $	 */

#ifndef lint
static char vcid[] = "$Id: to_fb.c,v 1.5 1998/05/21 15:24:49 oye Exp $";
#endif /* lint */
# ifdef obsolete
# endif

# include <stdlib.h>
# include <string.h>
# include <sys/types.h>
# include <sys/mtio.h>
# include <sys/ioctl.h>
/*
 * stand alone program to copy a tape file
 * to a FORTRAN-BINARY file
 */
# define PMODE 0644
# define BLOCK_SIZE 65536
# ifndef FB_IO
# define FB_IO 1
# endif

static int LittleEndian = 1;

/* c------------------------------------------------------------------------ */

main(argc, argv)
  int argc;
  char *argv[];
{
    char *inputdev = NULL, *outputdev = NULL;
    char buf[BLOCK_SIZE];
    int * iptr;
    int i, f, m, n, mark, in, out, num_files=999999, num_recs=0, r;
    int io_type=0, out_io_type=0, num_filesx = 0;
    int eof_count = 0, null_dev = 0, dd_output = 0;
    int eof_rec_count = 0, eof_interval = 0, forced_eof_count = 0;
    long recl;
    double size = 0, tsize = 0;
    struct mtop op;
    char * null_name = "/dev/null", *aa, *source_dev, *out_dev;

    buf[0] = 0;
    iptr = (int *)buf;
    *iptr = 1;
    LittleEndian = buf[0] == 1 ? 1 : 0;

    if( getenv( "SOURCE_DEV" ) && getenv( "DEST_DEV" ) ) {
       inputdev = getenv( "SOURCE_DEV" );
       outputdev = getenv( "DEST_DEV" );
    }
    else if(argc < 2 ) {
       printf("Usage: %s inputdev outputdev [file_count [rec_count]]\n"
	      , argv[0]);
       exit(1);
    }
    else {
       inputdev = argv[1];
       outputdev = argv[2];
    }
    
    dd_output = (aa = getenv( "DD_OUTPUT")) ? 1 : 0;
    if( dd_output )
      { out_io_type = 0; }
		 
    printf("argc: %d ", argc);
    for(i=0; i < argc; i++ )
      printf(" %s ", argv[i]);
    printf("\n");
    
    if( aa = getenv( "FORCED_EOFS" ) ) {
       n = atoi( aa );
       if( n > 0 )
	 { eof_interval = n; }
       printf( "Forced EOFS every %d records\n", eof_interval );
    }

    if( aa = getenv( "NUM_FILES" ) ) {
       n = atoi( aa );
       if( n > 0 )
	 { num_filesx = n; }
    }
    else if(argc > 3)
	   { num_filesx = atoi(argv[3]); }

    if( aa = getenv( "NUM_RECS" ) ) {
       n = atoi( aa );
       if( n > 0 )
	 { num_recs = n; }
    }
    else if(argc > 4)
	  { num_recs = atoi(argv[4]); }

    /*
    inputdev = "/dev/rmt/0b";
    outputdev = "/dev/rmt/2b";
    num_files = 4;
     */

    if( num_filesx == -1 )
	{ dd_output = 1; }
    else if( num_filesx > 0 )
	{ num_files = num_filesx; }

    if(!strstr(inputdev, "/dev/"))
	  io_type = FB_IO;
    if(!strstr(outputdev, "/dev/"))
	  out_io_type = FB_IO;


    if( dd_output )
      { out_io_type = 0; }
		 
    null_dev = strstr(outputdev, null_name) ? 1 : 0;

    if(num_recs > 0 && io_type == FB_IO) {
	printf("rec_count > 0 does not work for fortran-binary i/o\n");
	exit(1);
    }

    /* open the input file */
    printf("Opening: %s \n", inputdev);
    if((in=open(inputdev,0)) < 0 ) {
	printf("Unable to open: %s state: %d\n", inputdev, in);
	exit(1);
    }
    /* open the output file */
    printf("Opening: %s \n", outputdev);
    if((out=creat(outputdev,PMODE)) < 0 ) {
	printf("Unable to open: %s state: %d\n", outputdev, in);
	exit(1);
    }

    printf("Copying ");
    if(num_recs > 0 )
	  printf( "%d recs from each of ", num_recs);
    printf("%d file(s)\n", num_files );
    /*
     * loop through the input
     */
    for(f=0; f < num_files; f++ ) {
	r = 0;
	size = 0;
	while(1) {
	    if(io_type == FB_IO) {
		m = fb_read(in,buf,BLOCK_SIZE);
	    }
	    else {
		m = read(in,buf,BLOCK_SIZE);
	    }
	    if(m == 0)
		{ ++eof_count; }
	    if( m < 0 )
	      { mark = 0; }
	    if(m <= 0)
	      { break; }
	    r++;
	    eof_count = 0;
# ifdef obsolete
	    printf("Rec %3d\n", r );
# endif
	    if( null_dev ) {
	       mark = 0;
	    }
	    else if(out_io_type == FB_IO) {
		n = fb_write(out,buf,m);
	    }
	    else {
		n = write(out,buf,m);
		if( eof_interval && !(++eof_rec_count % eof_interval )) {
		    printf( "Forced EOF at f: %d rec: %d  %.3f MB\n"
			    , ++forced_eof_count, eof_rec_count
			    , size * 1.e-6 );
		    op.mt_op = MTWEOF;
		    op.mt_count = 1;
		    if (ioctl (out, MTIOCTOP, &op) < 0)
			{
			    perror ("to_fb:Mag tape WEOF");
			}
		}
	    }
	    size += (float)m;
	    if(num_recs > 0) 
		  if(r >= num_recs) {
		      op.mt_op = MTFSF;
		      op.mt_count = 1;
		      printf("Skip to next EOF\n");
		      if (ioctl (in, MTIOCTOP, &op) < 0 )
			    perror("to_fb:mtskpf");
		      break;
		  }
	}
	if( null_dev || dd_output ) { /* unix utility "dd" for copying tapes */
	   mark = 0;
	}
	else if(out_io_type == FB_IO) {
	   n = fb_write(out,buf,(long)0); /* eof on disk */
	}
	else {
	   op.mt_op = MTWEOF;
	   op.mt_count = 1;
	   if (ioctl (out, MTIOCTOP, &op) < 0)
	     {
		perror ("to_fb:Mag tape WEOF");
	     }
	}
	tsize += size;
	printf("at %9.3f MB   Copied ", tsize * 1.e-6 );
	printf("%7.3f MB or ", size*1.e-6);
	printf("%4d records from ", r);
	printf("file: %2d\n", f+1);
	if(eof_count > 2)
	    { break; }
	if(m < 0)
	      break;
    }
}
/* c------------------------------------------------------------------------ */

void swack4(ss, tt)
  char *ss, *tt;
{
   *tt++ = *(ss+3);
   *tt++ = *(ss+2);
   *tt++ = *(ss+1);
   *tt = *ss;
}
/* c------------------------------------------------------------------------ */

int fb_read( fin, buf, count )
  int fin, count;
  char *buf;
{
    /* fortran-binary read routine
     */
    long int size_rec=0, rlen1, rlen2=0, nab;

    /* read the record size */
    rlen1 = read (fin, &nab, sizeof(nab));

    if( rlen1 < sizeof(nab))
	  return(rlen1);

    if(LittleEndian) {
       swack4(&nab, &size_rec);
    }
    else {
       size_rec = nab;
    }

    if( size_rec > 0 ) {
	/* read the record
	 * (the read may be less than the size of the record)
	 */
	rlen2 = size_rec <= count ? size_rec : count;

	rlen1 = read (fin, buf, rlen2);	/* read it! */
	if( rlen1 < 1 )
	      return(rlen1);
	rlen2 = rlen1 < size_rec ?
	      size_rec-rlen1 : 0; /* set up skip to end of record */
    }
    else
	  rlen1 = 0;
    
    rlen2 += sizeof(size_rec);

    /* skip thru the end of record */
    rlen2 = lseek( fin, rlen2, 1 );
    return(rlen1);
}
/* c------------------------------------------------------------------------ */

int fb_write( fout, buf, count )
  int fout, count;
  char *buf;
{
    /* fortran-binary write routine
     */
    long int size_rec=count, rlen1, rlen2=0, blip;

    if(LittleEndian) {
       swack4(&size_rec, &blip);
    }
    else {
       blip = size_rec;
    }
    /* write the record length */
    rlen1 = write(fout, &blip, sizeof(blip));
    if( rlen1 < sizeof(size_rec))
	  return(rlen1);

    if( size_rec > 0 ) {
	/* write the record */
	rlen1 = write (fout, buf, size_rec);
	if( rlen1 < 1 )
	      return(rlen1);
    }
    /* write the record length */
    rlen2 = write (fout, &blip, sizeof(blip));
    if( rlen2 < sizeof(blip))
	  return(rlen2);

    else if(size_rec == 0)
	  return(0);
    else
	  return(rlen1);
}
/* c------------------------------------------------------------------------ */
