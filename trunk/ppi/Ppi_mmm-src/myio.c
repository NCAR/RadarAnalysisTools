#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

int RegularFile = 0;
int CurrentPtrLoc = 0;
int CurrentNbytesInBuf = 0;

/* open file and determine if it's a regular file or a tape device */

int myopen(filename)
     char *filename;
{
  int rval, fd;
  struct stat buffer;
  

  RegularFile = 0;

  fd = open(filename, O_RDONLY);
  /*printf("+++ In myopen: opening file: %s fd= %d \n", filename,fd);*/
  /* see if it's a normal file or a tape device */

  rval = stat(filename, &buffer);
  if (rval != 0) {
    printf("+++error locating file: %s+++\n", filename);
    fd = -1;
  }
  else {
    if ((buffer.st_mode & (0170000)) == S_IFREG) {
      RegularFile = 1;
    }
    else {
      RegularFile = 0;
    }
  }

  CurrentPtrLoc = 0;
  CurrentNbytesInBuf = 0;
  return fd;

}

#define MXBUF 65536

/* read in requested number of bytes from requested file descriptor;
 * this can be a regular file or a tape device
 */
int myread(array, nbytes, fd)
     char array[];
     int nbytes;
     int fd;
{
  static char buf[MXBUF];     /* buffer to hold tape records */
  int rval;
  int BytesTransferred;
  void read_record();

  if (RegularFile == 1) { /* do a normal read */
    rval = read(fd, array, nbytes);
    if (rval < 0) {
      printf("+++error reading from file+++\n");
      exit(1);
    }
  }
  else { /* tape read */
    if (nbytes > CurrentNbytesInBuf) {
      memcpy(array, &(buf[CurrentPtrLoc]), CurrentNbytesInBuf);
      BytesTransferred = CurrentNbytesInBuf;
      CurrentPtrLoc = 0;
      read_record(buf, &CurrentNbytesInBuf, fd);
      if (CurrentNbytesInBuf != 0)  {
	rval = myread(&(array[BytesTransferred]), 
		      (nbytes - BytesTransferred), fd) ;
	if (rval != -1) rval = rval + BytesTransferred;
      }
      else {
	rval = -1; /* error reading; set return value to indicate that */
      }
    }
    else {
      memcpy(array, &(buf[CurrentPtrLoc]), nbytes);
      CurrentNbytesInBuf -= nbytes;
      CurrentPtrLoc += nbytes;
      rval = nbytes;
    }
  }

  return rval;

}

/* do actual system call to read a tape record */

void read_record(buf, nbytes, fd)
     char *buf;
     int *nbytes;
     int fd;
{

  *nbytes = read(fd, buf, MXBUF);
  if (*nbytes < 0) {
    printf("error reading from file\n");
    exit(1);
  }

  return;

}
/************************************************************/
#if defined (IBMRISC) || defined (HP)
  void c_atoi(char tape_unit[8],int *iun)
#elif defined (CRAY)
  void C_ATOI(char tape_unit[8],int *iun)
#elif defined (linux)
  void c_atoi__(char tape_unit[8],int *iun)
#else
  void c_atoi_(char tape_unit[8],int *iun)
#endif
{

   *iun = atoi(tape_unit);
}
