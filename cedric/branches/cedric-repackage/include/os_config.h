/******************************************************************
 * os_config.h
 *
 * Operating system configuration header
 *
 * There is one section for each operating system.
 *
 * The following are currently supported:
 *
 *   AIX            - IBM RISC
 *   DECOSF1        - DEC Alpha
 *   HPUX           - HP
 *   IRIX4          - SGI IRIX Version 4
 *   IRIX5          - SGI IRIX Version 5
 *   IRIX6          - SGI IRIX Version 6
 *   LINUX
 *   SUNOS4         - SUN Berkeley Version 4 (Solaris 1)
 *   SUNOS5         - SUN SYS5 (Solaris 2)
 *   SUNOS5_64      - SUN SYS5 (Solaris 2) - 64-bit - ULTRA
 *   ULTRIX_DEC3100 - DEC ULTRIX
 */

#ifndef os_config_h
#define os_config_h

#ifdef __cplusplus
extern "C" {
#endif

#define MAX_PATH_LEN 1024
#define MAX_HOST_LEN 256

/*******************************************************************/

#ifdef AIX

#define PATH_DELIM "/"

/*
 * pointer for fd_set calls
 */

#define FD_SET_P (fd_set *)

#include <sys/select.h>
#include <sys/time.h>
#include <sys/fcntl.h>  /* declares open() */

typedef struct timeval timeval_t;

extern int setitimer(int, struct itimerval *, struct itimerval *);

#endif /* AIX */

/*******************************************************************/

#ifdef DECOSF1

#define PATH_DELIM "/"

#define HAVE_VFORK		/* support the vfork() system call */

/*
 * pointer for fd_set calls
 */

#define FD_SET_P (fd_set *)

#include <sys/types.h>
#include <sys/time.h>
#include <sys/select.h>

typedef struct timeval timeval_t;

union semun {
  int val;
  struct semid_ds *buf;
  unsigned short *array;
};

#endif /* DECOSF1 */

/*******************************************************************/

#ifdef HPUX

#define PATH_DELIM "/"

#define HAVE_VFORK		/* support the vfork() system call */

/*
 * pointer for fd_set calls
 */

#define FD_SET_P (int *)

#include <sys/time.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/ipc.h>

typedef struct timeval timeval_t;

extern int setitimer(int, struct itimerval *, struct itimerval *);

union semun {
  int		val;		/* value for SETVAL */
  struct semid_ds	*buf;	/* buffer for IPC_STAT & IPC_SET */
  unsigned short	*array;	/* array for GETALL & SETALL */
};

#endif /* HPUX */

/*******************************************************************/

#ifdef IRIX4

#define PATH_DELIM "/"

#include <sys/types.h>
#include <sys/time.h>

/*
 * pointer for fd_set calls
 */

#define FD_SET_P (fd_set *)

typedef struct timeval timeval_t;

extern int select(int width,
		  fd_set *rdset, fd_set *wrset, fd_set *exset,
		  struct timeval *timeout);

extern int kill(pid_t, int);

extern int syslog( int priority, char *message, ...);

extern int gethostname(char *name, int namelen);

#ifndef bzero
extern void bzero(void *d, size_t len);        /* needed for FD_ZERO ! */
#endif

#endif /* IRIX4 */

/*******************************************************************/

#ifdef IRIX5

#define PATH_DELIM "/"

/* BSD types */

#define _BSD_TYPES
#include <sys/bsd_types.h>

#include <sys/types.h>
#include <sys/time.h>
#include <sys/select.h>

/*
 * pointer for fd_set calls
 */

#define FD_SET_P (fd_set *)

typedef struct timeval timeval_t;

extern int select(int width,
		  fd_set *rdset, fd_set *wrset, fd_set *exset,
		  struct timeval *timeout);

extern int setitimer(int, struct itimerval *, struct itimerval *);

extern int kill(pid_t, int);

#ifndef bzero
extern void bzero(void *d, int len);        /* needed for FD_ZERO ! */
#endif

#endif /* IRIX5 */

/*******************************************************************/
#ifdef IRIX6

#define PATH_DELIM "/"

/* BDS types */

#define _BSD_TYPES
#include <sys/bsd_types.h>

#include <sys/types.h>
#include <sys/time.h>
#include <sys/select.h>

/*
 * pointer for fd_set calls
 */

#define FD_SET_P (fd_set *)

typedef struct timeval timeval_t;

extern int select(int width,
		  fd_set *rdset, fd_set *wrset, fd_set *exset,
		  struct timeval *timeout);

extern int setitimer(int, struct itimerval *, struct itimerval *);

extern int kill(pid_t, int);

#endif /* IRIX6 */

/*******************************************************************/

#ifdef LINUX

#define PATH_DELIM "/"

  /*#include <linux/ipc.h>*/
#include <sys/time.h>

typedef struct timeval timeval_t;

  /*extern int setitimer(int, const struct itimerval *, struct itimerval *);*/

/*
 * pointer for fd_set calls
 */

#define FD_SET_P (fd_set *)

#endif /* LINUX */

/*******************************************************************/

#ifdef SUNOS4			/* Sunos 4.x, Solaris 1.x */

#define PATH_DELIM "/"

#ifdef LINT
#define sun
#endif

#define HAVE_VFORK		/* support the vfork() system call */

/*
 * pointer for fd_set calls
 */

#define FD_SET_P (fd_set *)

#include <stdio.h>
#include <floatingpoint.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>

typedef struct timeval timeval_t;

extern int printf(const char *format, ...);
extern int fprintf(FILE *stream, const char *format, ...);
extern int fflush(FILE *stream);
extern int fseek(FILE *stream, long offset, int whence);
extern size_t fread(void *ptr, size_t size, size_t nitems, FILE *stream);
extern size_t fwrite(const void *ptr, size_t size,
		     size_t nitems, FILE *stream);
extern int fclose(FILE *stream);
extern int sscanf(const char *s, const char *format, ...);

extern void perror(const char *s);

extern time_t time(time_t *tloc);

extern int system(const char *string);

extern int select(int width,
		  fd_set *rdset, fd_set *wrset, fd_set *exset,
		  timeval_t *timeout);

int semctl(int, int, int, ...);
int semget(key_t, int, int);
int semop(int, struct sembuf *, unsigned);

int shmctl(int, int, ...);
int shmget(key_t, int, int);
void *shmat(int, void *, int);
int shmdt(void *);

extern int setitimer (int which,
		      struct itimerval *value,
		      struct itimerval *ovalue);

extern int ftruncate(int fd, off_t length);

extern int wait3(union wait *statusp, int options,
		 struct rusage *rusage);

extern int getopt (int argc, char * const argv[], const char *optstring);

extern int openlog(char * ident, int logopt, int facility);
extern int syslog( int priority, char *message, ...);

extern int gethostname(char *name, int namelen);
extern int gettimeofday(struct timeval *tp,struct timezone *tzp);

#ifndef bzero
extern void bzero(void *d, size_t len);        /* needed for FD_ZERO ! */
#endif

#endif /* SUNOS4 */


/*******************************************************************/

#ifdef SUNOS5

#define PATH_DELIM "/"

#define HAVE_VFORK		/* support the vfork() system call */

/*
 * pointer for fd_set calls
 */

#define FD_SET_P (fd_set *)

#include <fcntl.h>  /* declares open() */
#include <sys/types.h>
#include <sys/time.h>
#include <sys/select.h>
#include <sys/stat.h>

typedef struct timeval timeval_t;

union semun {
  int val;
  struct semid_ds *buf;
  unsigned short *array;
};

#ifndef __cplusplus
extern int gettimeofday(struct timeval *tp,void *unused);
#endif

extern int gethostname(char *name, int namelen);

#include <signal.h>

extern int sigblock(int mask);
extern int sigmask(int signum);
extern int sigpause(int mask);
extern int sigsetmask(int mask);

#endif /* SUNOS5 */

/*******************************************************************/

#ifdef SUNOS5_64

#define PATH_DELIM "/"

#define HAVE_VFORK		/* support the vfork() system call */

#include <fcntl.h>
#include <sys/stat.h>

/*
 * pointer for fd_set calls
 */

#define FD_SET_P (fd_set *)

#include <fcntl.h>  /* declares open() */
#include <sys/types.h>
#include <sys/time.h>
#include <sys/select.h>

typedef struct timeval timeval_t;

union semun {
  int val;
  struct semid_ds *buf;
  unsigned short *array;
};

extern int gethostname(char *name, int namelen);

#include <strings.h>      /* declares bzero */

#include <signal.h>

extern int sigblock(int mask);
extern int sigmask(int signum);
extern int sigpause(int mask);
extern int sigsetmask(int mask);

#endif /* SUNOS5_64 */

/*******************************************************************/

#ifdef ULTRIX_DEC3100

#define PATH_DELIM "/"

/*
 * pointer for fd_set calls
 */

#define FD_SET_P (fd_set *)

/*
 * byte swapping - some machines have an internal byte representation
 * different from the XDR format - if so, define XDR_SWAP
 */

#define XDR_SWAP

#endif /* ULTRIX_DEC3100 */

#ifdef __cplusplus
}
#endif

#endif /* os_config_h */


