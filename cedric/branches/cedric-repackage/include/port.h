#ifdef __cplusplus
 extern "C" {
#endif
#ifndef PORT_WAS_INCLUDED
#define PORT_WAS_INCLUDED
#include <dirent.h>
#include <signal.h>

/**************************************
 * PORThostname()
 *
 * Short host name - no internet detail.
 *
 * Returns pointer to static memory - do not free.
 */

extern char *PORThostname(void);

/**************************************
 * PORThostnameFull()
 *
 * Fully qualified internet host name.
 *
 * Returns pointer to static memory - do not free.
 */

extern char *PORThostnameFull(void);

/****************************
 * PORThostIpAddr()
 * 
 * Returns IP address of host.
 *
 * Returns pointer to static memory - do not free.
 */

extern char *PORThostIpAddr(void);

/****************************
 * PORTremoteIpAddr()
 * 
 * Returns IP address of remote host.
 *
 * Returns pointer to static memory - do not free.
 */

extern char *PORTremoteIpAddr(char *remote_hostname);

/****************************
 * PORThostIsLocal()
 * 
 * Checks if the hostname given is the local host. It does
 * this by comparing the IP addresses.
 *
 * Returns TRUE or FALSE
 */

extern int PORThostIsLocal(char *hostname);

/***************
 * PORTsignal()
 *
 * cover for signal; ensures signals are "reliable"
 * specifies SA_RESTART, so interrupted system calls are restarted
 * unless its SIGALRM.
 * see Stevens p298 and p 396
 */

typedef void (*PORTsigfunc)(int);
extern PORTsigfunc PORTsignal(int signo, PORTsigfunc handler);

/***************
 * PORTscandir()
 *
 * cover for scandir which is not available on SUNOS4
 */
extern int PORTscandir( const char *dir,
                        struct dirent ***namelist,
                        int (*select)(struct dirent *),
                        int (*cmp)(const struct dirent * const *,
                                   const struct dirent * const *) );

/***************
 * PORTalphasort()
 *
 * cover for alphasort which is not available on SUNOS4
 */
extern int PORTalphasort( const struct dirent * const * a,
                          const struct dirent * const * b);


#endif
#ifdef __cplusplus
}
#endif
