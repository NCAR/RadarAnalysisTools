/* *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* */
/* ** Copyright UCAR (c) 1992 - 2001 */
/* ** University Corporation for Atmospheric Research(UCAR) */
/* ** National Center for Atmospheric Research(NCAR) */
/* ** Research Applications Program(RAP) */
/* ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA */
/* ** 2001/11/19 23:15:6 */
/* *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* */
#ifdef __cplusplus
 extern "C" {
#endif
#ifndef ANSI_WAS_INCLUDED
#define ANSI_WAS_INCLUDED

#ifdef SUNOS4

#include <sys/types.h>

/* ANSI - missing ansi routines. */

extern void *memmove (void *s1, const void *s2, size_t n);
extern unsigned long strtoul (const char *str, char **ptr, int base);
extern int atexit (void (*func)(void));

#endif

#endif
#ifdef __cplusplus
}
#endif


