#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#include "cedric.h"

/* the following function uses standard C library calls to
 * return the date and time in a format useful to CEDRIC
 */
#if defined (IBMRISC) || defined (HP)
void cdate(buffer)
#elif defined (CRAY)
void CDATE(buffer)
#else
void cdate_(buffer)
#endif
char buffer[15];
{
	struct tm *tmbuf;
	time_t clock;
	time(&clock);
	strftime(buffer,15,"%Y%m%d%H%M%S",localtime(&clock));
	return;
}
