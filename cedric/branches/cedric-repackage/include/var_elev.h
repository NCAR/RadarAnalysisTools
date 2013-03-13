/*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*/
/*
 * RCS info
 *  $Author: dave $
 *  $Locker:  $
 *  $Date: 1998/06/16 23:13:26 $
 *  $Id: var_elev.h,v 1.2 1998/06/16 23:13:26 dave Exp $
 *  $Revision: 1.2 $
 *  $State: Exp $
 *
 *  $Log: var_elev.h,v $
 *  Revision 1.2  1998/06/16 23:13:26  dave
 *  Removed #include <copyright.h>
 *
 * Revision 1.1  1998/06/12  20:15:57  dave
 * Added variable elevation chunk data and a variable elevation vertical
 * level type
 *
 */
/**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**/


/************************************************************************

Header: var_elev.h

Author: Dave ALbo

Date:	Thu Jun 11 17:37:47 1998

Description:	Header file for variable elevation chunk data

*************************************************************************/

# ifndef    VAR_ELEV_H
# define    VAR_ELEV_H


/* System include files / Local include files */
#include <stdio.h>

/* Constant definitions / Macro definitions / Type definitions */

#define VAR_ELEV_BAD_ELEV -99.99


/* External global variables / Non-static global variables / Static globals */


/* External functions / Internal global functions / Internal static functions */
/*
 * Print the var_elev stuff passed in as an MDV chunk
 */
extern void VAR_ELEV_print(FILE *outfile, void *data, int size);

/*
 * convert input var_elev data from BE into local format.
 */
extern void VAR_ELEV_variable_elev_from_BE(void *data, int size);

/*
 * convert input var_elev data from local into BE.
 */
extern void VAR_ELEV_variable_elev_to_BE(void *data, int size);

/*
 * Return var_elev record size in bytes associated with input nazimuth
 */
extern int VAR_ELEV_record_size(int nazimuth);

/*
 * Build and return the var_elev data associated with the inputs
 * Return the length of the data in len
 *
 * The user must free the space by calling VAR_ELEV_destroy
 */
extern void *VAR_ELEV_build(float *elevations, int nazimuth, int *len);

/*
 * Free chunk data allocated by call to VAR_ELEV_build
 */
extern void VAR_ELEV_destroy(void **var_elev);

/*
 * Return the number of elevations in the input var_elev data
 */
extern int VAR_ELEV_num_elevations(void *var_elev);

/*
 * Return the elevation associated with the input azimuth index
 * as found in the var_elev data passed in.
 */
extern float VAR_ELEV_get_elevation(void *var_elev, int index);

# endif     /* VAR_ELEV_H */
