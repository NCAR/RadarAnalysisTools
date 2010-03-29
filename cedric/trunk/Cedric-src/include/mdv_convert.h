#ifdef __cplusplus
extern "C" {
#endif

/*************************************************************************
 * MDV_CONVERT.H : Prototypes and defines for routines for converting MDV
 *                 data between the supported formats.
 *
 * N. Rehak, Feb 1997. NCAR, RAP.
 */

#ifndef MDV_CONVERT_H
#define MDV_CONVERT_H

#include <dataport/port_types.h>

/******************************************************************************
 * MDV_CONVERT_VOLUME: Allocate space for a data volume (data for all levels)
 * and convert the given volume of data into the specified format. Caller is 
 * responsible for freeing up the returned buffer when done.
 *
 * Inputs: input_volume - pointer to the input volume of data.
 *         input_volume_size - number of bytes in the input volume.
 *         nx - grid size in the x direction.
 *         ny - grid size in the y direction.
 *         nz - grid size in the z direction.
 *         input_volume_format - the format of the input data.
 *         return_volume_format - the desired format for the returned data.
 *
 * Outputs: return_volume_size - number of bytes in returned volume.
 *
 * Returns: returns a pointer to the converted data volume information, or
 *          NULL if there is an error.  The space for the returned data volume
 *          is allocated by this routine and must be freed by the calling
 *          routine.
 */

ui08 *MDV_convert_volume(ui08 *input_volume,
			 int input_volume_size,
			 int nx,
			 int ny,
			 int nz,
			 int input_volume_format,
			 int return_volume_format,
			 int *return_volume_size);


#endif

#ifdef __cplusplus
}
#endif
