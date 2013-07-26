#ifdef __cplusplus
extern "C" {
#endif

/*************************************************************************
 * MDV_UTILS.H : Contains utility routiens for MDV files
 *
 * R. Ames March 1996. NCAR, RAP.
 *
 */

#define MDV_UTILS_H

#include "mdv_file.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#ifndef MDV_SUCCESS
#define MDV_SUCCESS 0
#endif

#ifndef MDV_FAILURE
#define MDV_FAILURE -1
#endif


#ifndef MDV_UTILS_C
 
/*****************************************************************
 * MDV_DATA_ELEMENT_SIZE: Give the size (in bytes) of the data 
 * element given the encoding type integer.
 * Returns -1 if encoding type is MDV_NATIVE.
 * --Rachel Ames 3/96
 * Binary encoding of the data in the arrays. 
 * MDV_NATIVE   0     Whatever units the data is already in 
 * MDV_INT8     1     Uncompressed 8 bit unsigned integers 
 * MDV_INT16    2     Uncompressed 16 bit unsigned integers 
 * MDV_INT32    3     Uncompressed 32 bit unsigned integers
 * MDV_INT64    4     Uncompressed 64 bit unsigned integers
 * MDV_FLOAT32  5     Uncompressed 32 bit signed IEEE Floats
 * MDV_FLOAT64  6     Uncompressed 64 bit signed IEEE Floats 
 */

int MDV_data_element_size(int encoding_type);

/*
 * Swap routines.
 */

/*****************************************************************
 * MDV_MASTER_HEADER_FROM_BE: Converts master header from big endian
 * format to native format.  Nancy Rehak 6/97
 */

void MDV_master_header_from_BE(MDV_master_header_t *m_hdr) ;

/*****************************************************************
 * MDV_MASTER_HEADER_TO_BE: Converts master header from native
 * format to big endian format.  Nancy Rehak 6/97
 */

void MDV_master_header_to_BE(MDV_master_header_t *m_hdr) ;

/*****************************************************************
 * MDV_FIELD_HEADER_FROM_BE: Converts field header from big endian
 * format to native format.  Nancy Rehak 6/97
 */

void MDV_field_header_from_BE(MDV_field_header_t *f_hdr);

/*****************************************************************
 * MDV_FIELD_HEADER_TO_BE: Converts field header from native
 * format to big endian format.  Nancy Rehak 6/97
 */

void MDV_field_header_to_BE(MDV_field_header_t *f_hdr);

/*****************************************************************
 * MDV_VLEVEL_HEADER_FROM_BE: Conversts vlevel header from big endian
 * format to native format.  Nancy Rehak 6/97
 */
 
void MDV_vlevel_header_from_BE(MDV_vlevel_header_t *v_hdr);

/*****************************************************************
 * MDV_VLEVEL_HEADER_TO_BE: Converts vlevel header from native
 * format to big endian format.  Nancy Rehak 6/97
 */
 
void MDV_vlevel_header_to_BE(MDV_vlevel_header_t *v_hdr);

/*****************************************************************
 * MDV_FIELD_VLEVEL_HEADER_FROM_BE: Converts a field_vlevel header
 * from big endian format to native format.  Nancy Rehak 6/97
 */
 
void MDV_field_vlevel_header_from_BE(MDV_field_vlevel_header_t *fv_head);

/*****************************************************************
 * MDV_FIELD_VLEVEL_HEADER_TO_BE: Converts a field_vlevel header
 * from native format to big endian format.  Nancy Rehak 6/97
 */
 
void MDV_field_vlevel_header_to_BE(MDV_field_vlevel_header_t *fv_head);

/*****************************************************************
 * MDV_CHUNK_HEADER_FROM_BE: Converts a chunk header from big endian
 * format to native format.  Nancy Rehak 6/97
 */
 
void MDV_chunk_header_from_BE(MDV_chunk_header_t *c_hdr);

/*****************************************************************
 * MDV_CHUNK_HEADER_TO_BE: Converts a chunk header from native
 * format to big endian format.  Nancy Rehak 6/97
 */
 
void MDV_chunk_header_to_BE(MDV_chunk_header_t *c_hdr);

/*****************************************************************
 * MDV_UNENCODED_VOLUME_FROM_BE: Converts the data in an unencoded
 * data volume from big endian format to native format.  N. Rehak 6/97
 *
 * returns MDV_SUCCESS or MDV_FAILURE
 */
 
int MDV_unencoded_volume_from_BE(void *volume_data,
				 ui32 volume_size,
				 int data_type);

/*****************************************************************
 * MDV_PLANE_RLE8_FROM_BE: Converts the data in a plane of data
 * encoded in the MDV_PLANE_RLE8 format from big endian format to
 * native format.  N. Rehak 6/97
 *
 * returns MDV_SUCCESS or MDV_FAILURE
 */
 
int MDV_plane_rle8_from_BE(void *plane_data);

/*****************************************************************
 * MDV_PLANE_RLE8_TO_BE: Converts the data in a plane of data
 * encoded in the MDV_PLANE_RLE8 format from native format to
 * big endian format.  N. Rehak 6/97
 *
 * returns MDV_SUCCESS or MDV_FAILURE
 */
 
int MDV_plane_rle8_to_BE(void *plane_data);

/*****************************************************************
 * MDV_CHUNK_DATA_FROM_BE: Converts chunk data from big endian
 * format to native format if the chunk data type is a known type.
 * The data pointer must point to the record length value preceding
 * the chunk data.  N. Rehak 6/97
 *
 * returns MDV_SUCCESS or MDV_FAILURE */
 
int MDV_chunk_data_from_BE(void *c_data,
			   MDV_chunk_header_t *c_hdr);

/*****************************************************************
 * MDV_CHUNK_DATA_TO_BE: Converts chunk data from native format to
 * big endian format if the chunk data type is a known type.
 * The data pointer must point to the record length value preceding
 * the chunk data.  N. Rehak 6/97
 *
 * returns MDV_SUCCESS or MDV_FAILURE */
 
int MDV_chunk_data_to_BE(void *c_data,
			 MDV_chunk_header_t *c_hdr);

/*****************************************************************
 * MDV_get_field_name: Returns pointer to field name - NULL on error
 *
 */

char *MDV_get_field_name(int field_code);

/*****************************************************************
 * MDV_get_field_units: Returns pointer to field units - NULL on error
 *
 */

char *MDV_get_field_units(int field_code);

/*****************************************************************
 * MDV_get_field_abbrev: Returns pointer to field abbrev - NULL on error
 *
 */

char *MDV_get_field_abbrev(int field_code);


#endif /* MDV_UTILS_C not defined */
#ifdef __cplusplus
}
#endif
