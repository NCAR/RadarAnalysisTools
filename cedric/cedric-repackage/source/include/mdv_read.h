#ifdef __cplusplus
extern "C" {
#endif

/*************************************************************************
 * MDV_READ.H : Prototypes and defines for routines for reading MDV data
 *              from files.
 *
 * F. Hage Dec 1993. NCAR, RAP.
 *
 * Separated from mdv_user.h.  N. Rehak, Aug. 1996.
 */

#ifndef MDV_READ_H
#define MDV_READ_H

#include <stdio.h>

#include "os_config.h"
#include "mdv_handle.h"
#include "mdv_file.h"

/******************************************************************************
 * MDV_VERIFY:
 *
 * Verify that a file is in MDV format
 *
 * Returns: TRUE or FALSE
 */
 
extern int MDV_verify( char * infile_name);

/******************************************************************************
 * MDV_GET_DATASET: Allocate space for an entire dataset. Read the headers
 * and set the pointers to beginning of field and chunk data.
 * Caller is responsible for freeing up the buffer when done. 
 * Caller is responsible for making sure the MDV_dataset_t is correct and 
 * proper for the open file.
 *
 * Inputs: infile_name - name of the input file.
 *         dsp - pointer to the dataset used for storing the information in
 *               memory.  If the dataset is orignally empty, it must be
 *               initialized before calling this routine.
 *
 * Outputs: dsp - updated to contain the dataset information from the input
 *                file, byte swapped if necessary.  Memory is allocated or
 *                reallocated as needed.  Note that chunk data will only be
 *                byte swapped if it is of a type known to the library.
 *
 * Returns: MDV_SUCCESS or MDV_FAILURE.
 */
 
extern int MDV_get_dataset(char *infile_name, MDV_dataset_t *dsp);

/******************************************************************************
 * MDV_LOAD_MASTER_HEADER: Load mdv data file header into given area from
 * disk.  Memory for the master header is assumed to be allocated before
 * this routine is called.  The bytes in the header are swapped if necessary
 * to put the data in native format.
 *
 * Inputs: infile - pointer to the input file.  This is assumed to currently
 *                  be open for read.
 *         m_hdr - pointer to the master header to be loaded.
 *
 * Outputs: m_hdr - updated to include the values read in from disk,
 *                  byte swapped as necessary.
 *
 * Returns: MDV_SUCCESS or MDV_FAILURE.
 */

extern int MDV_load_master_header(FILE *infile, MDV_master_header_t *m_hdr);
 
/******************************************************************************
 * MDV_LOAD_FIELD_HEADER: Load mdv field header data into the given structure
 * from disk.  Memory for the field header is assumed to be allocated before
 * this routine is called.  The bytes in the header are swapped if necessary
 * to put the data in native format.
 *
 * Inputs: infile - pointer to the input file.  This is assumed to currently
 *                  be open for read.
 *         f_hdr - pointer to the field header to be loaded.
 *         field_num - number of the field being loaded.  This is used to
 *                     determine the position on disk where the field header
 *                     information is located.
 *
 * Outputs: f_hdr - updated to include the values read in from disk, byte
 *                  swapped as necessary.
 *
 * Returns: MDV_SUCCESS or MDV_FAILURE.
 */

extern int MDV_load_field_header(FILE *infile, MDV_field_header_t *f_hdr,
				 int field_num);

/******************************************************************************
 * MDV_LOAD_VLEVEL_HEADER: Load mdv vlevel header data into the given
 * structure from disk.  Memory for the vlevel header is assumed to be
 * allocated before this routine is called.  The bytes in the header are
 * swapped if necessary to put the data in native format.
 *
 * Inputs: infile - pointer to the input file.  This is assumed to currently
 *                  be open for read.
 *         v_hdr - pointer to the vlevel header to be loaded.
 *         offset - This information is used to determine the position on 
 *                 disk where the vlevel header information is located.
 *         field_num - the field number.  This information is also used to
 *                     determine the disk location.
 *
 * Output: v_hdr - updated to include the values read in from disk,
 *                 byte swapped as necessary.
 *
 * Returns: MDV_SUCCESS or MDV_FAILURE.
 */
 
extern int MDV_load_vlevel_header(FILE *infile, MDV_vlevel_header_t *v_hdr,
				  int offset, int vlevel_num);

/******************************************************************************
 * MDV_LOAD_VLEVEL_HEADER_OFFSET: Load mdv vlevel header data into the given
 * structure from disk given the offset for the first vlevel header in the
 * file rather than the master header.  Memory for the vlevel header is
 * assumed to be allocated before this routine is called.  The bytes in the
 * header are swapped if necessary to put the data in native format.
 *
 * Inputs: infile - pointer to the input file.  This is assumed to currently
 *                  be open for read.
 *         v_hdr - pointer to the vlevel header to be loaded.
 *         vlevel_offset - offset in the file to the first vlevel header.
 *                         This information is used to determine the disk
 *                         location of the desired vlevel header.
 *         field_num - the field number.  This information is also used to
 *                     determine the disk location.
 *
 * Output: v_hdr - updated to include the values read in from disk,
 *                 byte swapped as necessary.
 *
 * Returns: MDV_SUCCESS or MDV_FAILURE.
 */
 
extern int MDV_load_vlevel_header_offset(FILE *infile,
					 MDV_vlevel_header_t *v_hdr, 
					 int vlevel_offset,
					 int field_num);

/******************************************************************************
 * MDV_LOAD_FIELD_VLEVEL_HEADER: Load mdv field_vlevel header data into the 
 * given structure from disk.  Memory for the field/vlevel header is assumed
 * to be allocated before this routine is called.  Memory for the individual
 * structures pointed to by the field/vlevel header are allocated by this
 * routine.
 *
 * Inputs:  infile - pointer to the input file.  This is assumed to currently
 *                  be open for read.
 *          fv_hdr - pointer to the field/vlevel header to be loaded.
 *          m_hdr - pointer to the associated master header information.
 *                  This information is used to determine the position on
 *                  disk where the field and vlevel headers are located.
 *          field_num - the field number.  This information is also used to
 *                      determine the header locations.
 *
 * Outputs:  fv_hdr - updated to contain the field and vlevel header
 *                    information.  Memory is allocated for both loaded
 *                    headers.
 *
 * Returns: MDV_SUCCESS or MDV_FAILURE.
 */
 
extern int MDV_load_field_vlevel_header(FILE *infile,
					MDV_field_vlevel_header_t *fv_hdr,
					MDV_master_header_t *m_hdr,
					int field_num);

/******************************************************************************
 * MDV_LOAD_CHUNK_HEADER: Load mdv chunk header data into the given structure
 * from disk.  Memory for the chunk header is assumed to be allocated before
 * this routine is called.  The bytes in the header are swapped if necessary.
 *
 * Inputs: infile - pointer to the input file.  This is assumed to currently
 *                  be open for read.
 *         c_hdr - pointer to the chunk header to be loaded.
 *         m_hdr - pointer to the assocated master header information.  This
 *                 information is used to determine the position on disk
 *                 where the chunk header information is located.
 *         chunk_num - the chunk number.  This information is also used to
 *                     determine the chunk header location.
 *
 * Outputs: c_hdr - updated to include the values read in from disk, byte
 *                  swapped as necessary.
 *
 * Returns: MDV_SUCCESS or MDV_FAILURE.
 */
 
extern int MDV_load_chunk_header(FILE *infile, MDV_chunk_header_t *c_hdr,
				 MDV_master_header_t *m_hdr, int chunk_num);

/******************************************************************************
 * MDV_GET_VOLUME: Allocate space for a data volume (data for all levels) and 
 * read the desired volume into the buffer from the Open file. Caller is 
 * responsible for freeing up the buffer when done. Caller is responsible 
 * for making sure the field_header is correct and proper for the open file.
 *
 * Inputs: infile - pointer to the input file.  This is assumed to currently
 *                  be open for read.
 *         f_hdr - field header for the volume to be loaded.  This header
 *                 must contain the correct encoding_type, volume_size and
 *                 field_data_offset for the data.
 *         return_type - format in memory for the returned data volume.
 *
 * Outputs: None.
 *
 * Returns: returns a pointer to the data volume information, byte swapped if
 *          necessary, or NULL if there is an error.  This space for this
 *          data volume is allocated by this routine and must be freed by the
 *          calling routine.
 */

  /*extern void *MDV_get_volume(FILE *infile, MDV_field_header_t *f_hdr,
    int return_type);*/

/******************************************************************************
 * MDV_GET_VOLUME_SIZE: Allocate space for a data volume (data for all levels)
 * and read the desired volume into the buffer from the Open file. Returns the
 * size of the volume returned.  Caller is responsible for freeing up the
 * buffer when done. Caller is responsible for making sure the field_header is
 * correct and proper for the open file.
 *
 * Inputs: infile - pointer to the input file.  This is assumed to currently
 *                  be open for read.
 *         f_hdr - field header for the volume to be loaded.  This header
 *                 must contain the correct encoding_type, volume_size and
 *                 field_data_offset for the data.
 *         return_type - format in memory for the returned data volume.
 *
 * Outputs: volume_size - number of bytes in the volume of data returned.
 *
 * Returns: returns a pointer to the data volume information, byte swapped if
 *          necessary, or NULL if there is an error.  This space for this
 *          data volume is allocated by this routine and must be freed by the
 *          calling routine.
 */

  extern void * MDV_get_volume_size(FILE *infile,
				  MDV_field_header_t *f_hdr,
				  int return_type,int satflg,
				  int *volume_size);

/******************************************************************************
 * MDV_GET_PLANE: Allocate space for a data plane and read the desired plane
 * into the buffer from the Open file. Caller is responsible for freeing
 * up the buffer when done. Caller is responsible for making sure the
 * field_vlevel_header is correct and proper for the open file.
 *
 * Inputs: infile - pointer to the input file.  This is assumed to currently
 *                  be open for read.
 *         fld_hdr - field header for the plane to be loaded.  This header
 *                   must contain the correct encoding_type and
 *                   field_data_offset for the data.
 *         return_type - format in memory for the returned data plane.
 *         plane_num - number of the field plane to be loaded.
 *
 * Outputs: None.
 *
 * Returns: returns a pointer to the data plane information, byte swapped if
 *          necessary, or NULL if there is an error.  This space for this
 *          data plane is allocated by this routine and must be freed by the
 *          calling routine.
 */

extern void * MDV_get_plane(FILE *infile, MDV_field_header_t *f_hdr,
			    int return_type, int plane_num);

/******************************************************************************
 * MDV_GET_CHUNK_DATA: Get the chunk data for the given chunk.  The chunk
 * data is swapped if the library knows about that type of chunk data.
 *
 * Inputs: infile - pointer to the input file.  This is assumed to currently
 *                  be open for read.
 *         c_hdr - pointer to the chunk header to be loaded.
 *
 * Outputs: None.
 *
 * Returns: Pointer to the chunk data read from the file, byte swapped if
 *          necessary and if of a type known by the library.  Returns NULL
 *          on error.
 */
 
extern void *MDV_get_chunk_data(FILE *infile, MDV_chunk_header_t *c_hdr);

/*************************************************************************
 *
 * MDV_read_all
 *
 * Reads all fields and planes in volume.
 * Handle members and pointers are loaded up with the data.
 *
 * Must call MDV_init_handle() first.
 *
 * This routine may be called repeatedly - memory allocation is
 * handled within the routine.
 *
 * When done, call MDV_free_handle() to free memory. 
 *
 * Returns 0 on success, -1 on failure.
 *
 **************************************************************************/

extern int MDV_read_all(MDV_handle_t *mdv, char *file_path, int return_type);

#endif

#ifdef __cplusplus
}
#endif
