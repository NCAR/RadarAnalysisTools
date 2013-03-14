#ifdef __cplusplus
extern "C" {
#endif

/******************************************************************************
 *  MDV_WRITE.C  Subroutines writing MDV data to files.
 *  F. Hage.  Dec 1993. RAP, R. Ames 6/96.
 *
 *  Separated from mdv_user.c.  N. Rehak, Aug. 1996.
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>
#include "./include/mdv_utils.h"
#include "./include/os_config.h"
#define MDV_COMPRESSION_NONE 0
#define MDV_COMPRESSION_RLE  1
#define MDV_SCALING_ROUNDED   1

/*
 * Forward declarations to internal routines
 */

static int write_field_int8(FILE *outfile,
			    MDV_field_header_t *field_hdr,
			    ui08 *volume_data);

static int write_field_plane_rle8(FILE *outfile,
				  MDV_field_header_t *field_hdr,
				  ui08 *volume_data,
				  int field_data_offset,
				  int field_num);

static int write_field_row_rle8(FILE *outfile,
				MDV_field_header_t *field_hdr,
				ui08 *volume_data,
				int field_data_offset,
				int field_num);



static int write_data_plane_rle8(FILE *outfile,
				 si32 volume_size,
				 si32 nplanes,
				 si32 *plane_locs,
				 si32 *plane_sizes,
				 ui08 **volume_data);

static int write_data_row_rle8(FILE *outfile,
			       ui08 *row_data,
			       si32 row_size);

static int write_fortran_reclen(FILE *outfile,
				si32 record_length);

static int write_row_locs(FILE *outfile,
			  si32 *row_locs,
			  int num_rows);

static int write_row_sizes(FILE *outfile,
			   si32 *row_sizes,
			   int num_rows);





/******************************************************************************

int MDV_write_dataset(FILE *outfile, MDV_dataset_t *dataset,
		      int output_encoding_type,
		      int swap_chunk_data)
{

} /* end MDV_write_dataset */


/******************************************************************************
 * MDV_WRITE_MASTER_HEADER: Write an MDV master header to the file in the
 * appropriate place.
 *
 * Inputs: outfile - pointer to the output file.  Assumed to be currently
 *                   open for write.
 *         m_hdr - pointer to master header information.
 *
 * Outputs: outfile - updated on disk to contain the information in the
 *                    master header, byte swapped if necessary.
 *
 * Returns: MDV_SUCCESS or MDV_FAILURE.
 */

int MDV_write_master_header(FILE *outfile, MDV_master_header_t *m_hdr)
{
  static char *routine_name = "MDV_write_master_header";
  
  MDV_master_header_t m_hdr_be = *m_hdr;
  
  /*
   * Make sure the output file pointer is valid.
   */

  if (outfile == NULL)
  {
    fprintf(stderr,
            "%s: Invalid output file pointer.\n",
            routine_name);
    return(MDV_FAILURE);
  }

  /*
   * Move to the beginning of the file.
   */

  if (fseek(outfile, 0, SEEK_SET) != 0) 
  {
    fprintf(stderr,
            "%s: Error moving to beginning of output file.\n",
            routine_name);
    return(MDV_FAILURE);
  }
  
  /*
   * Make sure the header is in big-endian format.
   */

  MDV_master_header_to_BE(&m_hdr_be);
  
  /*
   * Write the header to the output file.
   */

  if (fwrite(&m_hdr_be, sizeof(MDV_master_header_t), 1, outfile) != 1)
  {
    fprintf(stderr,
            "%s: Error writing master header to output file.\n",
            routine_name);
    return(MDV_FAILURE);
  }
  
  return(MDV_SUCCESS);
}


/******************************************************************************
 * MDV_WRITE_FIELD: Write the MDV field header and data from the given 
 * structure into the proper place in the file.  The position of the
 * field header is calculated from the field number and the position of
 * the field data is passed in since this position may be different from
 * the position of the data in the dataset in memory if the data is being
 * output in a format different from that in memory.
 *
 * Inputs: outfile - pointer to the output file.  Assumed to be currently
 *                   open for write.
 *         f_hdr - pointer to the field header information.
 *         f_data - pointer to the field data.  The format of this data
 *                  must match the description given in the field header.
 *         field_num - the field number for this field information.  This
 *                     is used for positioning the field header in the
 *                     output file and for error messages.
 *         field_data_offset - offset in the output file where the field
 *                             data should begin.  This may differ from
 *                             the field data offset in the header if the
 *                             data is being written in a different format
 *                             than it is being stored in memory.
 *         output_encoding_type - format for the output field data.
 *
 * Outputs: outfile - updated on disk to conatine the information in the
 *                    field header and the field data.  The field header
 *                    information on disk contains the field data offset
 *                    and encoding type of the data as it is written to
 *                    disk rather than that in f_hdr.
 *
 * Returns: the number of bytes written to the file for the field data, or
 *          -1 on error.
 */

int MDV_write_field(FILE *outfile, MDV_field_header_t *f_hdr,
		    void *f_data,
		    int field_num,
		    int field_data_offset,
		    int output_encoding_type)
{

}


/******************************************************************************
 * MDV_WRITE_FIELD_HEADER: Write an MDV field header from the given 
 * structure into the proper place in the file.
 *
 * Inputs: outfile - pointer to the output file.  Assumed to be currently
 *                   open for write.
 *         f_hdr - pointer to field header information.
 *         field_num - the field number for this field.  This is used to
 *                     determine where in the file the header will be
 *                     written.
 *
 * Outputs: outfile - updated on disk to contain the information in the
 *                    field header, byte swapped if necessary.
 *
 * Returns: MDV_SUCCESS or MDV_FAILURE.
 */

int MDV_write_field_header(FILE *outfile, MDV_field_header_t *f_hdr,
			   int field_num)
{

  static char *routine_name = "MDV_write_field_header";
  
  long hdr_offset;
  MDV_field_header_t f_hdr_be = *f_hdr;
  
  /*
   * Make sure the output file pointer is valid.
   */

  if (outfile == NULL)
  {
    fprintf(stderr,
            "%s: Invalid output file pointer.\n",
            routine_name);
    return(MDV_FAILURE);
  }

  /*
   * Move to the appropriate position in the output file.
   */

  hdr_offset = sizeof(MDV_master_header_t) + 
    (field_num * sizeof(MDV_field_header_t));

  if (fseek(outfile, hdr_offset, SEEK_SET) != 0) 
  {
    fprintf(stderr,
            "%s: Error moving to field header %d position in output file.\n",
            routine_name, field_num);
    return(MDV_FAILURE);
  }

  /*
   * Make sure the header is in big-endian format.
   */

  MDV_field_header_to_BE(&f_hdr_be);
  
  /*
   * Write the header to the output file.
   */

  if (fwrite(&f_hdr_be, sizeof(MDV_field_header_t), 1, outfile) != 1)
  {
    fprintf(stderr,
            "%s: Error writing field header %d to output file.\n",
            routine_name, field_num);
    return(MDV_FAILURE);
  }

  return(MDV_SUCCESS);

}


/******************************************************************************
 * MDV_WRITE_VLEVEL_HEADER: Write an MDV vlevel header from the given
 * structure into the proper place in the file.
 *
 * Inputs: outfile - pointer to the output file.  Assumed to be currently
 *                   open for write.
 *         v_hdr - pointer to vlevel header information.
 *         m_hdr - pointer to associated master header information.  This
 *                 information is used to position the vlevel header in
 *                 the output file.
 *         field_num - the number of the field associated with the vlevel
 *                     information.  This value is also used to position
 *                     the vlevel header in the output file.
 *
 * Outputs: outfile - updated on disk to contain the information in the
 *                    vlevel header, byte swapped if necessary.
 *
 * Returns: MDV_SUCCESS or MDV_FAILURE.
 */
 
int MDV_write_vlevel_header(FILE *outfile, MDV_vlevel_header_t *v_hdr,
			    MDV_master_header_t *m_hdr, int field_num)
{

}


/******************************************************************************
 * MDV_WRITE_FIELD_VLEVEL_HEADER: Write an MDV field header and an MDV vlevel
 * header into a file.  Note, these are not written in the file sequentially.
 * The master header field and vlevel offsets MUST be correct!
 *
 * Inputs: outfile - pointer to the output file.  Assumed to be currently
 *                   open for write.
 *         fv_hdr - pointer to the field/vlevel header structure to be
 *                  written to disk.
 *         m_hdr - pointer to the associated master header information.
 *                 This information is used for positioning the field and
 *                 vlevel headers in the file.
 *         field_num - the number of the associated field.  This information
 *                     is also used for positioning the headers in the file.
 *
 * Returns: MDV_SUCCESS or MDV_FAILURE.
 */
 
int MDV_write_field_vlevel_header(FILE *outfile,
				  MDV_field_vlevel_header_t *fv_hdr,
				  MDV_master_header_t *m_hdr, int field_num)
{

}
 



/*********************************************************
 * STATIC ROUTINES
 *********************************************************/

/******************************************************************************
 * WRITE_FIELD_INT8: Write the field data in MDV_INT8 format.  Returns the
 *                   size, in bytes, of the volume of data written to disk,
 *                   or -1 on error.
 */

static int write_field_int8(FILE *outfile,
			    MDV_field_header_t *field_hdr,
			    ui08 *volume_data)
{


} /* end write_field_int8 */

/******************************************************************************
 * WRITE_FIELD_INT16: Write the field data in MDV_INT8 format.  Returns the
 *                   size, in bytes, of the volume of data written to disk,
 *                   or -1 on error.
 */

static int write_field_int16(FILE *outfile,
			    MDV_field_header_t *field_hdr,
			    ui16 *volume_data)
{

} /* end write_field_int8 */


/******************************************************************************
 * WRITE_FIELD_PLANE_RLE8: Write the field data in MDV_PLANE_RLE8 format.
 *                         Returns the size, in bytes, of the volume of data
 *                         written to disk, or -1 on error.
 */

static int write_field_plane_rle8(FILE *outfile,
				  MDV_field_header_t *field_hdr,
				  ui08 *volume_data,
				  int field_data_offset,
				  int field_num)
{

    
} /* end write_field_plane_rle8 */


/******************************************************************************
 * WRITE_FIELD_ROW_RLE8: Write the field data in MDV_ROW_RLE8 format.
 *                       Returns the size, in bytes, of the volume of data
 *                       written to disk, or -1 on error.
 *
 *                       Assumes that the output file pointer is currently
 *                       positioned at the beginning of the FORTRAN record
 *                       length preceding the volume data.
 *
 * Returns the size in bytes of the output volume, or -1 on error.
 */

static int write_field_row_rle8(FILE *outfile,
				MDV_field_header_t *field_hdr,
				ui08 *volume_data,
				int field_data_offset,
				int field_num)
{

    
} /* end write_field_row_rle8 */


/******************************************************************************
 * WRITE_DATA_INT16: Write the given data volume to disk in MDV_INT8
 *                  format.  This routine performs any data swapping
 *                  necessary for the write.
 */

static int write_data_int16(FILE *outfile,
			   si32 volume_size,
			   ui16 *volume_data)
{

} /* end write_data_int16 */

/******************************************************************************
 * WRITE_DATA_PLANE_RLE8: Write the given data volume to disk in
 *                        MDV_PLANE_RLE8 format.  This routine performs
 *                        any data swapping necessary for the write.
 *                        The volume_size sent in includes the sizes of
 *                        the plane_locs array and the plane_sizes array.
 */

static int write_data_plane_rle8(FILE *outfile,
				 si32 volume_size,
				 si32 nplanes,
				 si32 *plane_locs,
				 si32 *plane_sizes,
				 ui08 **volume_data)
{


} /* end write_data_plane_rle8 */


/******************************************************************************
 * WRITE_DATA_ROW_RLE8: Write the given data row to disk in MDV_ROW_RLE8
 *                      format.  This routine performs any data swapping
 *                      necessary for the write.  Note that this routine
 *                      only writes out the given row and doesn't write
 *                      the FORTRAN record lengths or the row size and
 *                      location arrays.
 *
 * Returns MDV_SUCCESS or MDV_FAILURE.
 */

static int write_data_row_rle8(FILE *outfile,
			       ui08 *row_data,
			       si32 row_size)
{


} /* end write_data_row_rle8 */


/******************************************************************************
 * WRITE_FORTRAN_RECLEN: Writes the FORTRAN record length field to the
 *                       output file at the current position.  Swaps the
 *                       data as necessary.
 *
 * Returns MDV_SUCCESS or MDV_FAILURE.
 */

static int write_fortran_reclen(FILE *outfile,
				si32 record_length)
{

} /* end write_fortran_reclen */


/******************************************************************************
 * WRITE_ROW_LOCS: Writes the row locations array to the output file at the
 *                 current position for MDV_ROW_RLE8 files.  Swaps the data
 *                 as necessary.
 *
 * Returns MDV_SUCCESS or MDV_FAILURE.
 */

static int write_row_locs(FILE *outfile,
			  si32 *row_locs,
			  int num_rows)
{

} /* end write_row_locs */


/******************************************************************************
 * WRITE_ROW_SIZES: Writes the row sizess array to the output file at the
 *                  current position for MDV_ROW_RLE8 files.  Swaps the data
 *                  as necessary.
 *
 * Returns MDV_SUCCESS or MDV_FAILURE.
 */

static int write_row_sizes(FILE *outfile,
			   si32 *row_sizes,
			   int num_rows)
{

} /* end write_row_sizes */

#ifdef __cplusplus
}
#endif
