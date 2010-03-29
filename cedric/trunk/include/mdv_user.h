#ifdef __cplusplus
extern "C" {
#endif

/*************************************************************************
 * MDV_USER.H : Defines necessary for using general MDV routines.
 *
 * F. Hage Dec 1993. NCAR, RAP.
 *
 */

#define MDV_USER_H

#include "os_config.h"

#include "mdv_file.h"
#include "mdv_print.h"
#include "mdv_read.h"
#include "mdv_utils.h"
#include "mdv_write.h"


/******************************************************************************
 * MDV_FREE_FIELD_VLEVEL_HEADER: Frees space for field_vlevel header fields.
 *
 * Inputs: fv_hdr - pointer to field/vlevel header structure to be freed.
 *
 * Outputs: fv_hdr - internal pointers are freed and set to NULL.  The
 *                   MDV_field_vlevel_header_t pointer is NOT freed by
 *                   this routine.
 */

void MDV_free_field_vlevel_header( MDV_field_vlevel_header_t *fv_hdr);

/******************************************************************************
 * MDV_FREE_DATASET: Frees space for an MDV_dataset.  If the datafile_buf
 * pointer is NULL, the dataset is assumed to have been constructed by the
 * calling routine and each header/data pointer is freed separately and the
 * pointers are set to NULL.  If the datafile_buf pointer is set, the dataset
 * is assumed to have been read directly from disk and the datafile_buf
 * pointer is freed and all of the other pointers are set to NULL.
 *
 * Inputs: dataset - pointer to dataset to be freed.
 *
 * Outputs: dataset - pointers are freed and set to NULL.  The MDV_dataset_t
 *                    pointer is NOT freed by this routine.
 */
 
void MDV_free_dataset( MDV_dataset_t *dataset);
 
/******************************************************************************
 * MDV_INIT_DATASET: Initializes an MDV_dataset_t struct.  No memory is freed
 * by this routine.  If the dataset contains memory which needs to be freed,
 * call MDV_free_dataset.
 *
 * Inputs: dsp - pointer to dataset to be initialized.
 *
 * Outputs: dsp - pointers are set to NULL and values are initialized to 0.
 *                No memory is freed by this routine.
 */

void MDV_init_dataset( MDV_dataset_t *dsp);

/******************************************************************************
 * MDV_INIT_MASTER_HEADER: Initializes an MDV_master_header_t struct.
 *
 * Inputs: master_hdr - pointer to master header to be initialized.
 *
 * Outputs: master_hdr - values are set to "initialized" values.  Generally,
 *                       this means 0, but sets things like the FORTRAN
 *                       record lengths appropriately.
 */

void MDV_init_master_header(MDV_master_header_t *master_hdr);

/******************************************************************************
 * MDV_INIT_FIELD_HEADER: Initializes an MDV_field_header_t struct.
 *
 * Inputs: field_hdr - pointer to field header to be initialized.
 *
 * Outputs: field_hdr - values are set to "initialized" values.  Generally,
 *                      this means 0, but sets things like the FORTRAN
 *                      record lengths appropriately.
 */

void MDV_init_field_header(MDV_field_header_t *field_hdr);

/******************************************************************************
 * MDV_INIT_VLEVEL_HEADER: Initializes an MDV_vlevel_header_t struct.
 *
 * Inputs: vlevel_hdr - pointer to vlevel header to be initialized.
 *
 * Outputs: vlevel_hdr - values are set to "initialized" values.  Generally,
 *                       this means 0, but sets things like the FORTRAN
 *                       record lengths appropriately.
 */

void MDV_init_vlevel_header(MDV_vlevel_header_t *vlevel_hdr);

/******************************************************************************
 * MDV_INIT_CHUNK_HEADER: Initializes an MDV_chunk_header_t struct.
 *
 * Inputs: chunk_hdr - pointer to chunk header to be initialized.
 *
 * Outputs: chunk_hdr - values are set to "initialized" values.  Generally,
 *                      this means 0, but sets things like the FORTRAN
 *                      record lengths appropriately.
 */

void MDV_init_chunk_header(MDV_chunk_header_t *chunk_hdr);

/******************************************************************************
 * MDV_SET_MASTER_HDR_OFFSETS: Set all of the offset values the master
 * header.  The offset values are set based on the values of the other
 * fields in the header, so these must be set before this routine is 
 * called.
 *
 * Inputs: m_hdr - pointer to master header to be updated.
 *
 * Outputs: m_hdr - all offset values are set appropriately.
 */
 
void MDV_set_master_hdr_offsets(MDV_master_header_t *m_hdr);

/******************************************************************************
 * MDV_SET_CHUNK_HDR_OFFSETS: Set the chunk data offsets in all of the chunk
 * headers.  This is done based on the offset for the last data field, so
 * this value must be set before this routine is called.
 *
 * Inputs: dataset - pointer to the dataset whose chunk header offset values
 *                   are to be updated.
 *
 * Outputs: dataset - all offset values in the chunk headers are set
 *                    appropriately.  Othere dataset information is used
 *                    to determine these offsets and must be accurate
 *                    before this routine is called.
 */
 
void MDV_set_chunk_hdr_offsets(MDV_dataset_t *dataset);

/******************************************************************************
 * MDV_GET_FIRST_FIELD_OFFSET: Get the file offset for the first field in
 * the MDV file.
 *
 * Inputs: dataset - pointer to the dataset whose chunk header offset values
 *                   are to be updated.
 *
 * Outputs: dataset - all offset values in the chunk headers are set
 *                    appropriately.  Othere dataset information is used
 *                    to determine these offsets and must be accurate
 *                    before this routine is called.
 */
 
si32 MDV_get_first_field_offset(MDV_master_header_t *master_hdr);

#ifdef UNDER_DEVEL

/****** THESE FUNCTIONS ARE NOT CURRENTLY IMPLEMENTED as of 1/5/96 *********/
/* But they are planned and in progress.                                   */
/***************************************************************************/

int MDV_name_to_utime(char *dir_name,char *f_name);


int MDV_find_data_sets( char **top_dir, 
                        int num_dirs, 
                        char *name_list[],
                        char *match,  /* the file name must contain a match to
                                         this regular expression to be valid */
                        int len,      /* min length of file names in bytes */
                        int t_min,    /* min time of interest */
                        int t_max);   /* max time of interest */


int MDV_find_forecast_files( char *dir, 
                        char *name_list[],
                        char *match, /* the file name must contain a match to 
                                        this regular expression to be valid */
                        int len,     /* min length of file names in bytes */
                        int t_min,   /* min time of interest */
                        int t_max);  /* max time of interest */

MDV_master_header_t* MDV_load_info( char *file_name);

MDV_master_header_t* MDV_get_info( char *host, 
                                   int port, 
                                   int generation_time, 
                                   int fcast_time );

MDV_master_header_t* MDV_get_info_si( char *type, 
                                      char *instance, 
                                      int generation_time, 
                                      int fcast_time);
 
MDV_data_reply_t* MDV_load_data( char *file_name, 
                                 MDV_data_request_t  *request);

MDV_data_reply_t* MDV_get_data( char * host, 
                                int port, 
                                MDV_data_request_t  *request);

MDV_data_reply_t* MDV_get_data( char *type, 
                                char *instance, 
                                MDV_data_request_t  *request);

FILE *MDV_open_data_time(int dtime,
                         int num_data_dirs,
                         char **data_dir,
                         char *data_set_suffix,
                         char *file_suffix);
 
#endif /* UNDER_DEVEL */

#ifdef __cplusplus
}
#endif
