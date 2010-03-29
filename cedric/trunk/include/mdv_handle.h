#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************
*                    
* MDV_HANDLE.H
*
* MDV handle struct header file
*                   
* May 1997
*************************************************************************/

# ifndef MDV_HANDLE_H
# define MDV_HANDLE_H

#include "mdv_file.h"

/**************
 * MDV_handle_t
 *
 * The MDV_handle_handle_t is a container class for an entire
 * volume.
 *
 * It is initialized by MDV_init_handle(), and freed by
 * MDV_free_handle().
 */

typedef struct {
  
  MDV_master_header_t master_hdr; 	

  MDV_field_header_t *fld_hdrs; /* array of field headers */

  MDV_vlevel_header_t *vlv_hdrs; /* array of vlevel headers */

  MDV_chunk_header_t *chunk_hdrs; /* array of chunk headers */

  void **chunk_data; /* array of pointers to chunk data */

  void ***field_plane; /* array of field plane pointers -
			* field_plane[ifield][iplane] */

  /*
   * memory allocation
   */

  int n_fields_alloc;		               
  int n_chunks_alloc;		               
  int n_levels_alloc;

  /*
   * read_all flag - gets set after a read_all
   */

  int read_all_done;

} MDV_handle_t;

/*
 * prototypes
 */
/*************************************************************************
 *
 * MDV_init_handle
 *
 * initializes the memory associated with handle
 *
 * Returns 0 on success, -1 on failure.
 *
 **************************************************************************/

extern int MDV_init_handle(MDV_handle_t *mdv);

/*************************************************************************
 *
 * MDV_free_handle
 *
 * Frees the memory associated with handle
 *
 **************************************************************************/

extern void MDV_free_handle(MDV_handle_t *mdv);

/*****************************************************************************
 *
 * MDV_set_volume3d:  get a volume of mdv data and return it as a
 *                    three-dimensional array.
 *
 * The MDV_handle_t stores field data as a 2-d array.
 *
 * NOTE:  the caller is responsible for freeing the 3d dataset
 *        by calling MDV_free_dataset3d()
 *
 ****************************************************************************/

extern void *** MDV_set_volume3d( MDV_handle_t *mdv, int field_index, 
				  int return_type, int nrepeat_beams );

/*****************************************************************************
 *
 * MDV_free_volume3d: free memory associated with 3-d pointers
 *                    
 ****************************************************************************/

extern void MDV_free_volume3d(void ***three_d_array, int nrepeat_beams );

/*************************************************************************
 *
 * MDV_alloc_handle_arrays()
 *
 * allocates memory for handle arrays
 *
 **************************************************************************/

extern void MDV_alloc_handle_arrays(MDV_handle_t *mdv,
				    int n_fields,
				    int n_levels,
				    int n_chunks);

/*************************************************************************
 *
 * MDV_field_name_to_pos
 *
 * Returns field position to match the name.
 * Returns -1 on error (name not in file).
 *
 * NOTE: must use MDV_read_all() before using this function.
 *
 **************************************************************************/

extern int MDV_field_name_to_pos(MDV_handle_t *mdv,
				 char *field_name);
     
#endif /* MDV_HANDLE_H */

#ifdef __cplusplus
}
#endif
