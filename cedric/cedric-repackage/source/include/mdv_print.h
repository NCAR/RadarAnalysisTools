/* MDV print routines and their utilities -- Rachel Ames */

#ifdef __cplusplus
 extern "C" {
#endif

#include <stdio.h>
#include <time.h> 
#include "mdv_file.h"


void MDV_print_master_header(MDV_master_header_t *mmh, 
                             FILE *outfile);

void MDV_print_master_header_full(MDV_master_header_t *mmh, 
                                  FILE *outfile);

void MDV_print_field_vlevel_header(MDV_field_vlevel_header_t *mfvh, 
                                   FILE *outfile);

void MDV_print_field_vlevel_header_full(MDV_field_vlevel_header_t *mfvh,
                                        FILE *outfile);

void MDV_print_field_header(MDV_field_header_t *mvh, 
                            FILE *outfile);

void MDV_print_field_header_full(MDV_field_header_t *mvh, 
                                 FILE *outfile);

void MDV_print_vlevel_header(MDV_vlevel_header_t *mvh, int nz, 
                             char *field_name, FILE *outfile);

void MDV_print_vlevel_header_full(MDV_vlevel_header_t *mvh, int nz, 
                                  char *field_name, FILE *outfile);

void MDV_print_chunk_header(MDV_chunk_header_t *mch, FILE *outfile);

void MDV_print_chunk_header_full(MDV_chunk_header_t *mch, FILE *outfile);

void MDV_print_chunk_data_full(void *data, si32 chunk_id,
			       si32 size, FILE *outfile);

void MDV_print_dataset_data(MDV_dataset_t *dataset,
			    FILE *outfile);

void MDV_print_field_plane_full(MDV_field_header_t *fhdr,
				void *plane_ptr,
				int field_num, int plane_num,
				FILE *outfile);

void MDV_print_dataset(MDV_dataset_t *dataset, FILE *outfile);

void MDV_print_dataset_full(MDV_dataset_t *dataset, FILE *outfile);

char *MDV_verttype2string(int vert_type);

char *MDV_proj2string(int proj_type);

char *MDV_encode2string(int encode_type);

char *MDV_colltype2string(int coll_type);

char *MDV_orient2string(int orient_type);

char *MDV_order2string(int order_type);

#ifdef __cplusplus
}
#endif
