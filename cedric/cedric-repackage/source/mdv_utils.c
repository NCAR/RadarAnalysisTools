/*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
 ** Copyright (c) 1999, UCAR
 ** University Corporation for Atmospheric Research(UCAR)
 ** National Center for Atmospheric Research(NCAR)
 ** Research Applications Program(RAP)
 ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA
 ** 1993/3/2 13:59:21
 *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*/
/*****************************************************************
 * MDV_UTILS.C: contains general purpose routines for working
 * with mdv files. 
 * R. Ames March 1996. NCAR, RAP.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include <sys/param.h>
#include "./include/mdv_macros.h"
#include "./include/compress.h"
#include "./include/bigend.h"
#include "./include/mem.h"
#include "./include/var_elev.h"
#include "./include/mdv_field_codes.h"
#include "./include/mdv_print.h"
#include "./include/mdv_utils.h"
#include "./include/mdv_private.h"


#include "./include/mdv_file.h"
#include <memory.h>
#include <netinet/in.h>


#define UMALLOC_CODE 0x0ff10ff2
static si32 Nmalloc = 0;
#define NBYTES_WORD 4
static int Nbytes_extra = 0;
static int Moffset = 0;
static char **Malloc_addr;
static int Nshift;
#define MDV_RL8_FLAG 0xfe0104fdU   /* This flag was used by early MDV */
                                   /* files.  Now, MDV files use the */
                                   /* RL8_FLAG, but we must keep this */
                                   /* flag also so the early MDV data can */
                                   /* still be decoded.  This is only used */
                                   /* in the check at the beginning of the */
                                   /* decode routine. */

/************
 * Static functions.
 */

static void *load_plane_int8(void *buffer,
			     MDV_field_header_t *field_hdr,
			     int data_type,
			     int plane_num,
			     int *return_size);

static void *load_plane_plane_rle8(void *buffer,
				   MDV_field_header_t *field_hdr,
				   int data_type,
				   int plane_num,
				   int *return_size);

static void *load_plane_row_rle8(void *buffer,
				 MDV_field_header_t *field_hdr,
				 int data_type,
				 int plane_num,
				 int *return_size);

/******************************************************************************
 * MDV_compressed()
 *
 * Returns TRUE if the compression_type is in the compressed range,
 *         FALSE otherwise.
 *
 * This test is needed because some files have headers which did not
 * zero out the spares, so the new compression stuff does not work
 * properly.
 */
/*******************************************************************/
int MDV_compressed(int compression_type)

{

  int true = 1;
  int false = 0;


  if (compression_type < MDV_COMPRESSION_RLE ||
      compression_type > MDV_COMPRESSION_GZIP) {
    return (false);
  } else {
    return (true);
  }

}

/*******************************************************************
 * umalloc_verify()
 *
 * verify malloc'd entries
 *
 *******************************************************************/

void umalloc_verify(void)
{

  si32 imalloc;
  si32 start_id;
  si32 end_id;
  ui32 msize;
  int Debug_level = 1;
  void alloc_check_block();


  if (Debug_level > 1) {

    for (imalloc = 0; imalloc < Nmalloc; imalloc++) {

      alloc_check_block (Malloc_addr[imalloc],
			 &start_id, &end_id, &msize);

    } /* imalloc */

  } /* if (debug_level > 1) */

}

/*******************************************************************
 * urealloc()
 *
 * reallocation
 *
 ********************************************************************/

void *(urealloc)(void *user_addr, size_t size)

{

  char *addr;
  char *new_addr;
  si32 *laddr;
  si32 imalloc;
  ui32 msize;
  int match_found;
  int Debug_level = 0;
  int true = 1;
  int false = 0;


  /*
   * return NULL if size is 0
   */

  if (size == 0) {
      return ((char *) NULL);
  }

  if (Debug_level > 1) {
    umalloc_verify();
  }

  if (user_addr == NULL) {

    /*
     * use malloc instead
     */

    addr = umalloc(size);
    return ((void *) addr);
    
  } /* if (match_found == false)  */
  
  addr = (char *) user_addr - Moffset;

      
  /*
   * compute size to preserve word boundary alignment
   */

  if (Debug_level > 0) {
    msize = ((((size + NBYTES_WORD * 2 - 1) >> Nshift) << Nshift) +
	     Nbytes_extra);
  } else {
    msize = size;
  }

  /*
   * reallocate
   */

  if ((new_addr = (char *) realloc(addr, msize)) == NULL) {
    fprintf(stderr, "ERROR - urealloc\n");
    fprintf(stderr, 
	    "Cannot perform realloc, addr, size = %p, %d\n",
	    addr, (int) size);
    exit(0);
  }

  user_addr = (void *) (new_addr + Moffset);


  /*
   * return offset pointer
   */

  return((void *) user_addr);

}


/*******************************************************************
 * alloc_check_block()
 *
 * check that the id at start and end of block is the same - 
 * if not, block has been corrupted.
 *
 ********************************************************************/

void alloc_check_block(char *addr, si32 *ret_start_id,
			      si32 *ret_end_id, ui32 *ret_size)

{

  si32 *start_id;
  si32 *end_id;
  si32 *msize;

  if (addr != NULL) {

    start_id = (si32 *) addr + 1;
    msize = start_id + 1;
    end_id = start_id + *msize / sizeof(si32) - 2;

    if (*start_id <= 0 || *end_id <= 0 ||
	*msize < 0 || *start_id != *end_id) {

	  
      fprintf(stderr, "ERROR - alloc_check_block\n");
      fprintf(stderr, "Malloc block corrupted\n");
      fprintf(stderr,
	      "%s %10p(%10p), %10d, %10d(%10d)\n",
	      "            : addr(user), id, size(user) =",
	      addr, addr + Moffset,
	      (int) *start_id,
	      (int) *msize, (int) (*msize - Nbytes_extra));
       exit(0);

    } /* if (*start_id != *end_id) */

    *ret_start_id = *start_id;
    *ret_end_id = *end_id;
    *ret_size = *msize;

   } /* if (addr != NULL) */

}
/*******************************************************************
 * ufree()
 *
 * normal free
 *
 *******************************************************************/

void (ufree)(void *user_addr)

{

  char *addr;
  si32 match_found;
  si32 start_id;
  si32 end_id;
  ui32 msize;
  si32 imalloc;
  int  Debug_level  = 0;
  int  false = 0;
  int  true = 1;

  if (user_addr == NULL) {

    /*
     * nothing to free
     */

    return;
    
  } /* if (match_found == false)  */

  /*
   * compute pointer to start of actual block
   */
  
  addr = (char *) user_addr - Moffset;
  
  /*
   * if not allocated under umalloc, free and return
   */

  if ((Debug_level == 0) ||
      (*(si32 *) addr != UMALLOC_CODE)) {
    free ((char *) user_addr);
    return;
  }

  /*
   * if debug level is 1+, check block integrity
   */
  
  if (Debug_level > 0) {
    alloc_check_block (addr, &start_id, &end_id, &msize);
  }
  
  /*
   * if debug level is 2+, set entry in address array to NULL
   */
  
  if (Debug_level > 1) {
    
    match_found = false;
    
    for (imalloc = 0; imalloc < Nmalloc; imalloc++) {
      
      if (addr == Malloc_addr[imalloc]) {
	
	if (Debug_level > 2) {
	  
	  fprintf(stderr,
		  "%s %10p(%10p), %10d, %10d(%10d)\n",
		  "ufree    : addr(user), id, size(user) =",
		  addr, addr + Moffset,
		  (int) start_id,
		  (int) msize, (int) msize - Nbytes_extra);
	  
	} /* if (Debug_level > 2) */
	
	Malloc_addr[imalloc] = (char *) NULL;
	
	match_found = true;
	
	break;
	
      } /* if (addr == Malloc_addr[imalloc]) */
      
    } /* imalloc */
    
    if (match_found == false) {
      
      fprintf(stderr, "ERROR - ufree\n");
      fprintf(stderr, "Trying to free block at addr %p.\n", addr);
      fprintf(stderr, "This block not currently allocated.\n");
      exit(0);      
      
    } /* if (match_found == false)  */
    
  } /* if (Debug_level > 1) */
  
  free((char *) addr);
  
}

/*******************************************************************/
void *(umalloc)(size_t size)
{
  ui32 msize;
  char *addr;

  if (size == 0) {
    return ((void *) NULL);
  }


  msize = size;
  if ((addr = (char *) malloc(msize)) == NULL) {
    fprintf(stderr, "ERROR - umalloc\n");
    fprintf(stderr, "Cannot perform malloc, size = %d\n", (int) size);
    exit(0);
  }

  return((void *) addr);
}
/********************************************************************/

void *(ucalloc)(size_t num, size_t size)
{

  ui32 csize;
  char *user_addr;

  /*
   * compute size in bytes
   */

  csize = num * size;

  /*
   * normal malloc
   */


  user_addr = umalloc(csize);

  /*
   * initialize area with zeros
   */

  memset ((void *) user_addr, 0, csize);

  return((void *) user_addr);

}

/*****************************************************************
 * MDV_RECALLOC: Allocs or reallocs depending on which one is 
 * necessary.   
 * Returns pointer with appropriate space. 
 * --Rachel Ames 3/96, RAP/NCAR
 */

void * MDV_recalloc(void * ptr_to_mem, int number_of_mem, int size_of_mem)
{ 
   if (ptr_to_mem == NULL) {
      if ((ptr_to_mem = calloc(number_of_mem, size_of_mem)) == NULL){
         fprintf(stderr,"\nError MDV_recalloc. Couldn't calloc pointer.\n");
         return (NULL);
      }
   }
   else {
      if ((ptr_to_mem = realloc(ptr_to_mem, number_of_mem*size_of_mem))==NULL) {
         fprintf(stderr,"\nError MDV_recalloc. Couldn't realloc pointer.\n");
         return (NULL);
      }
   }

   return(ptr_to_mem);
}

/*****************************************************************
 * MDV_DATA_ELEMENT_SIZE: Give the size (in bytes) of the data 
 * element given the encoding type integer.
 * Returns -1 if encoding type is MDV_NATIVE.
 * --Rachel Ames 3/96
 */

int MDV_data_element_size (int encoding_type)

{
   
 switch(encoding_type) {
    
  case MDV_INT8 :
    return(INT8_SIZE);
    
  case MDV_INT16 :
    return(INT16_SIZE);
    
  case MDV_FLOAT32 :
    return(FLOAT32_SIZE);
    
  default:
    return(INT8_SIZE);
  
}
}


/*****************************************************************
 * MDV_MASTER_HEADER_FROM_BE: Converts master header from big endian
 * format to native format.  Nancy Rehak 6/97
 */

void MDV_master_header_from_BE(MDV_master_header_t *m_hdr) 
{
  /* swap header si32 and fl32's */
  BE_to_array_32((ui32 *)(&m_hdr->record_len1),
		 MDV_NUM_MASTER_HEADER_32 * sizeof(si32));

  /* swap the last record length */
  m_hdr->record_len2 = BE_to_si32(m_hdr->record_len2);

  return;
}

 
/*****************************************************************
 * MDV_MASTER_HEADER_TO_BE: Converts master header from native
 * format to big endian format.  Nancy Rehak 6/97
 */

void MDV_master_header_to_BE(MDV_master_header_t *m_hdr) 
{
  /* swap header si32 and fl32's */
  BE_from_array_32((ui32 *)(&m_hdr->record_len1),
		   MDV_NUM_MASTER_HEADER_32 * sizeof(si32));

  /* swap the last record length */
  m_hdr->record_len2 = BE_from_si32(m_hdr->record_len2);

  return;
}

 
/*****************************************************************
 * MDV_FIELD_HEADER_FROM_BE: Converts field header from big endian
 * format to native format.  Nancy Rehak 6/97
 */

void MDV_field_header_from_BE(MDV_field_header_t *f_hdr)
{
  /* swap header si32 and fl32's */
  BE_to_array_32((ui32 *)(&f_hdr->record_len1),
		 MDV_NUM_FIELD_HEADER_32 * sizeof(si32));
 
  /* swap the last record length */
  f_hdr->record_len2 = BE_to_si32(f_hdr->record_len2);
 
  return;
}

/*****************************************************************
 * MDV_FIELD_HEADER_TO_BE: Converts field header from native
 * format to big endian format.  Nancy Rehak 6/97
 */

void MDV_field_header_to_BE(MDV_field_header_t *f_hdr)
{
  /* swap header si32 and fl32's */
/* *WHY* ???  */
  BE_from_array_32((ui32 *)(&f_hdr->record_len1),
		   MDV_NUM_FIELD_HEADER_32 * sizeof(si32));
 
  /* swap the last record length */
  f_hdr->record_len2 = BE_from_si32(f_hdr->record_len2);
 
  return;
}


/*****************************************************************
 * MDV_VLEVEL_HEADER_FROM_BE: Swaps vlevel header from big endian
 * format to native format.  Nancy Rehak 6/97
 */
 
void MDV_vlevel_header_from_BE(MDV_vlevel_header_t *v_hdr)
{
  /* swap header si32 and fl32's. No chars so everything done at once */
  BE_to_array_32((ui32 *)(&v_hdr->record_len1),
		 MDV_NUM_VLEVEL_HEADER_32 * sizeof(si32));
 
  /* swap the last record length */
  v_hdr->record_len2 = BE_to_si32(v_hdr->record_len2);
 
  return;
}


/*****************************************************************
 * MDV_VLEVEL_HEADER_TO_BE: Swaps vlevel header from native
 * format to big endian format.  Nancy Rehak 6/97
 */
 
void MDV_vlevel_header_to_BE(MDV_vlevel_header_t *v_hdr)
{
  /* swap header si32 and fl32's. No chars so everything done at once */
  BE_from_array_32((ui32 *)(&v_hdr->record_len1),
		   MDV_NUM_VLEVEL_HEADER_32 * sizeof(si32));
 
  /* swap the last record length */
  v_hdr->record_len2 = BE_from_si32(v_hdr->record_len2);
 
  return;
}


/*****************************************************************
 * MDV_FIELD_VLEVEL_HEADER_FROM_BE: Converts a field_vlevel header
 * from big endian format to native format.  Nancy Rehak 6/97
 */
 
void MDV_field_vlevel_header_from_BE(MDV_field_vlevel_header_t *fv_head)
{
  MDV_field_header_from_BE(fv_head->fld_hdr);

  if (fv_head->vlv_hdr != NULL)
    MDV_vlevel_header_from_BE(fv_head->vlv_hdr);

  return;
}


/*****************************************************************
 * MDV_FIELD_VLEVEL_HEADER_TO_BE: Converts a field_vlevel header
 * from native format to big endian format.  Nancy Rehak 6/97
 */
 
void MDV_field_vlevel_header_to_BE(MDV_field_vlevel_header_t *fv_head)
{
  MDV_field_header_to_BE(fv_head->fld_hdr);

  if (fv_head->vlv_hdr != NULL)
    MDV_vlevel_header_to_BE(fv_head->vlv_hdr);

  return;
}


/*****************************************************************
 * MDV_CHUNK_HEADER_FROM_BE: Converts a chunk header from big endian
 * format to native format.  Nancy Rehak 6/97
 */
 
void MDV_chunk_header_from_BE(MDV_chunk_header_t *c_hdr)
{
  /* swap header si32 and fl32's */
  BE_to_array_32((ui32 *)(&c_hdr->record_len1),
		 MDV_NUM_CHUNK_HEADER_32 * sizeof(si32));
 
  /* swap the last record length */
  c_hdr->record_len2 = BE_to_si32(c_hdr->record_len2);

  return;
}

/*****************************************************************
 * MDV_CHUNK_HEADER_TO_BE: Converts a chunk header from native
 * format to big endian format.  Nancy Rehak 6/97
 */
 
void MDV_chunk_header_to_BE(MDV_chunk_header_t *c_hdr)
{
  /* swap header si32 and fl32's */
  BE_from_array_32((ui32 *)(&c_hdr->record_len1),
		   MDV_NUM_CHUNK_HEADER_32 * sizeof(si32));
 
  /* swap the last record length */
  c_hdr->record_len2 = BE_from_si32(c_hdr->record_len2);

  return;
}

/*****************************************************************
 * MDV_UNENCODED_VOLUME_FROM_BE: Converts the data in an unencoded
 * data volume from big endian format to native format.  N. Rehak 6/97
 *
 * returns MDV_SUCCESS or MDV_FAILURE
 */
 
int MDV_unencoded_volume_from_BE(void *volume_data,
				 ui32 volume_size,
				 int data_type)
{
  char *routine_name = "MDV_unencoded_volume_from_BE";
  
  switch(data_type)
  {
  case MDV_INT8 :
    /* No data swapping necessary */
    break;
    
  case MDV_INT16 :
    BE_to_array_16((ui16 *)volume_data, volume_size);
    break;
    
  case MDV_FLOAT32 :
    BE_to_array_32((ui32 *)volume_data, volume_size);
    break;
    
  default:
    fprintf(stderr,
	    "%s: Do not know how to byte swap data in %s format\n",
	    routine_name, MDV_encode2string(data_type));
    return(MDV_FAILURE);
    
  } /* endswitch - data_type */

  return(MDV_SUCCESS);
}


/*****************************************************************
 * MDV_UNENCODED_VOLUME_TO_BE: Converts the data in an unencoded
 * data volume from native format to big endian format.  N. Rehak 6/97
 *
 * returns MDV_SUCCESS or MDV_FAILURE
 */
 
int MDV_unencoded_volume_to_BE(void *volume_data,
			       ui32 volume_size,
			       int data_type)
{
  char *routine_name = "MDV_unencoded_volume_to_BE";
  
  switch(data_type)
  {
  case MDV_INT8 :
    /* No data swapping necessary */
    break;
    
  case MDV_INT16 :
    BE_to_array_16((ui16 *)volume_data, volume_size);
    break;
    
  case MDV_FLOAT32 :
    BE_to_array_32((ui32 *)volume_data, volume_size);
    break;
    
  default:
    fprintf(stderr,
	    "%s: Do not know how to byte swap data in %s format\n",
	    routine_name, MDV_encode2string(data_type));
    return(MDV_FAILURE);
    
  } /* endswitch - data_type */

  return(MDV_SUCCESS);
}
/**********************************************************************
 * ta_compress.c
 *
 * Generic compression utilities.
 *
 * Mike Dixon, RAP, NCAR, P.O.Box 3000, Boulder, CO, USA
 *
 * July 1999
 *
 **********************************************************************/
int ta_compressed(void *compressed_buffer)
     
{

  ui32 magic_cookie;
  int  true = 1;
  int  false = 0;

  memcpy(&magic_cookie, compressed_buffer, sizeof(ui32));
  BE_to_array_32(&magic_cookie, sizeof(ui32));

  if (magic_cookie == LZO_COMPRESSED ||
      magic_cookie == BZIP_COMPRESSED ||
      magic_cookie == GZIP_COMPRESSED ||
      magic_cookie == RLE_COMPRESSED ||
      magic_cookie == _RLE_COMPRESSED ||
      magic_cookie == __RLE_COMPRESSED ||
      magic_cookie == ZLIB_COMPRESSED) {

    return (true);

  } else {

    return (false);

  }

}

/**********************************************************************
 * ta_compress()
 *
 * Compress according to the compression method.
 *
 * The memory for the encoded buffer is allocated by this routine,
 * and passed back to the caller.
 * This should be freed by the calling routine using ta_compress_free().
 *
 * The length of the compressed data buffer (*nbytes_compressed_p) is set.
 *
 * Returns pointer to the encoded buffer, NULL on error.
 *
 **********************************************************************/

void *ta_compress(ta_compression_method_t method,
		  void *uncompressed_buffer,
		  unsigned int nbytes_uncompressed,
		  unsigned int *nbytes_compressed_p)

{

  if(method ==  TA_COMPRESSION_NONE){
     return (_ta_no_compress(TA_NOT_COMPRESSED,
			    uncompressed_buffer,
			    nbytes_uncompressed,
			    nbytes_compressed_p));
  }
    
  else if (method == TA_COMPRESSION_RLE){
    return (rle_compress(uncompressed_buffer,
			 nbytes_uncompressed, nbytes_compressed_p));
  }
    
  else{
    fprintf(stderr, "ERROR - ta_compress\n");
    fprintf(stderr, " Cedric Unsupported compression method: %d\n", method);
    fprintf(stderr, "Cedric can only handle RLE compressed files at this time\n");
    return NULL;

  }

}

/**********************************************************************
 * ta_decompress() - toolsa generic decompression
 *
 * Perform generic decompression on buffer created using
 *   rle_compress(), lzo_compress() or bzip_compress().
 *
 * Switches on the magic cookie in the header.
 *
 * The memory for the uncompressed data buffer is allocated by this routine.
 * This should be freed by the calling routine using ta_decompress_free();
 *
 * On success, returns pointer to the uncompressed data buffer.
 * Also, *nbytes_uncompressed_p is set.
 *
 * On failure, returns NULL.
 *
 **********************************************************************/

void *ta_decompress(void *compressed_buffer,
		    unsigned int *nbytes_uncompressed_p)
     
{

  ui32 magic_cookie;

  memcpy(&magic_cookie, compressed_buffer, sizeof(ui32));
  BE_to_array_32(&magic_cookie, sizeof(ui32));

  if (magic_cookie == TA_NOT_COMPRESSED) {

    compress_buf_hdr_t hdr;
    char *uncompressed_data;
    char *compressed_data;
    
    memcpy(&hdr, compressed_buffer, sizeof(compress_buf_hdr_t));
    BE_to_array_32(&hdr, sizeof(compress_buf_hdr_t));
    
    uncompressed_data = (char *) umalloc (hdr.nbytes_uncompressed);
    *nbytes_uncompressed_p = hdr.nbytes_uncompressed;
    
    compressed_data = (char *) compressed_buffer + sizeof(compress_buf_hdr_t);
    memcpy(uncompressed_data, compressed_data, hdr.nbytes_uncompressed);
    return (uncompressed_data);

  } else if (magic_cookie == RLE_COMPRESSED ||
	     magic_cookie == _RLE_COMPRESSED ||
	     magic_cookie == __RLE_COMPRESSED) {
    
    /* fprintf(stderr, "RLE decompression\n"); */

    return (rle_decompress(compressed_buffer, nbytes_uncompressed_p));

  } 

  *nbytes_uncompressed_p = 0;
  return (NULL);

}

/**********************************************************************
 * ta_compress_free() - free up buffer allocated by any ta_compress
 * routines
 *
 */

void ta_compress_free(void *buffer)

{
  free(buffer);
}


/*****************************************************
 * _ta_no_compress()
 *
 * Load up compressed_buffer with uncompressed data
 *
 * returns output buffer of header plus original data
 *
 * private routine for use only by other compression routines.
 */

void *_ta_no_compress(unsigned int magic_cookie,
		      void *uncompressed_buffer,
		      unsigned int nbytes_uncompressed,
		      unsigned int *nbytes_compressed_p)
     
{
 
  unsigned int nbytes_buffer =
    nbytes_uncompressed + sizeof(compress_buf_hdr_t);
  void *out_buffer = umalloc(nbytes_buffer);
  compress_buf_hdr_t *hdr = (compress_buf_hdr_t *) out_buffer;
  
  /*
   * load header and swap
   */
  
  MEM_zero(*hdr);
  hdr->magic_cookie = magic_cookie;
  hdr->nbytes_uncompressed = nbytes_uncompressed;
  hdr->nbytes_compressed = nbytes_buffer;
  hdr->nbytes_coded = nbytes_uncompressed;
  BE_from_array_32(hdr, sizeof(compress_buf_hdr_t));
  
  if (nbytes_compressed_p != NULL) {
    *nbytes_compressed_p = nbytes_buffer;
  }
  
  /*
   * load buffer
   */
  
  memcpy((char *) out_buffer + sizeof(compress_buf_hdr_t),
	 uncompressed_buffer, nbytes_uncompressed);
  
  /*
   * swap header
   */
  
  return (out_buffer);

}


/**********************************************************************
 * rle_compress()
 *
 * In the compressed data, the first 20 bytes are a header as follows:
 *
 *   (ui32) Magic cookie - RL8_FLAG
 *   (ui32) key for compression
 *   (ui32) nbytes_buffer (nbytes_compressed + sizeof header)
 *   (ui32) nbytes_uncompressed
 *   (ui32) nbytes_compressed
 *
 * The header is in BE byte order.
 *
 * The compressed data follows the header.
 *
 * The memory for the encoded buffer is allocated by this routine,
 * and passed back to the caller.
 * This should be freed by the calling routine using ta_compress_free();
 *
 * The length of the compressed buffer (*nbytes_compressed_p) is set.
 * This length is for the header plus the compressed data.
 *
 * Returns pointer to the encoded buffer.
 *
 **********************************************************************/

void *rle_compress(void *uncompressed_buffer,
		   unsigned int nbytes_uncompressed,
		   unsigned int *nbytes_compressed_p)

{

  ui08 *compressed_buffer;
  ui32 nbytes_compressed;

  compressed_buffer = uRLEncode8(uncompressed_buffer,
				 nbytes_uncompressed,
				 255,
				 &nbytes_compressed);

  
  *nbytes_compressed_p = nbytes_compressed;
  return (compressed_buffer);

}
/**********************************************************************
 * rle_decompress()
 *
 * Perform RLE decompression on buffer created using rle_compress();
 *
 * The memory for the uncompressed data buffer is allocated by this routine.
 * This should be freed by the calling routine using ta_compress_free();
 *
 * On success, returns pointer to the uncompressed data buffer.
 * Also, *nbytes_uncompressed_p is set.
 *
 * On failure, returns NULL.
 *
 **********************************************************************/

void *rle_decompress(void *compressed_buffer,
		     unsigned int *nbytes_uncompressed_p)

{

  ui08 *uncompressed_buffer;
  ui32 nbytes_uncompressed;

  uncompressed_buffer = uRLDecode8(compressed_buffer,
				   &nbytes_uncompressed);

  *nbytes_uncompressed_p = nbytes_uncompressed;

  return (uncompressed_buffer);

}

/*****************************************************************
 * MDV_PLANE_TO_BE: Converts the data in a plane of data from big
 * endian format to native format.  N. Rehak 8/98
 *
 * returns MDV_SUCCESS or MDV_FAILURE
 */
 
int MDV_plane_from_BE(MDV_field_header_t *field_hdr, void *plane_ptr)
{
  static char *routine_name = "MDV_plane_from_BE";
  
  switch (field_hdr->encoding_type)
  {
  case MDV_INT8 :
    /*
     * No swapping necessary.
     */

    return(MDV_SUCCESS);
    
  case MDV_INT16 :
    BE_to_array_16(plane_ptr,
		   field_hdr->nx * field_hdr->ny * sizeof(si16));
    return(MDV_SUCCESS);
    
  case MDV_FLOAT32 :
    BE_to_array_32(plane_ptr,
		   field_hdr->nx * field_hdr->ny * sizeof(si32));
    return(MDV_SUCCESS);
    
  case MDV_PLANE_RLE8 :
    return(MDV_plane_rle8_from_BE(plane_ptr));
  
  } /* endswitch */
  
  fprintf(stderr, "ERROR: mdv:%s\n", routine_name);
  fprintf(stderr, "Invalid encoding type %d found in field header.\n",
	  field_hdr->encoding_type);
  
  return(MDV_FAILURE);
}


/*****************************************************************
 * MDV_PLANE_TO_BE: Converts the data in a plane of data from native
 * format to big endian format.  N. Rehak 8/98
 *
 * returns MDV_SUCCESS or MDV_FAILURE
 */
 
int MDV_plane_to_BE(MDV_field_header_t *field_hdr, void *plane_ptr)
{
  static char *routine_name = "MDV_plane_to_BE";
  
  switch (field_hdr->encoding_type)
  {
  case MDV_INT8 :
    /*
     * No swapping necessary.
     */

    return(MDV_SUCCESS);
    
  case MDV_INT16 :
    BE_from_array_16(plane_ptr,
		     field_hdr->nx * field_hdr->ny * sizeof(si16));
    return(MDV_SUCCESS);
    
  case MDV_FLOAT32 :
    BE_from_array_32(plane_ptr,
		     field_hdr->nx * field_hdr->ny * sizeof(si32));
    return(MDV_SUCCESS);
    
  case MDV_PLANE_RLE8 :
    return(MDV_plane_rle8_to_BE(plane_ptr));
  } /* endswitch */
  
  fprintf(stderr, "ERROR: mdv:%s\n", routine_name);
  fprintf(stderr, "Invalid encoding type %d found in field header.\n",
	  field_hdr->encoding_type);
  
  return(MDV_FAILURE);
}


/*****************************************************************
 * MDV_PLANE_RLE8_FROM_BE: Converts the data in a plane of data
 * encoded in the MDV_PLANE_RLE8 format from big endian format to
 * native format.  N. Rehak 6/97
 *
 * returns MDV_SUCCESS or MDV_FAILURE
 */
 
int MDV_plane_rle8_from_BE(void *plane_data)
{
  si32 *plane_ptr = (si32 *)plane_data;
  
  plane_ptr[0] = BE_to_si32(plane_ptr[0]);  /* RL8_FLAG */
  plane_ptr[1] = BE_to_si32(plane_ptr[1]);  /* key */
  plane_ptr[2] = BE_to_si32(plane_ptr[2]);  /* nbytes_array */
  plane_ptr[3] = BE_to_si32(plane_ptr[3]);  /* nbytes_full */
  plane_ptr[4] = BE_to_si32(plane_ptr[4]);  /* nbytes_coded */

  /* All of the rest of the data is byte data so doesn't need swapping */

  return(MDV_SUCCESS);
}


/*****************************************************************
 * MDV_PLANE_RLE8_TO_BE: Converts the data in a plane of data
 * encoded in the MDV_PLANE_RLE8 format from native format to
 * big endian format.  N. Rehak 6/97
 *
 * returns MDV_SUCCESS or MDV_FAILURE
 */
 
int MDV_plane_rle8_to_BE(void *plane_data)
{
  si32 *plane_ptr = (si32 *)plane_data;
  
  plane_ptr[0] = BE_from_si32(plane_ptr[0]);  /* RL8_FLAG */
  plane_ptr[1] = BE_from_si32(plane_ptr[1]);  /* key */
  plane_ptr[2] = BE_from_si32(plane_ptr[2]);  /* nbytes_array */
  plane_ptr[3] = BE_from_si32(plane_ptr[3]);  /* nbytes_full */
  plane_ptr[4] = BE_from_si32(plane_ptr[4]);  /* nbytes_coded */

  /* All of the rest of the data is byte data so doesn't need swapping */

  return(MDV_SUCCESS);
}


/*****************************************************************
 * MDV_ROW_RLE8_FROM_BE: Converts the data in a plane of data
 * encoded in the MDV_ROW_RLE8 format from big endian format to
 * native format.  N. Rehak 8/98
 *
 * returns MDV_SUCCESS or MDV_FAILURE
 */
 
int MDV_row_rle8_from_BE(void *plane_data, int num_rows)
{
  si32 *row_ptr = (si32 *)plane_data;
  int row;
  
  for (row = 0; row < num_rows; row++)
  {
    int row_size;
    
    row_ptr[0] = BE_to_si32(row_ptr[0]);  /* RL8_FLAG */
    row_ptr[1] = BE_to_si32(row_ptr[1]);  /* key */
    row_ptr[2] = BE_to_si32(row_ptr[2]);  /* nbytes_array */
    row_ptr[3] = BE_to_si32(row_ptr[3]);  /* nbytes_full */
    row_ptr[4] = BE_to_si32(row_ptr[4]);  /* nbytes_coded */

    row_size = row_ptr[2];
    
    /* All of the rest of the data is byte data so doesn't need swapping */

    row_ptr = (si32 *)((char *)row_ptr + row_size);
    
  }
  
  return(MDV_SUCCESS);
}


/*****************************************************************
 * MDV_ROW_RLE8_TO_BE: Converts the data in a plane of data
 * encoded in the MDV_ROW_RLE8 format from native format to big
 * endian format.  N. Rehak 8/98
 *
 * returns MDV_SUCCESS or MDV_FAILURE
 */
 
int MDV_row_rle8_to_BE(void *plane_data, int num_rows)
{
  si32 *row_ptr = (si32 *)plane_data;
  int row;
  
  for (row = 0; row < num_rows; row++)
  {
    int row_size;
    
    row_size = row_ptr[2];
    
    row_ptr[0] = BE_to_si32(row_ptr[0]);  /* RL8_FLAG */
    row_ptr[1] = BE_to_si32(row_ptr[1]);  /* key */
    row_ptr[2] = BE_to_si32(row_ptr[2]);  /* nbytes_array */
    row_ptr[3] = BE_to_si32(row_ptr[3]);  /* nbytes_full */
    row_ptr[4] = BE_to_si32(row_ptr[4]);  /* nbytes_coded */

    /* All of the rest of the data is byte data so doesn't need swapping */

    row_ptr = (si32 *)((char *)row_ptr + row_size);
    
  }
  
  return(MDV_SUCCESS);
}



/*****************************************************************
 * MDV_get_field_name: Returns pointer to field name - NULL on error
 *
 */

char *MDV_get_field_name(int field_code)
{
    if(field_code < 0 || field_code > MDV_MAX_FIELD_CODE) return (char *) NULL;
	/* Make sure static array is not screwed up */
	assert(field_code == mdv_field_code_info[field_code].code);
	return mdv_field_code_info[field_code].name;
}
 
/*****************************************************************
 * MDV_get_field_units: Returns pointer to field units - NULL on error
 *
 */

char *MDV_get_field_units(int field_code)
{
    if(field_code < 0 || field_code > MDV_MAX_FIELD_CODE) return (char *) NULL;
	/* Make sure static array is not screwed up */
	assert(field_code == mdv_field_code_info[field_code].code);
	return mdv_field_code_info[field_code].units;
}
 
/*****************************************************************
 * MDV_get_field_abbrev: Returns pointer to field abbrev - NULL on error
 *
 */

char *MDV_get_field_abbrev(int field_code)
{
    if(field_code < 0 || field_code > MDV_MAX_FIELD_CODE) return (char *) NULL;
	/* Make sure static array is not screwed up */
	assert(field_code == mdv_field_code_info[field_code].code);
	return mdv_field_code_info[field_code].abbrev;
}
 

/*****************************************************************
 * MDV_get_field_code_from_name()
 *
 * Returns field code for field name
 *
 * Returns code on success, -1 on failure (no match).
 */

int MDV_get_field_code_from_name(char *name)
{
  int ncodes;
  int i;
  
  ncodes = sizeof(mdv_field_code_info) / sizeof(mdv_field_code_t);

  for (i = 0; i < ncodes; i++) {
    if (!strcmp(name, mdv_field_code_info[i].name)) {
      return (i);
    }
  }
  return(-1);

}


/*****************************************************************
 * MDV_get_field_code_from_abbrev()
 *
 * Returns field code for field abbrev
 *
 * Returns code on success, -1 on failure (no match).
 */

int MDV_get_field_code_from_abbrev(char *abbrev)
{
  int ncodes;
  int i;
  
  ncodes = sizeof(mdv_field_code_info) / sizeof(mdv_field_code_t);

  for (i = 0; i < ncodes; i++) {
    if (!strcmp(abbrev, mdv_field_code_info[i].abbrev)) {
      return (i);
    }
  }
  return(-1);

}


/******************************************************************************
 * MDV_CALC_PLANE_SIZE: Calculates the number of bytes used to store the
 *                      data in the indicated plane.
 *
 * Returns the plane size on success, -1 on failure.
 */

int MDV_calc_plane_size(MDV_field_header_t *field_hdr, int plane_num,
			void *plane_ptr)
{
  static char *routine_name = "MDV_calc_plane_size";
  
  /*
   * Check for errors.
   */

  if (plane_num >= field_hdr->nz)
  {
    fprintf(stderr, "ERROR: mdv:%s\n", routine_name);
    fprintf(stderr, "Invalid plane number %d requested\n", plane_num);
    fprintf(stderr, "Field only has %d planes\n", field_hdr->nz);
    
    return(-1);
  }
  
  /*
   * Calculate the plane size.
   */

  if (!MDV_compressed(field_hdr->compression_type)) {

    /*
     * compute field size from grid dimension
     */
    
    return(field_hdr->nx * field_hdr->ny *
	   MDV_data_element_size(field_hdr->encoding_type));
    
  } else {

    /*
     * for compressed types, the compressed plane is preceded
     * by two ui32s: offset (always 0) and plane size.
     */

    si32 array_size = ((si32 *)plane_ptr)[1];
    return(array_size);

  }
}


/******************************************************************************
 * MDV_LOAD_PLANE: Loads the indicated plane from the given buffer.  The
 *                 buffer is expected to point to the beginning of the volume
 *                 data for the field.
 */

void *MDV_load_plane(void *buffer, MDV_field_header_t *fld_hdr, 
		     int return_type, int plane_num, int *plane_size)
{
  static char *routine_name = "MDV_load_plane";
  
  char *buf_ptr = (char *)buffer;
  
  void *plane_ptr;
  
  /*
   * Initialize return values
   */

  *plane_size = 0;
  
  /*
   * Do some sanity checking
   */

  if (fld_hdr == NULL)
  {
    fprintf(stderr,
	    "%s: Invalid pointers in parameter list.\n",
	    routine_name);
    return(NULL);
  }
  
  if (plane_num < 0 || (plane_num > fld_hdr->nz - 1))
  {
    fprintf(stderr,
	    "%s: Invalide plane number %d requested.\n",
	    routine_name, plane_num);
    return NULL;
  }
  
  /*
   * Skip the record size.
   */

  buf_ptr += sizeof(si32);
  
  /*
   * Load the data and convert it based on the encoding type.
   */

  switch(fld_hdr->encoding_type)
  {
  case MDV_INT8 :
    plane_ptr = load_plane_int8(buf_ptr, fld_hdr,
				return_type, plane_num, plane_size);
    break;
    
  case MDV_PLANE_RLE8 :
    plane_ptr = load_plane_plane_rle8(buf_ptr, fld_hdr,
				      return_type, plane_num, plane_size);
    break;
    
    
  default:
    fprintf(stderr,
	    "%s: Cannot load plane in %s format -- not yet implemented\n",
	    routine_name, MDV_encode2string(fld_hdr->encoding_type));
    return NULL;
    break;
  } /* endswitch - fld_hdr->encoding_type */
  
  return(plane_ptr);
}


/******************************************************************************
 * MDV_CALC_BUFFER_SIZE: Calculates the size of a flat buffer needed to
 *                       store this MDV data.
 */

int MDV_calc_buffer_size(MDV_handle_t *mdv)
{
  int buffer_size = 0;
  
  int n_fields = mdv->master_hdr.n_fields;
  int n_chunks = mdv->master_hdr.n_chunks;
  
  int i;
  
  /*
   * Add the master header.
   */

  buffer_size += sizeof(MDV_master_header_t);
  
  /*
   * Add the field headers.
   */

  buffer_size += n_fields * sizeof(MDV_field_header_t);
  
  /*
   * Add the vlevel headers.
   */

  if (mdv->master_hdr.vlevel_included)
    buffer_size += n_fields * sizeof(MDV_vlevel_header_t);
  
  /*
   * Add the chunk headers.
   */

  buffer_size += n_chunks * sizeof(MDV_chunk_header_t);
  
  /*
   * Add the field data.  Note that the FORTRAN record lengths are
   * not included in the volume size.
   */

  for (i = 0; i < n_fields; i++)
    buffer_size += mdv->fld_hdrs[i].volume_size + (2 * sizeof(si32));
  
  /*
   * Add the chunk data.  Note that the FORTRAN record lengths are
   * not included in the chunk size.
   */

  for (i = 0; i < n_chunks; i++)
    buffer_size += mdv->chunk_hdrs[i].size + (2 * sizeof(si32));
      
  return(buffer_size);
}


/********************************************************
 * STATIC ROUTINES
 ********************************************************/

/********************************************************
 * LOAD_PLANE_INT8
 * Routine for loading a plane of data in MDV_INT8 format
 * from the given buffer and returning it in the specified
 * format. The buffer pointer is assumed to point to the
 * beginning of the volume data, after the FORTRAN record
 * length. All necessary byte swapping is done in this routine.
 *
 * This routine assumes that the plane number is valid for
 * this data.
 *
 * Returns pointer to data buffer or NULL on error.  Note
 * that the calling routine must free the data pointer
 * returned.
 */

static void *load_plane_int8(void *buffer,
			     MDV_field_header_t *field_hdr,
			     int data_type,
			     int plane_num,
			     int *return_size)
{
  static char *routine_name = "load_plane_int8";
  
  char *buf_ptr;
  
  void *return_buf;
  
  int plane_size;
  int plane_loc;
  
  /*
   * Compute size of returned plane.
   */

  plane_size = field_hdr->nx * field_hdr->ny * sizeof(ui08);

  /*
   * Compute where the data is located in the buffer.
   */

  plane_loc = plane_num * plane_size;
    
  /*
   * Move to the data position.
   */

  buf_ptr = (char *)buffer + plane_loc;
  
  /*
   * No swapping necessary - byte data
   */

  /*
   * Convert the data to the desired return type.
   */

  switch(data_type)
  {
  case MDV_INT8 :
    return_buf = malloc(plane_size);
    memcpy(return_buf, buf_ptr, plane_size);
    *return_size = plane_size;
    break;
    
  default:
    fprintf(stderr,
	    "%s: Cannot convert %s plane to %s format -- not yet implemented\n",
	    routine_name, MDV_encode2string(MDV_INT8),
	    MDV_encode2string(data_type));
    *return_size = 0;
    return(NULL);
  } /* endswitch - data_type */
  
  return(return_buf);
  
} /* end read_plane_int8 */


/********************************************************
 * LOAD_PLANE_PLANE_RLE8
 * Routine for loading a plane of data in MDV_PLANE_RLE8
 * format from the given buffer and returning it in the
 * specified format. The buffer pointer is assumed to point
 * to the beginning of the volume data, after the FORTRAN
 * record length. All necessary byte swapping is done in
 * this routine.
 *
 * This routine assumes that the plane number is valid for
 * this data.
 *
 * Returns pointer to data buffer or NULL on error.  Note
 * that the calling routine must free the data pointer
 * returned.
 */

static void *load_plane_plane_rle8(void *buffer,
				   MDV_field_header_t *field_hdr,
				   int data_type,
				   int plane_num,
				   int *return_size)
{
  static char *routine_name = "load_plane_plane_rle8";
  
  char *buf_ptr = (char *)buffer;
  
  void *return_buf;
  void *encode_buf;

  si32 *vlevel_locs;
  si32 *vlevel_sizes;
  int array_size = field_hdr->nz * sizeof(si32);
  ui32 nbytes;
  si32 *lptr;
  ui08 *uRLDecode8();
  
  /*
   * Allocate memory for location array
   */

  if ((vlevel_locs = (si32 *)malloc(array_size)) == NULL)
  {
    fprintf(stderr,
	    "%s: Error allocating %d bytes for vlevel_locs array\n",
	    routine_name, array_size);
    return(NULL);
  }

  /*
   * Allocate memory for size array
   */

  if ((vlevel_sizes = (si32 *)malloc(array_size)) == NULL)
  {
    fprintf(stderr,
	    "%s: Error allocating %d bytes for vlevel_sizes array\n",
	    routine_name, array_size);
    free(vlevel_locs);
    return(NULL);
  }

  /*
   * Load the location information.
   */
  
  memcpy(vlevel_locs, buf_ptr, array_size);
  buf_ptr += array_size;
  
  /*
   * Load the size information.
   */

  memcpy(vlevel_sizes, buf_ptr, array_size);
  buf_ptr += array_size;
  
  /*
   * Swap the plane information, if necessary.
   */

  BE_to_array_32(vlevel_locs, array_size);
  BE_to_array_32(vlevel_sizes, array_size);
  
  /*
   * Go to beginning of this plane data.
   */

  encode_buf = buf_ptr + vlevel_locs[plane_num];
  
  /*
   * Swap the encoded plane.
   */

  lptr = encode_buf;
    
  lptr[0] = BE_to_si32(lptr[0]);   /* RL8_FLAG */
  lptr[1] = BE_to_si32(lptr[1]);   /* key */
  lptr[2] = BE_to_si32(lptr[2]);   /* nbytes_array */
  lptr[3] = BE_to_si32(lptr[3]);   /* nbytes_full */
  lptr[4] = BE_to_si32(lptr[4]);   /* nbytes_coded */
  
  /*
   * Put the data into the desired format.
   */

  switch(data_type)
  {
  case MDV_INT8:
    /*
     * Decode this plane of data
     */

    return_buf = (void *)uRLDecode8(encode_buf, &nbytes);

    if (return_buf == NULL ||
	nbytes != field_hdr->nx * field_hdr->ny * INT8_SIZE)
    {
      fprintf(stderr,
	      "%s: Error decoding buffer into %s format\n",
	      routine_name, MDV_encode2string(MDV_INT8));

      if (return_buf != NULL)
      {
	free(return_buf);
	*return_size = 0;
	return_buf = NULL;
      }
    }
    
    *return_size = nbytes;
    break;
    
  case MDV_PLANE_RLE8 :
    return_buf = encode_buf;
    *return_size = lptr[2];
    break;
    
  default:
    fprintf(stderr,
	    "%s: Cannot convert %s data into %s format -- not yet implemented\n",
	    routine_name, MDV_encode2string(MDV_PLANE_RLE8),
	    MDV_encode2string(data_type));
    free(vlevel_locs);
    free(vlevel_sizes);
    *return_size = 0;
    return(NULL);
    break;
  } /* endswitch - data_type */
  
  /*
   * Clean up
   */

  free(vlevel_locs);
  free(vlevel_sizes);

  return(return_buf);
  
} /* end load_plane_plane_rle8 */


/**********************************************************************
 * uRLEncode8() - performs run-length encoding on byte data which uses
 *                all 8 bits
 *
 * In the coded data, the first 20 bytes are as follows:
 *
 * (si32) Magic cookie - RL8_FLAG
 * (si32) key
 * (si32) nbytes_array
 * (si32) nbytes_full
 * (si32) nbytes_coded
 *
 * The coded data follows these 20 bytes. The coded data is padded out
 * to end on a 4-byte boundary.
 *
 * The memory for the encoded array is allocated by this routine.
 *
 * Returns pointer to the encoded array. The number of bytes in the
 * encodeded data array is returned via nbytes_array.
 *
 * utility routine
 *
 * N. Rehak RAP, NCAR, Boulder CO 5 Oct 1990
 *
 * hacked from uRLEncode.c by Mike Dixon
 *
 **********************************************************************/

ui08 *uRLEncode8(ui08 *full_data,
		 ui32 nbytes_full,
		 ui32 key,
		 ui32 *nbytes_array)
     
{

  register ui08 byteval;
  register ui08 *coded_data;
  register ui08 *fdata, *cdata, *end;

  register ui32 runcount;

  si32 *lptr;

  ui32 nbytes_coded;
  ui32 nbytes_extra;
  ui32 nbytes_alloc;
  ui32 nbytes_unpadded;

  int i;

  /*
   * full_data is original array
   * fdata is pointer into original array
   * cdata is pointer into coded array
   * end is pointer to last byte in original array
   */

  /*
   * initial allocation of encoded array, the size of the original array
   * plus the extra bytes at the start for the nbyte values, plus enough
   * bytes to pass a word boundary. Twice this will be sufficient for the
   * worst case in which the pattern is
   *    key non-key key non-key key ...
   * which expands to twice the original size.
   */

  nbytes_extra = RL8_NBYTES_EXTRA;
  nbytes_alloc = 2 * (nbytes_full + nbytes_extra + NBYTES_WORD);
  coded_data = calloc (nbytes_alloc, 1);

  /*
   * set the number of bytes in the full data, and the pointer to the
   * number of encoded bytes
   */

  /*
   * set pointers to data arrays
   */

  fdata = full_data;
  cdata = coded_data + nbytes_extra;
  
  /*
   * set pointer to last data byte
   */

  end = fdata + nbytes_full;

  if (full_data != NULL) {

    while (fdata < end) {

      /*
       * get byte value
       */

      byteval = *fdata;
      runcount = 1;
      fdata++;
      
      while ((fdata < end) &&
	     (runcount < 255) &&
	     (*fdata == byteval)) {
	
	/*
	 * count up adjacent bytes of same value
	 */

	fdata++;
	runcount++;
	
      }

      if (runcount <= 3 && byteval != key) {
	
	/*
	 * count <= 3, and byteval is not key,
	 * so store as single bytes because there is no
	 * advantage to RLE
	 */
	
	for (i = 0; i < runcount; i++) {
	  *cdata = byteval;
	  cdata++;
	}
	
      } else {

	/*
	 *  count > 3, store as RLE indicator, count then byte value
	 */

	*cdata = key;
	*(cdata + 1) = runcount;
	*(cdata + 2) = byteval;
	cdata += 3;

      }

    } /* while (fdata < end) */

    /*
     * compute the number of bytes in the encoded data, including the 
     * leading 8 bytes and the padding to go to a word boundary
     */

    nbytes_coded = cdata - coded_data - nbytes_extra;
    nbytes_unpadded = nbytes_coded + nbytes_extra;
    *nbytes_array =
      ((ui32) ((nbytes_unpadded - 1) / NBYTES_WORD) + 1) * NBYTES_WORD;

    /*
     * realloc the coded_data array
     */

    coded_data = (ui08 *) realloc
      ((char *) coded_data, (ui32) *nbytes_array);

    /*
     * set the bytes counts
     */

    lptr = (si32 *) coded_data;

    *lptr = BE_from_si32(RL8_FLAG);
    lptr++;
    *lptr = BE_from_si32(key);
    lptr++;
    *lptr = BE_from_si32(*nbytes_array);
    lptr++;
    *lptr = BE_from_si32(nbytes_full);
    lptr++;
    *lptr = BE_from_si32(nbytes_coded);

    return (coded_data);

  } else {
    return ((ui08 *) NULL);

  } /* if (full_data != NULL) */

}

/**********************************************************************
 * uRLDecode8() - performs run-length decoding on byte data which was
 *                compressed using uRLEncode8
 *
 * Returns the full data array. The size of the array is passed back via
 * nbytes_full.
 *
 * utility routine
 *
 * N. Rehak RAP, NCAR, Boulder CO 6 October 1992
 *
 * hacked from "uRLDecode.c" by Mike Dixon
 *
 **********************************************************************/

ui08  *uRLDecode8(ui08 *coded_data,ui32 *nbytes_full)
     
{

  register int runcount;
  int swap;
  int true = 1;
  int false = 0;
  
  si32 *lptr;
  si32 compress_flag;
  si32 key;

  ui32 nbytes_coded;
  ui32 nbytes_extra;

  register ui08 byteval;
  ui08 *full_data;
  register ui08 *end;
  register ui08 *fdata, *cdata;

  if (coded_data != NULL) {

    lptr = (si32 *) coded_data;
    compress_flag = *lptr;
    lptr++;

    if (compress_flag == RL8_FLAG ||
	compress_flag == MDV_RL8_FLAG) {
      swap = false;
    } else if (BE_to_si32(compress_flag) == RL8_FLAG ||
	       BE_to_si32(compress_flag) == MDV_RL8_FLAG) {
      swap = true;
    } else {
      /*
       * not compressed with this alg, return NULL
       */
      return ((ui08 *) NULL);
    }

    /*
     * get number of bytes for the coded and full data
     */

    if (swap) {
      key = BE_to_si32(*lptr);
    } else {
      key = *lptr;
    }

    lptr += 2;
    if (swap) {
      *nbytes_full = BE_to_si32(*lptr);
    } else {
      *nbytes_full = *lptr;
    }

    lptr++;
    if (swap) {
      nbytes_coded = BE_to_si32(*lptr);
    } else {
      nbytes_coded = *lptr;
    }

    nbytes_extra = RL8_NBYTES_EXTRA;

    /*
     * get space for full data
     */

    full_data = calloc(*nbytes_full, 1);
    
    fdata = full_data;
    cdata = coded_data + nbytes_extra;

    end = cdata + nbytes_coded;

    while (cdata < end) {

      byteval = *cdata;

      if (byteval == key) {

	/*
	 * if RLE flag value, the next byte contains the run count and
	 * the byte after contains the byte value
	 */

	cdata ++;
	runcount = *cdata;
	cdata++;
	byteval  = *cdata;

	/*
	 * set runcount values
	 */

	memset((char *) fdata, (int) byteval, (int) runcount);
	fdata += runcount;

      } else {

	/*
	 * if not RLE flag value, set single byte
	 */

	*fdata = byteval;
	fdata++;

      } /* if (byteval == key) */

      cdata++;

    } /* while (cdata < end) */

    return (full_data);

  } else {

    return ((ui08 *) NULL);

  }

}

/**********************************************************************
 * uRLCheck()
 *
 * Checks for compression type, and number of bytes in compressed
 * array.
 *
 * Returns 0 on success, -1 on error
 *
 * utility routine
 *
 * Mike Dixon, RAP, NCAR, Boulder CO
 *
 * Feb 1994
 *
 **********************************************************************/

int uRLCheck(ui08 *coded_data,
	     ui32 nbytes_passed,
	     int *eight_bit,
	     ui32 *nbytes_compressed)
     
{

  ui32 *lptr;
  ui32 first_int, third_int;
  ui32 compress_flag;
  int  true = 1;
  int  false = 0;
  
  if (coded_data == NULL)
    return (-1);
  
  if (nbytes_passed < sizeof(ui32))
    return (-1);
  
  lptr = (ui32 *) coded_data;
  first_int = BE_to_si32(*lptr);

  compress_flag = first_int;

  if (compress_flag == RL8_FLAG ||
      compress_flag == MDV_RL8_FLAG) {
    
    *eight_bit = true;

    if (nbytes_passed < 3 * sizeof(ui32))
      return (-1);
  
    lptr += 2;
    third_int = BE_to_si32(*lptr);
    *nbytes_compressed = third_int;

  } else {

    *eight_bit = false;
    *nbytes_compressed = first_int;

  }

  return (0);

}

/**********************************************************************
 * uRLEncode() - performs run-length encoding on byte data which uses
 *               only the lower 7 bits
 *
 * In the coded data, the first 12 bytes are as follows:
 *
 * (si32) nbytes_array, (si32) nbytes_full, (si32) nbytes_coded.
 *
 * The coded data follows these 12 bytes. The coded data is padded out
 * to end on a 4-byte boundary.
 *
 * The memory for the encoded array is allocated by this routine.
 *
 * Returns pointer to the encoded array. The number of bytes in the
 * encodeded data array is returned via nbytes_array.
 *
 * utility routine
 *
 * Mike Dixon RAP, NCAR, Boulder CO November 1990
 *
 **********************************************************************/

ui08 *uRLEncode(ui08 *full_data, ui32 nbytes_full,
		ui32 *nbytes_array)
     
{

  register ui08 byteval;
  register ui08 *coded_data;
  register ui08 *fdata, *cdata, *end;

  register ui32 runcount;

  ui32 nbytes_coded;
  ui32 nbytes_extra;
  ui32 nbytes_unpadded;

  /*
   * full_data is original array
   * fdata is pointer into original array
   * cdata is pointer into coded array
   * end is pointer to last byte in original array
   */

  /*
   * initial allocation of encoded array, the size of the original array
   * plus the extra bytes at the start for the nbyte values, plus enough
   * bytes to pass a word boundary. This will be sufficient for the
   * worst case in which there is no compression
   */

  nbytes_extra = RL7_NBYTES_EXTRA;
  
  coded_data = (ui08 *) malloc
    ((ui32) (nbytes_full + nbytes_extra + NBYTES_WORD));

  /*
   * set the number of bytes in the full data, and the pointer to the
   * number of encoded bytes
   */

  /*
   * set pointers to data arrays
   */

  fdata = full_data;
  cdata = coded_data + nbytes_extra;
  
  /*
   * set pointer to last data byte
   */

  end = full_data + nbytes_full;

  if (full_data != NULL) {

    while (fdata < end) {

      /*
       * get byte value
       */

      byteval = *fdata;
      fdata++;

      /*
       * return with NULL pointer if data exceeds 127
       */

      if (byteval > 127) {

	fprintf(stderr, "ERROR - uRLEncode\n");
	fprintf(stderr, "Byte value exceeds 127.\n");
	free(coded_data);
	return ((ui08 *) NULL);

      } /* if (byteval .... */
  
      runcount = 1;
      
      while ((fdata < end) &&
	     (runcount < 127) &&
	     (*fdata == byteval)) {

	/*
	 * count up adjacent bytes of same value
	 */

	fdata++;
	runcount++;
	
      }

      if (runcount == 1) {

	/*
	 * count = 1, store as single byte
	 */

	*cdata = byteval;
	cdata++;

      } else {

	/*
	 *  count > 1, store as count then byte value
	 */

	*cdata = 0x80 | runcount;
	*(cdata + 1) = byteval;
	cdata += 2;

      }

    } /* while (fdata < end) */

    /*
     * compute the number of bytes in the encoded data, including the 
     * leading 8 bytes and the padding to go to a word boundary
     */

    nbytes_coded = cdata - coded_data - nbytes_extra;
    nbytes_unpadded = nbytes_coded + nbytes_extra;
    *nbytes_array =
      ((ui32) ((nbytes_unpadded - 1) / NBYTES_WORD) + 1) * NBYTES_WORD;

    /*
     * realloc the coded_data array
     */

    coded_data = (ui08 *) realloc
      ((char *) coded_data, (ui32) *nbytes_array);

    /*
     * set the bytes counts
     */

    *((si32 *) coded_data) = BE_from_si32(*nbytes_array);
    *((si32 *) coded_data + 1) = BE_from_si32(nbytes_full);
    *((si32 *) coded_data + 2) = BE_from_si32(nbytes_coded);

    return (coded_data);

  } else {

    return ((ui08 *) NULL);

  } /* if (full_data != NULL) */

}

/**********************************************************************
 * uRLDecode() - performs run-length decoding on byte data which was
 *               compressed using uRLEncode
 *
 * Returns the full data array. The size of the array is passed back via
 * nbytes_full.
 *
 * utility routine
 *
 * Mike Dixon RAP, NCAR, Boulder CO November 1990
 *
 **********************************************************************/

ui08 *uRLDecode(ui08 *coded_data, ui32 *nbytes_full)
     
{

  register int runcount;
  ui32 nbytes_coded;
  ui32 nbytes_extra;

  register ui08 byteval;
  ui08 *full_data;
  register ui08 *end;
  register ui08 *fdata, *cdata;

  if (coded_data != NULL) {

    /*
     * get number of bytes for the coded and full data
     */

    *nbytes_full = BE_to_si32(*((si32 *) coded_data + 1));
    nbytes_coded = BE_to_si32(*((si32 *) coded_data + 2));
    nbytes_extra = RL7_NBYTES_EXTRA;

    /*
     * get space for full data
     */

    full_data = (ui08 *) malloc(*nbytes_full);
    
    fdata = full_data;
    cdata = coded_data + nbytes_extra;

    end = cdata + nbytes_coded;

    while (cdata < end) {

      byteval = *cdata;

      if ((byteval & 0x80) == 0x80) {

	/*
	 * if most significant bit set, mask off lower 7 bits and
	 * use as the count on the next byte value
	 */

	runcount = byteval & 0x7f;
	cdata++;
	byteval  = *cdata;

	/*
	 * set runcount values
	 */

	memset((char *) fdata, (int) byteval, (int) runcount);
	fdata += runcount;

      } else {

	/*
	 * if most significant bit not set, set single byte
	 */

	*fdata = byteval;
	fdata++;

      } /* if ((byteval & 0x80) == 0x80) */

      cdata++;

    } /* while (cdata < end) */

    return (full_data);

  } else {

    return ((ui08 *) NULL);

  }

}

static int BigEnd = 1;

/***********************************************
 * BE_reverse()
 *
 * Reverses the sense of this library. Therefore,
 * is called once, SmallEndian values are set.
 * If called twice, goes back to BigEndian.
 */

void BE_reverse(void)

{
  BigEnd = !BigEnd;
}

/************************************************
 * Return 1 if host is big_endian and 0 otherwise
 *
 * For debugging, if FORCE_SWAP is set, this routine will
 * always return FALSE, forcing a swap.
 */

int
BE_is_big_endian()
{
  
#ifdef FORCE_SWAP

  return (0);

#else

  union 
    {
      ui16    d;
      ui08     bytes[2];
    }
  short_int;

  short_int.d = 1;
  if (short_int.bytes[1] != 0)
    return (BigEnd);
  else
    return (!BigEnd);

#endif

}

/**********************************************************************
 * BE_swap_array_32()
 *
 * Performs an in-place 32-bit word byte swap, if necessary, to produce
 * BE representation from machine representation, or vice-versa.
 *
 * Array must be aligned.
 *
 * Returns the number of bytes converted.
 *
 */

si32
BE_swap_array_32(void *array, ui32 nbytes)
     
{

  static int big_endian;
  static int one_time = 0;
  ui32 i, l, nlongs;
  ui32 *this_long;
  ui32 *array32 = array;

  /* check for little or big endian */
  if (one_time == 0) {
    big_endian = BE_is_big_endian ();
    one_time = 1;
  }
  
  if (big_endian) {
    return (0);
  }
  
  nlongs = nbytes / sizeof(ui32);
  this_long = array32;
  
  for (i = 0; i < nlongs; i++) {

    l = *this_long;
    
    *this_long = (((l & 0xff000000) >> 24) |
		  ((l & 0x00ff0000) >> 8) |
		  ((l & 0x0000ff00) << 8) |
		  ((l & 0x000000ff) << 24));
    
    this_long++;

  }

  return (nbytes);

}

/**********************************************************************
 * BE_swap_array_16()
 *
 * Performs an in-place 16-bit word byte swap, if necessary, to produce
 * BE representation from machine representation, or vice-versa.
 *
 * Array must be aligned.
 *
 * Returns the number of bytes converted.
 *
 */

si32
BE_swap_array_16(void *array, ui32 nbytes)
     
{

  static int big_endian;
  static int one_time = 0;
  ui32 i, l, nlongs, nshorts;
  ui32 *this_long;
  ui16 *array16 = array;
  ui16 s;

  /* check for little or big endian */
  if (one_time == 0) {
    big_endian = BE_is_big_endian ();
    one_time = 1;
  }
  
  if (big_endian) {
    return (0);
  }
  
  nlongs = nbytes / sizeof(ui32);
  this_long = (ui32 *)array16;

  for (i = 0; i < nlongs; i++) {
    
    l = *this_long;
    
    *this_long = (((l & 0xff000000) >> 8) |
		   ((l & 0x00ff0000) << 8) |
		   ((l & 0x0000ff00) >> 8) |
		   ((l & 0x000000ff) << 8));

    this_long++;
  }
  
  if (nlongs * sizeof(ui32) != nbytes) {
    nshorts = nbytes / sizeof(ui16);
    s = array16[nshorts-1];
    array16[nshorts-1]= (((s & 0xff00) >> 8) | ((s & 0x00ff) << 8));
  }

  return (nbytes);
  
}

/*****************************************************
 * the to- and from- routines are identical,
 * and may be implemented in terms of BE_swap routines
 *
 * Later we may duplicate the code for effiency, to
 * avoid the calling overhead.
 */

/********************
 * BE_from_array_32()
 * Converts an array of 32s
 */

si32 BE_from_array_32(void *array, ui32 nbytes)

{
  return (BE_swap_array_32(array, nbytes));
}
     
/******************
 * BE_to_array_32()
 * Converts an array of 32s
 */

si32 BE_to_array_32(void *array, ui32 nbytes)

{
  return (BE_swap_array_32(array, nbytes));
}
     
/********************
 * BE_from_array_16()
 * Converts an array of 16s
 */

si32 BE_from_array_16(void *array, ui32 nbytes)

{
  return (BE_swap_array_16(array, nbytes));
}
     
/******************
 * BE_to_array_16()
 * Converts an array of 16s
 */

si32 BE_to_array_16(void *array, ui32 nbytes)

{
  return (BE_swap_array_16(array, nbytes));
}

/********************
 * BE_from_fl32()           
 * Converts a single fl32
 */                   

void BE_from_fl32(fl32 *dst, fl32 *src)                       
 
{
  memcpy( dst, src, sizeof(fl32) );
  BE_swap_array_32(dst, sizeof(fl32));   
}
 
/******************************
 *  BE_to_fl32       
 *  Converts a single fl32
 */

void BE_to_fl32(fl32 *src, fl32 *dst)

{
  memcpy( dst, src, sizeof(fl32) );
  BE_swap_array_32(dst, sizeof(fl32));
}

/********************************
 *  BE_from_si32 replaces htonl()
 *  Converts a single si32
 */

si32 BE_from_si32(si32 x)

{
  BE_swap_array_32((void *) &x, sizeof(si32));
  return (x);
}    

/******************************
 *  BE_to_si32 replaces ntohl()
 *  Converts a single si32
 */

si32 BE_to_si32(si32 x)

{
  BE_swap_array_32(&x, sizeof(si32));
  return (x);
}    

/********************************
 *  BE_from_si16 replaces htons()
 *  Converts a single si16
 */

si16 BE_from_si16(si16 x)

{
  BE_swap_array_16(&x, sizeof(si16));
  return (x);
}    

/******************************
 *  BE_to_si16 replaces ntohs()
 *  Converts a single si16
 */

si16 BE_to_si16(si16 x)

{
  BE_swap_array_16(&x, sizeof(si16));
  return (x);
}    

/********************************
 *  BE_from_ui32
 *  Converts a single ui32
 */

ui32 BE_from_ui32(ui32 x)

{
  BE_swap_array_32(&x, sizeof(ui32));
  return (x);
}    

/******************************
 *  BE_to_ui32
 *  Converts a single ui32
 */

ui32 BE_to_ui32(ui32 x)

{
  BE_swap_array_32(&x, sizeof(ui32));
  return (x);
}    

/********************************
 *  BE_from_ui16
 *  Converts a single ui16
 */

ui16 BE_from_ui16(ui16 x)

{
  BE_swap_array_16(&x, sizeof(ui16));
  return (x);
}    

/******************************
 *  BE_to_ui16
 *  Converts a single ui16
 */

ui16 BE_to_ui16(ui16 x)

{
  BE_swap_array_16(&x, sizeof(ui16));
  return (x);
}    
/*****************************************************************
 * MDV_ORDER2STRING: Convert the data order integer to an
 * ascii string.  See mdv_macros for the data order declarations.
 * --Nancy Rehak 4/96
 */

char * MDV_order2string(int order_type)
{
  switch(order_type)
  {
  case MDV_ORDER_XYZ :
    return("MDV_ORDER_XYZ");
  case MDV_ORDER_YXZ :
    return("MDV_ORDER_YXZ");
  case MDV_ORDER_XZY :
    return("MDV_ORDER_XZY");
  case MDV_ORDER_YZX :
    return("MDV_ORDER_YZX");
  case MDV_ORDER_ZXY :
    return("MDV_ORDER_ZXY");
  case MDV_ORDER_ZYX :
    return("MDV_ORDER_ZYX");
  default:
    return("Unknown Data Order");
  }
}

/*****************************************************************
 * MDV_COLLTYPE2STRING: Convert the collection type integer to an
 * ascii string.  See mdv_macros for the vertical type declarations.
 * --Rachel Ames 1/96
 */

char * MDV_colltype2string(int coll_type)
{

   switch(coll_type)  {
      case MDV_DATA_MEASURED :
        return("Measured");
      case MDV_DATA_EXTRAPOLATED :
	return("Extrapolated");
      case MDV_DATA_FORECAST :
	return("Forecast");
      case MDV_DATA_SYNTHESIS :
	return("Synthesis");
      case MDV_DATA_MIXED :
	return("Mixed");
      default:
	return("Unknown Collection Type");
   }
}

/*****************************************************************
 * MDV_ORIENT2STRING: Convert the data orientation integer to an
 * ascii string.  See mdv_macros for the orientation type declarations.
 * --Nancy Rehak 4/96
 */

char * MDV_orient2string(int orient_type)
{
  switch(orient_type)
  {
  case MDV_ORIENT_OTHER :
    return("MDV_ORIENT_OTHER");
  case MDV_ORIENT_SN_WE :
    return("MDV_ORIENT_SN_WE");
  case MDV_ORIENT_NS_WE :
    return("MDV_ORIENT_NS_WE");
  case MDV_ORIENT_SN_EW :
    return("MDV_ORIENT_SN_EW");
  case MDV_ORIENT_NS_EW :
    return("MDV_ORIENT_NS_EW");
  default:
    return("Unknown Orientation");
  }
}

/*****************************************************************
 * MDV_VERTTYPE2STRING: Convert the vertical type integer to ascii 
 * string.  See mdv_macros for the vertical type declarations.
 * --Rachel Ames 1/96
 */

char * MDV_verttype2string(int vert_type)
{

   switch(vert_type)  {
      case MDV_VERT_TYPE_SURFACE : 
        return("Surface"); 
      case MDV_VERT_TYPE_SIGMA_P :
	return("Sigma P"); 
      case MDV_VERT_TYPE_PRESSURE :
	return("Pressure (units mb)"); 
      case MDV_VERT_TYPE_Z :
	return("Constant Altitude (units KM MSL)"); 
      case MDV_VERT_TYPE_SIGMA_Z :
	return("Sigma Z"); 
      case MDV_VERT_TYPE_ETA :
	return("ETA"); 
      case MDV_VERT_TYPE_THETA :
	return("Theta"); 
      case MDV_VERT_TYPE_MIXED :
	return("Mixed"); 
      case MDV_VERT_TYPE_ELEV :
	return("Elevation Angles"); 
      case MDV_VERT_TYPE_COMPOSITE :
	return("Composite"); 
      case MDV_VERT_TYPE_CROSS_SEC :
	return("Cross Secional View"); 
      case MDV_VERT_SATELLITE_IMAGE :
	return("Satelite Image"); 
      default:
	return("Unknown Vertical Type"); 
   }
}  
/*****************************************************************/

#ifndef MAX
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif

#define _MEMBUF_INTERNAL

/*
 * MemBuf struct is private - it should not  be accessed by
 * external routines
 */

typedef struct {

  char *buf;           /* pointer to allocated buffer */
  size_t len;          /* number of bytes currently used */
  size_t nalloc;       /* allocated size of buffer */
  
} MEMbuf;


#define MEMBUF_START_SIZE 512

/*
 * NOTE ON USE:
 *
 * The following functions return the pointer to the user
 * buffer:
 *
 * MEMbufAlloc, MEMbufGrow
 * MEMbufLoad, MEMbufAdd, MEMbufConcat
 *
 * When using these, you must set your local buffer pointer
 * to the return value. This allows for the fact that the
 * buffer position may change during a realloc.
 *
 * MEMbufPtr() may also be used at any time to get the
 * user buffer location.
 * 
 * After using MEMbufDup(), MEMbufPtr() must be used to
 * get the pointer to the user buffer.
 *
 */

/*__________________________________________________________________________
 *
 * Default constructor
 *__________________________________________________________________________
 */ 

MEMbuf * 
MEMbufCreate(void) 

{ 

  /*	 
   *	Variables. 
   */ 
  
  MEMbuf *newBuf = umalloc(sizeof(MEMbuf));
  
  /* 
   *	Processing. 
   */ 

  newBuf->buf = (void *)ucalloc(1, MEMBUF_START_SIZE);
  newBuf->len = 0;
  newBuf->nalloc = MEMBUF_START_SIZE;
    
  return(newBuf); 

} 

/*__________________________________________________________________________
 *
 * Copy constructor
 *__________________________________________________________________________
 */ 

MEMbuf * 
MEMbufCreateCopy(MEMbuf *rhs) 

{ 
  void * MEMbufAdd();
  size_t MEMbufLen();
  void * MEMbufPtr();


  MEMbuf *newBuf = MEMbufCreate();
  MEMbufAdd(newBuf, MEMbufPtr(rhs), MEMbufLen(rhs));
    
  return(newBuf); 

} 

/*___________________________________________________________________________
 * 
 * Destructor
 *___________________________________________________________________________
 */ 

void
MEMbufDelete(MEMbuf * thisbuf)

{

  ufree(thisbuf->buf);
  ufree(thisbuf);
  return;

}

/*___________________________________________________________________________
 *
 * Reset the memory buffer - sets current length to 0
 * Zero's out allocated buffer memory.
 *___________________________________________________________________________
 */

void
MEMbufReset(MEMbuf * thisbuf)

{

  thisbuf->len = 0;
  memset(thisbuf->buf, 0, thisbuf->nalloc);
  return;

}

/*___________________________________________________________________________
 *
 * Free up memory allocated for data. MEMbuf still valid.
 *___________________________________________________________________________
 */

void
MEMbufFree(MEMbuf * thisbuf)

{
  
  thisbuf->buf = urealloc(thisbuf->buf, MEMBUF_START_SIZE);
  thisbuf->len = 0;
  thisbuf->nalloc = MEMBUF_START_SIZE;
  return;

}

/*___________________________________________________________________________
 * 
 * Prepare a buffer by allocating or reallocating a starting size.
 * This is done if you want to read data directly into the buffer.
 * Note that this routine sets things up so that the internal buffer
 * looks like the data has already been added, although that is left
 * up to the calling routine (i.e. the buffer length is numbytes
 * after the call to this routine).
 *
 * This routine does not change the existing parts of the buffer,
 * only adjusts the size.
 *
 * Buffer is resized as necessary.
 *
 * Returns pointer to user buffer.
 *___________________________________________________________________________
 */

void *
MEMbufPrepare(MEMbuf * thisbuf, size_t numbytes)

{
  void * MEMbufAlloc();  

  MEMbufAlloc(thisbuf, numbytes);
  thisbuf->len = numbytes;
  return(thisbuf->buf);

}

/*___________________________________________________________________________
 * 
 * Load numbytes from source array into start of target buffer.
 *
 * Buffer is resized as necessary.
 *
 * Returns pointer to user buffer.
 *___________________________________________________________________________
 */

void *
MEMbufLoad(MEMbuf * target, void * source, size_t numbytes)

{
  void *MEMbufAdd();

  MEMbufReset(target);
  return(MEMbufAdd(target, source, numbytes));

}

/*___________________________________________________________________________
 *
 * Add numbytes from source array onto end of buffer.
 *
 * Buffer is resized as necessary.
 *
 * Returns pointer to user buffer.
 *___________________________________________________________________________
 */

void *
MEMbufAdd(MEMbuf * target, void * source, size_t numbytes)

{
  void * MEMbufGrow();



  MEMbufGrow(target, numbytes);
  memcpy(target->buf + target->len, source, numbytes);
  target->len += numbytes;
  return(target->buf);

}

/*___________________________________________________________________________
 *
 * Concat the contents of one membuf onto end of another one.
 *
 * Target buffer is automatically resized as necessary.
 *
 * Returns pointer to user buffer.
 *___________________________________________________________________________
 */

void *
MEMbufConcat(MEMbuf * target, MEMbuf *src)

{

  return (MEMbufAdd(target, src->buf, src->len));

}

/*___________________________________________________________________________
 * 
 * Duplicates the mem buffer and its contents.
 *
 * Returns pointer to new message buffer.
 *___________________________________________________________________________
 */

MEMbuf *
MEMbufDup(MEMbuf * thisbuf)

{

  MEMbuf *newbuf = MEMbufCreate();
  MEMbufLoad(newbuf, thisbuf->buf, thisbuf->nalloc);
  return(newbuf);

}

/*___________________________________________________________________________
 * 
 * Get the currently-used length of the current buffer - in bytes
 *___________________________________________________________________________
 */

size_t
MEMbufLen(MEMbuf * thisbuf)

{
  return(thisbuf->len);
}

/*___________________________________________________________________________
 * 
 * Get a pointer to the start of the usable buffer
 *___________________________________________________________________________
 */

void *
MEMbufPtr(MEMbuf * thisbuf)

{
  return(thisbuf->buf);
}

/*___________________________________________________________________________
 *
 * Print out an MEMbuf struct. For internal debugging
 *___________________________________________________________________________
 */

void 
MEMbufPrint(MEMbuf * thisbuf, FILE * fp)

{

    fprintf(fp, "buf        = 0x%p\n", thisbuf->buf);
    fprintf(fp, "len        = %d\n", (int) thisbuf->len);
    fprintf(fp, "nalloc     = %d\n", (int) thisbuf->nalloc);
    return;

}

/*___________________________________________________________________________
 *
 * Check available space, alloc as needed
 *
 * Returns pointer to user buffer.
 *___________________________________________________________________________
 */

void *
MEMbufAlloc(MEMbuf * thisbuf, size_t nbytes_total)

{
  
  size_t new_alloc;

  if(nbytes_total > thisbuf->nalloc) {

    new_alloc = MAX(thisbuf->nalloc * 2, nbytes_total);
    thisbuf->buf = (char*)urealloc(thisbuf->buf, new_alloc);
    thisbuf->nalloc = new_alloc;
    
  } else if (nbytes_total < thisbuf->nalloc / 2) {
    
    new_alloc = thisbuf->nalloc / 2;
    thisbuf->buf = (char*)urealloc(thisbuf->buf, new_alloc);
    thisbuf->nalloc = new_alloc;
    if (thisbuf->len > thisbuf->nalloc) {
      thisbuf->len = thisbuf->nalloc;
    }
    
  }
  
  return (thisbuf->buf);

}

/*___________________________________________________________________________
 *
 * Check available space, grow if needed
 *
 * Returns pointer to user buffer.
 *___________________________________________________________________________
 */

void *
MEMbufGrow(MEMbuf * thisbuf, size_t nbytes_needed)

{

  size_t nbytes_total;

  nbytes_total = thisbuf->len + nbytes_needed;
  MEMbufAlloc(thisbuf, nbytes_total);
  return (thisbuf->buf);

}



