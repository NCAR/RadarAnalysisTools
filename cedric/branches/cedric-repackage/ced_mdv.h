#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <memory.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <dirent.h>
#include "./include/port_types.h"
#include "./include/mdv_read.h"
#include "./include/os_config.h"
#include "./include/mdv_print.h"
#include "./include/mdv_file.h"
#include "./include/mdv_user.h"
#include "./include/mdv_write.h"

#define MDV_READ_SUCCESSFUL             0
#define MDV_READ_OPEN_FAILURE           1
#define MDV_READ_BAD_MASTER_HDR         2
#define MDV_READ_INVALID_FIELD_NUM      3
#define MDV_READ_BAD_FIELD_HDR          4
#define MDV_READ_BAD_VLEVEL_HDR         5
#define MDV_READ_NO_VLEVEL_HDRS         6
#define MDV_READ_INVALID_CHUNK_NUM      7
#define MDV_READ_BAD_CHUNK_HDR          8
#define MDV_READ_DATA_ARRAY_TOO_SMALL   9
#define MDV_READ_DATA_ERROR  10

#ifndef BOOL_STR
#define BOOL_STR(a) (a == FALSE ? "false" : "true")
#endif

struct mdv_cdf{

  FILE  *mdv_in;
  FILE  *mdv_out;
  int   bytes_master;
  int   field_data_offset;
  int   offset_vlevels;
  int   vlevels;
  int   num_fields;
  int   in_unit;
  int   time_forecast;
};


