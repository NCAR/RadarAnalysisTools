/*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
 ** Copyright (c) 1992, UCAR
 ** University Corporation for Atmospheric Research(UCAR)
 ** Proprietary and Confidential to UCAR
 ** National Center for Atmospheric Research(NCAR)
 ** Research Applications Program(RAP)
 ** P.O.Box 3000, Boulder, Colorado, 80307, USA
 ** Licenced use only.
 ** Do not copy or distribute without authorization
 ** 1992/10/03 11:21:10
 *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*/
/**************************************************************************
 * mdv_grid.h
 *
 * header file for mdv grid struct
 *
 * Mike Dixon RAP NCAR Feb 1996
 *
 **************************************************************************/

#ifndef mdv_grid_h
#define mdv_grid_h
      
#ifdef __cplusplus
extern "C" {
#endif
  
#include <os_config.h>
#include <stdio.h>
#include <math.h>
#include <dataport/port_types.h>
#include <mdv/mdv_macros.h>
#include <mdv/mdv_file.h>
  
#define MDV_N_GRID_LABELS 3
#define MDV_GRID_UNITS_LEN 32

typedef struct {

  fl32 rotation;
  fl32 spare[9];

} mdv_flat_params_t;
 
typedef struct {

  fl32 spare[10];

} mdv_ll_params_t;
 
typedef struct {
  
  /*
   * projection params
   */
  
  fl32 proj_origin_lat;         /* lat of origin, degrees */
  fl32 proj_origin_lon;         /* long of origin, degrees */

  union {
    mdv_flat_params_t flat;
    mdv_ll_params_t ll;
  } proj_params;
  
  fl32 minx, miny, minz;	/* start value, SW corner,
				 * bottom plane (* scale)
				 * minz set to -1 if dz_constant is FALSE */
  
  fl32 dx, dy, dz;		/* cartesian spacing in each dirn (* scale)
				 * dz set to -1 if dz_constant is FALSE */
  
  fl32 sensor_x;                /* sensor coords */
  fl32 sensor_y;
  fl32 sensor_z;
  fl32 sensor_lat;
  fl32 sensor_lon;

  si32 spare[11];
    
  si32 proj_type;               /* type of projection used for grid
				 * MDV_PROJ_FLAT, MDV_PROJ_LATLON etc. */
 
  si32 dz_constant;		/* flag to indicate regularly spaced planes.
				 * Set to 1 if regularly-spaced planes
				 * (constant dz), 0 otherwise. 
				 */

  si32 nx, ny, nz;		/* number of points in each dirn */
  
  si32 nbytes_char;		/* number of bytes of character data
				 * at end of this struct */
  
  char unitsx[MDV_GRID_UNITS_LEN]; /* units in x dirn */
  char unitsy[MDV_GRID_UNITS_LEN]; /* units in y dirn */
  char unitsz[MDV_GRID_UNITS_LEN]; /* units in z dirn */
    
} mdv_grid_t;

struct mdv_grid_comps_s;

typedef void (*MDV_latlon2xy_t)(struct mdv_grid_comps_s *comps,
				double lat, double lon,
				double  *x, double *y);

typedef void (*MDV_xy2latlon_t)(struct mdv_grid_comps_s *comps,
				double x, double y,
				double *lat, double *lon);

/*
 * struct for mdv_grid computations
 */

typedef struct mdv_grid_comps_s {

  si32 proj_type;
  
  double origin_lat;
  double origin_lon;
  double rotation;

  double origin_lat_rad;
  double origin_lon_rad;
  double rotation_rad;

  double origin_colat;
  double sin_origin_colat;
  double cos_origin_colat;
  
  MDV_latlon2xy_t latlon2xy;
  MDV_xy2latlon_t xy2latlon;
  

} mdv_grid_comps_t;

/*
 * prototypes
 */
 
extern void MDV_init_flat(double origin_lat,
			  double origin_lon,
			  double rotation,
			  mdv_grid_comps_t *comps);

extern void MDV_init_latlon(mdv_grid_comps_t *comps);

extern void MDV_init_proj(mdv_grid_t *grid,
			  mdv_grid_comps_t *comps);

extern void MDV_latlon2xy(mdv_grid_comps_t *comps,
			  double lat, double lon,
			  double *x, double *y);

extern void MDV_print_grid(FILE *out, char *spacer,
			   mdv_grid_t *grid);

extern void MDV_xy2latlon(mdv_grid_comps_t *comps,
			  double x, double y,
			  double *lat, double *lon);

#ifdef __cplusplus
}
#endif

#endif

