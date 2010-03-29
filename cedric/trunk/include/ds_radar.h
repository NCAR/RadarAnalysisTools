/*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
 ** Copyright (c) 1992, UCAR
 ** University Corporation for Atmospheric Research(UCAR)
 ** National Center for Atmospheric Research(NCAR)
 ** Research Applications Program(RAP)
 ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA
 ** All rights reserved. Licenced use only.
 ** Do not copy or distribute without authorization
 ** 1993/3/3 22:0:32
 *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*/

/*******************************************************************
 * didss/ds_radar.h
 *
 * Radar data types for DIDSS.
 ******************************************************************/

#ifndef DsRadar_h
#define DsRadar_h

#ifdef __cplusplus
extern "C" {
#endif

#include "file_io.h"

#define DS_RADAR_GROUND_TYPE 0
#define DS_RADAR_AIRBORNE_FORE_TYPE 1
#define DS_RADAR_AIRBORNE_AFT_TYPE 2
#define DS_RADAR_AIRBORNE_TAIL_TYPE 3
#define DS_RADAR_AIRBORNE_LOWER_TYPE 4
#define DS_RADAR_SHIPBORNE_TYPE 5

#define DS_RADAR_UNKNOWN_MODE -1
#define DS_RADAR_CALIBRATION_MODE 0
#define DS_RADAR_SECTOR_MODE 1
#define DS_RADAR_COPLANE_MODE 2
#define DS_RADAR_RHI_MODE 3
#define DS_RADAR_VERTICAL_POINTING_MODE 4
#define DS_RADAR_TARGET_MODE 5
#define DS_RADAR_MANUAL_MODE 6
#define DS_RADAR_IDLE_MODE 7
#define DS_RADAR_SURVEILLANCE_MODE 8
#define DS_RADAR_VERTICAL_SWEEP_MODE 9

#define DS_POLARIZATION_HORIZ_TYPE 0
#define DS_POLARIZATION_VERT_TYPE 1
#define DS_POLARIZATION_RIGHT_CIRC_TYPE 2
#define DS_POLARIZATION_ELLIPTICAL_TYPE 3
#define DS_POLARIZATION_LEFT_CIRC_TYPE 4
#define DS_POLARIZATION_DUAL_TYPE 5

/*
 * message type definition
 */

#define DS_MESSAGE_TYPE_DSRADAR 1001
#define DS_MESSAGE_TYPE_RIDDS_BEAM 1002
#define DS_MESSAGE_TYPE_LL_BEAM 1003

/*
 * end-of-vol message type - needs to be moved - dixon
 */

#define DS_MESSAGE_TYPE_END_OF_VOLUME 1008

/*
 * part data type definitions
 */

#define DS_DATA_TYPE_RADAR_PARAMS 1
#define DS_DATA_TYPE_RADAR_FIELD_PARAMS 2
#define DS_DATA_TYPE_RADAR_BEAM_DATA 4
#define DS_DATA_TYPE_RADAR_FLAGS 8

/*
 * The message part data types are as follows:
 *
 * RADAR_PARAMS = 1,
 * FIELD_PARAMS = 2,
 * RADAR_BEAM   = 4,
 * RADAR_FLAGS  = 8
 *
 * The actual definitions are in a typedef in the DsRadarMsg class.
 */
  
/*
 * DS_MESSAGE_TYPE_RADAR has the following components.
 * Not all components will be in each message.
 *
 *   DsRadarParams_t
 *   nfields * DsFieldParams_t
 *   DsBeamHdr_t and gate_by_gate data
 */

#define NCHAR_DS_RADAR_PARAMS (2 * DS_LABEL_LEN)

typedef struct {
  
  si32 radar_id;		/* unique number */

  si32 radar_type;              /* use radar type defs above */

  si32 nfields;                 /* number of fields */

  si32 ngates;                  /* number of range gates */

  si32 samples_per_beam;        /* number of pulse samples per
				 * data beam */

  si32 scan_type;		/* the current scan strategy */
  
  si32 scan_mode;		/* use scan type defs above */
  
  si32 nfields_current;	        /* the number of fields currently being
				 * sent - the positions of the fields
				 * are indicated by the
				 * bits set in the field_flag */
  
  si32 field_flag;		/* for each field included in the beam data,
				 * the relevant bit is set in this long.
				 * For example, suppose there are a total
				 * of 6 fields referred to in the params
				 * struct, and only fields
				 * 0, 1, 3, and 5 are currently in
				 * the data stream.
				 * Then, field_flag = 00.....0101011 */

  si32 polarization;            /* use polarization type defs above */

  si32 spare_ints[4];

  fl32 radar_constant;          /* radar constant */
  fl32 altitude;		/* km */
  fl32 latitude;		/* degrees */
  fl32 longitude;		/* degrees */
  fl32 gate_spacing;		/* km */
  fl32 start_range;		/* km */
  fl32 horiz_beam_width;	/* degrees */
  fl32 vert_beam_width;         /* degrees */
  fl32 pulse_width;		/* micro-seconds */
  fl32 prf;			/* pulse repitition freq (/s) */
  fl32 wavelength;		/* cm */
  fl32 xmit_peak_pwr;           /* watts */
  fl32 receiver_mds;            /* dBm */
  fl32 receiver_gain;           /* dB */
  fl32 antenna_gain;            /* dB */
  fl32 system_gain;             /* dB */
  fl32 unambig_vel;             /* m/s */
  fl32 unambig_range;           /* km */
   
  fl32 spare_floats[8];

  char radar_name[DS_LABEL_LEN];
  char scan_type_name[DS_LABEL_LEN];
  
} DsRadarParams_t;

#define NCHAR_DS_FIELD_PARAMS (DS_FIELD_NAME_LEN + DS_FIELD_UNITS_LEN)

typedef struct {

  si32 byte_width;                    /* width of data in bytes */
  si32 missing_data_value;            /* value used for missing data */
   
  fl32 scale;			      /* gain of the data */
  fl32 bias;			      /* offset of zero value */

  fl32 spare_floats[2];

  char name[DS_FIELD_NAME_LEN];       /* field name */
  char units[DS_FIELD_UNITS_LEN];     /* field units */
  
} DsFieldParams_t;

/*
 * struct for header for gate data packet
 */

typedef struct {
  
  si32 time;              /* secs since Jan 1 1970 */

  si32 vol_num;           /* the volume scan number */
  si32 tilt_num;	  /* the tilt number in the volume scan */
  
  si32 spare_ints[3];

  fl32 azimuth;		  /* deg */
  fl32 elevation;	  /* deg */
  fl32 target_elev;	  /* deg */

  fl32 spare_floats[1];
  
} DsBeamHdr_t;

typedef struct {
  
  si32 time;              /* secs since Jan 1 1970 */

  si32 vol_num;           /* the volume scan number */
  si32 tilt_num;	  /* the tilt number in the volume scan */
  si32 scan_type;		/* the current scan strategy */
  
  si32 start_of_tilt;
  si32 end_of_tilt;

  si32 start_of_volume;
  si32 end_of_volume;

  si32 new_scan_type;

  si32 spare_ints[4];

} DsRadarFlags_t;

#define DSRADAR_ELEV_INIT 91817161

typedef struct {
  
  si32 n_elev;
  fl32 *elev_array;
  ui08 *chunk_buf;
  int chunk_len;
  int init_flag;

} DsRadarElev_t;

/*
 * function prototypes
 */

/***********************
 * BE_to_DsRadarParams()
 *
 * Convert BE to DsRadarParams_t
 */

extern void BE_to_DsRadarParams(DsRadarParams_t *params);
     
/*************************
 * BE_from_DsRadarParams()
 *
 * Convert DsRadarParams_t to BE
 */

extern void BE_from_DsRadarParams(DsRadarParams_t *params);

/***********************
 * BE_to_DsFieldParams()
 *
 * Convert BE to DsFieldParams_t
 */

extern void BE_to_DsFieldParams(DsFieldParams_t *field);
     
/*************************
 * BE_from_DsFieldParams()
 *
 * Convert DsFieldParams_t to BE
 */

extern void BE_from_DsFieldParams(DsFieldParams_t *field);

/*******************
 * BE_to_DsBeamHdr()
 *
 * Convert BE to DsBeamHdr_t
 */

extern void BE_to_DsBeamHdr(DsBeamHdr_t *beam);

/***************************
 * BE_from_DsBeamHdr()
 *
 * Convert DsBeamHdr_t to BE
 */

extern void BE_from_DsBeamHdr(DsBeamHdr_t *beam);

/*******************
 * BE_to_DsRadarFlags()
 *
 * Convert BE to DsRadarFlags_t
 */

extern void BE_to_DsRadarFlags(DsRadarFlags_t *flags);

/***************************
 * BE_from_DsRadarFlags()
 *
 * Convert DsRadarFlags_t to BE
 */

extern void BE_from_DsRadarFlags(DsRadarFlags_t *flags);

/********************
 * DsRadarElev_init()
 *
 * Initialize radar elevation struct
 */

extern void DsRadarElev_init(DsRadarElev_t *elev);

/*********************
 * DsRadarElev_alloc()
 *
 * Alloc arrays for radar elevation struct
 */

extern void DsRadarElev_alloc(DsRadarElev_t *elev, int nelev);

/********************
 * DsRadarElev_free()
 *
 * Free arrays for radar elevation struct
 */

extern void DsRadarElev_free(DsRadarElev_t *elev);

/****************************
 * DsRadarElev_load_chunk()
 *
 * Load up chunk data
 */

extern void DsRadarElev_load_chunk(DsRadarElev_t *elev);

/****************************
 * DsRadarElev_unload_chunk()
 *
 * Unload chunk data into struct
 */

extern void DsRadarElev_unload_chunk(DsRadarElev_t *elev,
				     ui08 *chunk, int chunk_len);

/*****************
 * print routines
 */

extern void DsRadarParams_print(FILE *out, char *spacer,
				DsRadarParams_t *rparams);

extern void DsFieldParams_print(FILE *out, char *spacer,
				DsFieldParams_t *fparams);

extern void DsBeamHdr_print(FILE *out, char *spacer,
			    DsBeamHdr_t *bhdr);

extern void DsRadarFlags_print(FILE *out, char *spacer,
			       DsRadarFlags_t *flags);

extern void DsRadarElev_print(FILE *out, char *spacer,
			      DsRadarElev_t *elev);

#ifdef __cplusplus
}
#endif

#endif

