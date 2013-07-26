C
C mf_mdv.inc
C
C Include file for FORTRAN programs using the MDV wrapper routines.
C These parameters mirror the #define values in the <mdv/mdv_file.h>
C and <mdv/mdv_macros.h> C include files.  For explanations and to
C see the latest definition of the MDV files, see the C include files.
C
C Nancy Rehak, June 1997
C


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Defined values for "enumerated" fields in the file.  The "magic
c cookie" values and the revision number are described in the C
c include file <mdv/mdv_file.h>; the rest of the values are described
c in the C include file <mdv/mdv_macros.h>.

c
c "Magic cookie" values used in the different header structures.  These
c values can be tested against the actual field values the check for
c data corruption or program errors.
c

       integer MDV_MASTER_HEAD_MAGIC_COOKIE
       integer MDV_FIELD_HEAD_MAGIC_COOKIE
       integer MDV_VLEVEL_HEAD_MAGIC_COOKIE
       integer MDV_CHUNK_HEAD_MAGIC_COOKIE
       parameter (MDV_MASTER_HEAD_MAGIC_COOKIE = 14142,
     &            MDV_FIELD_HEAD_MAGIC_COOKIE = 14143,
     &            MDV_VLEVEL_HEAD_MAGIC_COOKIE = 14144,
     &            MDV_CHUNK_HEAD_MAGIC_COOKIE = 14145)

c
c The current MDV revision number
c

       integer MDV_REVISION_NUMBER
       parameter (MDV_REVISION_NUMBER = 1)

c
c Collection types
c

       integer MDV_DATA_MEASURED
       integer MDV_DATA_EXTRAPOLATED
       integer MDV_DATA_FORECAST
       integer MDV_DATA_SYNTHESIS
       integer MDV_DATA_MIXED
       parameter (MDV_DATA_MEASURED = 0,
     &            MDV_DATA_EXTRAPOLATED = 1,
     &            MDV_DATA_FORECAST = 2,
     &            MDV_DATA_SYNTHESIS = 3,
     &            MDV_DATA_MIXED = 4)

c
c How the data array is positioned on the earth
c

       integer MDV_ORIENT_OTHER
       integer MDV_ORIENT_SN_WE
       integer MDV_ORIENT_NS_WE
       integer MDV_ORIENT_SN_EW
       integer MDV_ORIENT_NS_EW
       parameter (MDV_ORIENT_OTHER = 0,
     &            MDV_ORIENT_SN_WE = 1,
     &            MDV_ORIENT_NS_WE = 2,
     &            MDV_ORIENT_SN_EW = 3,
     &            MDV_ORIENT_NS_EW = 4)

c
c Projection types
c

       integer MDV_PROJ_NATIVE
       integer MDV_PROJ_LATLON
       integer MDV_PROJ_ARTCC
       integer MDV_PROJ_STEREOGRAPHIC
       integer MDV_PROJ_LAMBERT_CONF
       integer MDV_PROJ_MERCATOR
       integer MDV_PROJ_POLAR_STEREO
       integer MDV_PROJ_POLAR_ST_ELLIP
       integer MDV_PROJ_CYL_EQUIDIST
       integer MDV_PROJ_FLAT
       integer MDV_PROJ_POLAR_RADAR
       integer MDV_PROJ_RADIAL
       integer MDV_PROJ_UNKNOWN
       parameter (MDV_PROJ_NATIVE = -1,
     &            MDV_PROJ_LATLON = 0,
     &            MDV_PROJ_ARTCC = 1,
     &            MDV_PROJ_STEREOGRAPHIC = 2,
     &            MDV_PROJ_LAMBERT_CONF = 3,
     &            MDV_PROJ_MERCATOR = 4,
     &            MDV_PROJ_POLAR_STEREO = 5,
     &            MDV_PROJ_POLAR_ST_ELLIP = 6,
     &            MDV_PROJ_CYL_EQUIDIST = 7,
     &            MDV_PROJ_FLAT = 8,
     &            MDV_PROJ_POLAR_RADAR = 9,
     &            MDV_PROJ_RADIAL = 10,
     &            MDV_PROJ_UNKNOWN = 99)

c
c Describes the order of the data in the arrays
c

       integer MDV_ORDER_XYZ
       integer MDV_ORDER_YXZ
       integer MDV_ORDER_XZY
       integer MDV_ORDER_YZX
       integer MDV_ORDER_ZXY
       integer MDV_ORDER_ZYX
       parameter (MDV_ORDER_XYZ = 0,
     &            MDV_ORDER_YXZ = 1,
     &            MDV_ORDER_XZY = 2,
     &            MDV_ORDER_YZX = 3,
     &            MDV_ORDER_ZXY = 4,
     &            MDV_ORDER_ZYX = 5)

c
c Vertical types
c

       integer MDV_VERT_TYPE_SURFACE
       integer MDV_VERT_TYPE_SIGMA_P
       integer MDV_VERT_TYPE_PRESSURE
       integer MDV_VERT_TYPE_Z
       integer MDV_VERT_TYPE_SIGMA_Z
       integer MDV_VERT_TYPE_ETA
       integer MDV_VERT_TYPE_THETA
       integer MDV_VERT_TYPE_MIXED
       integer MDV_VERT_TYPE_ELEV
       integer MDV_VERT_TYPE_COMPOSITE
       integer MDV_VERT_TYPE_CROSS_SEC
       integer MDV_VERT_SATELLITE_IMAGE
       parameter (MDV_VERT_TYPE_SURFACE = 1,
     &            MDV_VERT_TYPE_SIGMA_P = 2,
     &            MDV_VERT_TYPE_PRESSURE = 3,
     &            MDV_VERT_TYPE_Z = 4,
     &            MDV_VERT_TYPE_SIGMA_Z = 5,
     &            MDV_VERT_TYPE_ETA = 6,
     &            MDV_VERT_TYPE_THETA = 7,
     &            MDV_VERT_TYPE_MIXED = 8,
     &            MDV_VERT_TYPE_ELEV = 9,
     &            MDV_VERT_TYPE_COMPOSITE = 10,
     &            MDV_VERT_TYPE_CROSS_SEC = 11,
     &            MDV_VERT_SATELLITE_IMAGE = 12)

c
c Data formats
c

       integer MDV_NATIVE
       integer MDV_INT8
       integer MDV_INT16
       integer MDV_INT32
       integer MDV_INT64
       integer MDV_FLOAT32
       integer MDV_FLOAT64
       integer MDV_PLANE_RLE8
       integer MDV_ROW_RLE8
       parameter (MDV_NATIVE = 0,
     &            MDV_INT8 = 1,
     &            MDV_INT16 = 2,
     &            MDV_INT32 = 3,
     &            MDV_INT64 = 4,
     &            MDV_FLOAT32 = 5,
     &            MDV_FLOAT64 = 6,
     &            MDV_PLANE_RLE8 = 10,
     &            MDV_ROW_RLE8 = 11)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Lengths of fields and arrays in the headers

c
c Define parameters for the sizes of the arrays used to hold
c the MDV information.
c

c Character string lengths

       integer MDV_CHUNK_INFO_LEN
CMDVINFOL = MDV_INFO_LENGTH
       integer MDVINFOL
CMDVLFLDL = MDV_LONG_FIELD_LEN
       integer MDVLFLDL
CMDVNAMEL = MDV_NAME_LENGTH
       integer MDVNAMEL
CMDVSFLDL = MDV_SHORT_FIELD_LEN
       integer MDVSFLDL
CMDVTRANL = MDV_TRANSFORM_LEN
       integer MDVTRANL
CMDVUNITL = MDV_UNITS_LEN
       integer MDVUNITL
       parameter (MDV_CHUNK_INFO_LEN = 480,
     &            MDVINFOL = 512,
     &            MDVLFLDL = 64,
     &            MDVNAMEL = 128,
     &            MDVSFLDL = 16,
     &            MDVTRANL = 16,
     &            MDVUNITL = 16)

c Array lengths

       integer MDV_MAX_PROJ_PARAMS
       integer MDV_MAX_VLEVELS
       parameter (MDV_MAX_PROJ_PARAMS = 8,
     &            MDV_MAX_VLEVELS = 122)

c Master header lengths.  The size parameter is in bytes.

       integer MDVMSI32
       integer MDVMFL32
       integer MDV_MASTER_HEADER_SIZE
       parameter (MDVMSI32 = 41,
     &            MDVMFL32 = 21,
     &            MDV_MASTER_HEADER_SIZE = 1024)

c Field header lengths.  The size parameter is in bytes.

       integer MDVFSI32
       integer MDVFFL32
       integer MDV_FIELD_HEADER_SIZE
       parameter (MDVFSI32 = 39,
     &            MDVFFL32 = 31,
     &            MDV_FIELD_HEADER_SIZE = 416)

c Vlevel header lengths.  The size parameter is in bytes.

       integer MDV_NUM_VLEVEL_HEADER_SI32
       integer MDV_NUM_VLEVEL_HEADER_FL32
       integer MDV_VLEVEL_HEADER_SIZE
       parameter (MDV_NUM_VLEVEL_HEADER_SI32 = 127,
     &            MDV_NUM_VLEVEL_HEADER_FL32 = 127,
     &            MDV_VLEVEL_HEADER_SIZE = 1024)

c Chunk header lengths (the chunk header has no float values). 
c The size parameter is in bytes.

       integer MDV_NUM_CHUNK_HEADER_SI32
       integer MDV_CHUNK_HEADER_SIZE
       parameter (MDV_NUM_CHUNK_HEADER_SI32 = 6,
     &            MDV_CHUNK_HEADER_SIZE = 512)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Array index values for the different header arrays
c

c Master header integer field indices

C       integer MDV_MH_STRUCT_ID_INDEX
C       integer MDV_MH_REVISION_NUMBER_INDEX
C       integer MDV_MH_TIME_GEN_INDEX
C       integer MDV_MH_USER_TIME_INDEX
C       integer MDV_MH_TIME_BEGIN_INDEX
C       integer MDV_MH_TIME_END_INDEX
C       integer MDV_MH_TIME_CENTROID_INDEX
C       integer MDV_MH_TIME_EXPIRE_INDEX
C       integer MDV_MH_NUM_DATA_TIMES_INDEX
C       integer MDV_MH_INDEX_NUMBER_INDEX
C       integer MDV_MH_DATA_DIMENSION_INDEX
C       integer MDV_MH_DATA_COLLECT_TYPE_INDEX
C       integer MDV_MH_USER_DATA_INDEX
C       integer MDV_MH_NATIVE_VLEVEL_TYPE_INDEX
C       integer MDV_MH_VLEVEL_TYPE_INDEX
C       integer MDV_MH_VLEVEL_INCLUDED_INDEX
C       integer MDV_MH_GRID_ORDER_DIR_INDEX
C       integer MDV_MH_GRID_ORDER_INDICES_INDEX
C       integer MDV_MH_N_FIELDS_INDEX
C       integer MDV_MH_MAX_NX_INDEX
C       integer MDV_MH_MAX_NY_INDEX
C       integer MDV_MH_MAX_NZ_INDEX
C       integer MDV_MH_N_CHUNKS_INDEX
C       integer MDV_MH_FIELD_HDR_OFFSET_INDEX
C       integer MDV_MH_VLEVEL_HDR_OFFSET_INDEX
C       integer MDV_MH_CHUNK_HDR_OFFSET_INDEX
C       integer MDV_MH_FIELD_GRIDS_DIFFER_OFFSET
C       parameter (MDV_MH_STRUCT_ID_INDEX = 1,
C     &            MDV_MH_REVISION_NUMBER_INDEX = 2,
C     &            MDV_MH_TIME_GEN_INDEX = 3,
C     &            MDV_MH_USER_TIME_INDEX = 4,
C     &            MDV_MH_TIME_BEGIN_INDEX = 5,
C     &            MDV_MH_TIME_END_INDEX = 6,
C     &            MDV_MH_TIME_CENTROID_INDEX = 7,
C     &            MDV_MH_TIME_EXPIRE_INDEX = 8,
C     &            MDV_MH_NUM_DATA_TIMES_INDEX = 9,
C     &            MDV_MH_INDEX_NUMBER_INDEX = 10,
C     &            MDV_MH_DATA_DIMENSION_INDEX = 11,
C     &            MDV_MH_DATA_COLLECT_TYPE_INDEX = 12,
C     &            MDV_MH_USER_DATA_INDEX = 13,
C     &            MDV_MH_NATIVE_VLEVEL_TYPE_INDEX = 14,
C     &            MDV_MH_VLEVEL_TYPE_INDEX = 15,
C     &            MDV_MH_VLEVEL_INCLUDED_INDEX = 16,
C     &            MDV_MH_GRID_ORDER_DIR_INDEX = 17,
C     &            MDV_MH_GRID_ORDER_INDICES_INDEX = 18,
C     &            MDV_MH_N_FIELDS_INDEX = 19,
C     &            MDV_MH_MAX_NX_INDEX = 20,
C     &            MDV_MH_MAX_NY_INDEX = 21,
C     &            MDV_MH_MAX_NZ_INDEX = 22,
C     &            MDV_MH_N_CHUNKS_INDEX = 23,
C     &            MDV_MH_FIELD_HDR_OFFSET_INDEX = 24,
C     &            MDV_MH_VLEVEL_HDR_OFFSET_INDEX = 25,
C     &            MDV_MH_CHUNK_HDR_OFFSET_INDEX = 26,
C     &            MDV_MH_FIELD_GRIDS_DIFFER_OFFSET = 27)

c Master header real field indices

C       integer MDV_MH_SENSOR_LON_INDEX
C       integer MDV_MH_SENSOR_LAT_INDEX
C       integer MDV_MH_SENSOR_ALT_INDEX
C       parameter(MDV_MH_SENSOR_LON_INDEX = 7,
C     &           MDV_MH_SENSOR_LAT_INDEX = 8,
C     &           MDV_MH_SENSOR_ALT_INDEX = 9)

c Field header integer field indices

       integer MDV_FH_STRUCT_ID_INDEX
       integer MDV_FH_FIELD_CODE_INDEX
       integer MDV_FH_USER_TIME1_INDEX
       integer MDV_FH_FORECAST_DELTA_INDEX
       integer MDV_FH_USER_TIME2_INDEX
       integer MDV_FH_USER_TIME3_INDEX
       integer MDV_FH_FORECAST_TIME_INDEX
       integer MDV_FH_USER_TIME4_INDEX
       integer MDVFNXIX
       integer MDVFNYIX
       integer MDVFNZIX
       integer MDV_FH_PROJ_TYPE_INDEX
       integer MDV_FH_ENCODING_TYPE_INDEX
       integer MDV_DATA_ELEMENT_NBYTES_INDEX
       integer MDV_FH_FIELD_DATA_OFFSET_INDEX
       integer MDV_FH_VOLUME_SIZE_INDEX
       parameter (MDV_FH_STRUCT_ID_INDEX = 1,
     &            MDV_FH_FIELD_CODE_INDEX = 2,
     &            MDV_FH_USER_TIME1_INDEX = 3,
     &            MDV_FH_FORECAST_DELTA_INDEX = 4,
     &            MDV_FH_USER_TIME2_INDEX = 5,
     &            MDV_FH_USER_TIME3_INDEX = 6,
     &            MDV_FH_FORECAST_TIME_INDEX = 7,
     &            MDV_FH_USER_TIME4_INDEX = 8,
     &            MDVFNXIX = 9,
     &            MDVFNYIX = 10,
     &            MDVFNZIX = 11,
     &            MDV_FH_PROJ_TYPE_INDEX = 12,
     &            MDV_FH_ENCODING_TYPE_INDEX = 13,
     &            MDV_DATA_ELEMENT_NBYTES_INDEX = 14,
     &            MDV_FH_FIELD_DATA_OFFSET_INDEX = 15,
     &            MDV_FH_VOLUME_SIZE_INDEX = 16)

c Field header real field indices

       integer MDV_FH_PROJ_ORIGIN_LAT_INDEX
       integer MDV_FH_PROJ_ORIGIN_LON_INDEX
       integer MDV_FH_PROJ_PARAM1_INDEX
       integer MDV_FH_PROJ_PARAM2_INDEX
       integer MDV_FH_PROJ_PARAM3_INDEX
       integer MDV_FH_PROJ_PARAM4_INDEX
       integer MDV_FH_PROJ_PARAM5_INDEX
       integer MDV_FH_PROJ_PARAM6_INDEX
       integer MDV_FH_PROJ_PARAM7_INDEX
       integer MDV_FH_PROJ_PARAM8_INDEX
       integer MDV_FH_VERT_REFERENCE_INDEX
       integer MDVFGDX
       integer MDVFGDY
       integer MDVFGDZ
       integer MDVFXMIN
       integer MDVFYMIN
       integer MDVFZMIN
       integer MDV_FH_SCALE_INDEX
       integer MDV_FH_BIAS_INDEX
       integer MDV_FH_BAD_DATA_VALUE_INDEX
       integer MDV_FH_MISSING_DATA_VALUE_INDEX
       integer MDV_FH_PROJ_ROTATION_INDEX

       parameter (MDV_FH_PROJ_ORIGIN_LAT_INDEX = 1,
     &            MDV_FH_PROJ_ORIGIN_LON_INDEX = 2,
     &            MDV_FH_PROJ_PARAM1_INDEX = 3,
     &            MDV_FH_PROJ_PARAM2_INDEX = 4,
     &            MDV_FH_PROJ_PARAM3_INDEX = 5,
     &            MDV_FH_PROJ_PARAM4_INDEX = 6,
     &            MDV_FH_PROJ_PARAM5_INDEX = 7,
     &            MDV_FH_PROJ_PARAM6_INDEX = 8,
     &            MDV_FH_PROJ_PARAM7_INDEX = 9,
     &            MDV_FH_PROJ_PARAM8_INDEX = 10,
     &            MDV_FH_VERT_REFERENCE_INDEX = 11,
     &            MDVFGDX = 12,
     &            MDVFGDY = 13,
     &            MDVFGDZ= 14,
     &            MDVFXMIN = 15,
     &            MDVFYMIN = 16,
     &            MDVFZMIN = 17,
     &            MDV_FH_SCALE_INDEX = 18,
     &            MDV_FH_BIAS_INDEX = 19,
     &            MDV_FH_BAD_DATA_VALUE_INDEX = 20,
     &            MDV_FH_MISSING_DATA_VALUE_INDEX = 21,
     &            MDV_FH_PROJ_ROTATION_INDEX = 22)

c Vlevel header integer field indices

       integer MDV_VH_STRUCT_ID_INDEX
       integer MDV_VH_VLEVEL_TYPE_INDEX
       parameter (MDV_VH_STRUCT_ID_INDEX = 1,
     &            MDV_VH_VLEVEL_TYPE_INDEX = 2)

c Vlevel header real field indices

       integer MDV_VH_VLEVEL_PARAMS_INDEX
       parameter (MDV_VH_VLEVEL_PARAMS_INDEX = 1)

c Chunk header integer field indices

       integer MDV_CH_STRUCT_ID_INDEX
       integer MDV_CH_CHUNK_ID_INDEX
       integer MDV_CH_CHUNK_DATA_OFFSET_INDEX
       integer MDV_CH_SIZE_OFFSET

       parameter (MDV_CH_STRUCT_ID_INDEX = 1,
     &            MDV_CH_CHUNK_ID_INDEX = 2,
     &            MDV_CH_CHUNK_DATA_OFFSET_INDEX = 3,
     &            MDV_CH_SIZE_OFFSET = 4)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Function return values

c
c Status values returned by read routines.
c

       integer MDVROK
       integer MDV_READ_OPEN_FAILURE
       integer MDV_READ_BAD_MASTER_HDR
       integer MDV_READ_INVALID_FIELD_NUM
       integer MDV_READ_BAD_FIELD_HDR
       integer MDV_READ_BAD_VLEVEL_HDR
       integer MDV_READ_NO_VLEVEL_HDRS
       integer MDV_READ_INVALID_CHUNK_NUM
       integer MDV_READ_BAD_CHUNK_HDR
       integer MDV_READ_DATA_ARRAY_TOO_SMALL
       integer MDV_READ_DATA_ERROR
       parameter (MDVROK=  0,
     &            MDV_READ_OPEN_FAILURE = 1,
     &            MDV_READ_BAD_MASTER_HDR = 2,
     &            MDV_READ_INVALID_FIELD_NUM = 3,
     &            MDV_READ_BAD_FIELD_HDR = 4,
     &            MDV_READ_BAD_VLEVEL_HDR = 5,
     &            MDV_READ_NO_VLEVEL_HDRS = 6,
     &            MDV_READ_INVALID_CHUNK_NUM = 7,
     &            MDV_READ_BAD_CHUNK_HDR = 8,
     &            MDV_READ_DATA_ARRAY_TOO_SMALL = 9,
     &            MDV_READ_DATA_ERROR = 10)

c
c Status values returned by write routines.
c

       integer MDV_WRITE_SUCCESSFUL
       integer MDV_WRITE_OPEN_FAILURE
       integer MDV_WRITE_MASTER_HDR_FAILED
       integer MDV_WRITE_FIELD_FAILED
       parameter (MDV_WRITE_SUCCESSFUL = 0,
     &            MDV_WRITE_OPEN_FAILURE = 1,
     &            MDV_WRITE_MASTER_HDR_FAILED = 2,
     &            MDV_WRITE_FIELD_FAILED = 3)

