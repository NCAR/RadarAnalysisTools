#include "/opt/local/netcdf-3/include/netcdf.h"

/*
 *THESE ARE FOR THE NETCDF DIMENSIONS AT THE BEGINNING OF A NETCDF FILE.
 */
#define NUM_FIELDS_INDEX      0
#define SHORT_STRING_INDEX    1
#define LONG_STRING_INDEX     2
#define NUM_LANDMARKS_INDEX   3
#define USWRP_NUM_GRIDS_INDEX 4
#define NUM_X_INDEX           5    /*The number of x grid points*/
#define NUM_Y_INDEX           6    /*The number of y grid points*/
#define NUM_Z_INDEX           7
#define NUM_LEVELS_DIM_INDEX  8
#define RESOLUTION_INDEX      9
#define DATE_STRING_INDEX     10
#define TIME_INDEX            11
#define NUM_RADARS_INDEX      12

/*
 *THESE ARE FOR THE NETCDF VARIABLES.
 */
#define FIELD_NAME_INDEX       0
/*There can be up to NFMAX(25)field names*/
#define START_FIELD_IDS        1 
#define GRID_TYPE_INDEX        26
#define NUM_LEVELS_VAR_INDEX   27
#define RESOLUTION_VAR_INDEX   28
#define GRIDDED_X_INDEX        29
#define GRIDDED_Y_INDEX        30
#define VAR_RESOLUTION_INDEX   31
#define X_SPACING_INDEX        32
#define Y_SPACING_INDEX        33
#define X_MIN_INDEX            34
#define X_MAX_INDEX            35
#define Y_MIN_INDEX            36
#define Y_MAX_INDEX            37
#define LAT_INDEX              38
#define LON_INDEX              39
#define ALT_INDEX              40
#define START_DATE_INDEX       41   
#define END_DATE_INDEX         42
#define START_TIME_INDEX       43
#define END_TIME_INDEX         44
#define NUM_RADAR_INDEX        45
#define PROJECT_TEXT_INDEX     46
#define LANDMARK_X_INDEX       47
#define LANDMARK_Y_INDEX       48
#define LANDMARK_Z_INDEX       49
#define LANDMARK_NAMES_INDEX   50
#define RADAR_NAME_INDEX       51
#define REF_NYQUIST_INDEX      52
#define NYQUIST_VELS_INDEX     53
#define SOURCE_INDEX           54
#define PROJ_INDEX             55
#define VOL_HEAD_INDEX         56
#define SCIENTIST_INDEX        57
#define SUBMITTERS_INDEX       58
#define CEDRIC_DATE_INDEX      59
#define CEDRIC_TIME_INDEX      60
#define ZEBRA_X_INDEX          61
#define ZEBRA_Y_INDEX          62
#define ZEBRA_Z_INDEX          63
#define Z_SPACING_INDEX        64
#define BAD_FLAG_INDEX         65
#define PROGRAM_ID_INDEX       66
#define TIME_OFFSET_INDEX      67        /*This is for ZEBRA*/
#define BASE_TIME_INDEX        68        /*This is for ZEBRA*/
#define GRID_NAME_INDEX        69  
#define GRID_LAT_INDEX         70        /*This is for ZEBRA*/
#define GRID_LON_INDEX         71        /*This is for ZEBRA*/

typedef int boolean;


