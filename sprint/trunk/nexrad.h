#include <sys/file.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>

#define ANGSCALE 0.005493164
#define READ_BUFFER_MAX   65536
#define BYTES_FILE_HEADER 24     /* bytes in NEXRAD LII file header record*/
#define BYTES_TAPE_HEADER 31616  /* bytes in NEXRAD LII tape header record*/
#define BYTES_DATA_BLOCK  2432   /* bytes in NEXRAD LII data block*/
#define MIN_ARCH_BYTES    24
#define END_OF_FILE       -1
#define INCORRECT_FORMAT  -2 
#define FILE_READ_ERROR   -3
#define END_OF_TAPE       -4
#define TAPE_HEADER 1
#define FILE_HEADER 2
#define DATA_RECORD 3
#define YES         1
#define NO          0
#define DD_SCALE(x,scale,offset) ((x)*(scale)+(offset)+.5)
#define        NEX_NUM_DELTAS 5 /* num frequencies? */
#define    NEX_NUM_UNAMB_RNGS 8
#define SPEED_OF_LIGHT 2.997925e8 
#define           NEX_AZ_RATE 0.001373291016
/*
 *The MXFLD AND MXGAT MUST MATCH THE VALUES IN FORTRAN.
 */
#define MXFLD      3     /* max # of fields */
#define MXGAT      1837  /* max # of gates */
/*
 *The different types of messages.
 */
#define            DIGITAL_RADAR_DATA 1  
#define               RDA_STATUS_DATA 2
#define  PERFORMANCE_MAINTENANCE_DATA 3
#define           CONSOLE_MESSAGE_A2G 4
#define          MAINTENANCE_LOG_DATA 5
#define          RDA_CONTROL_COMMANDS 6
#define       VOLUME_COVERAGE_PATTERN 7
#define          CLUTTER_SENSOR_ZONES 8
#define              REQUEST_FOR_DATA 9
#define           CONSOLE_MESSAGE_G2A 10
#define         LOOPBACK_TEST_RDA_RPG 11
#define         LOOPBACK_TEST_RDG_RPA 12
#define     CLUTTER_FILTER_BYPASS_MAP 13
#define     EDITED_CLUTTER_FILTER_MAP 14


/* volume coverage patterns
 */
# define SCAN_16_PER_5_MIN 11
# define SCAN_11_PER_5_MIN 2x1
# define SCAN_8_PER_10_MIN 31
# define SCAN_7_PER_10_MIN 32
				  

/*
 * function prototypes
 */
void init_nexrad_structures();
void nexrad_rdbeam();
int read_nexrad_ray(float flddat[MXFLD][MXGAT],char cfldnam[MXFLD][9],
                    char requested[MXFLD][8], int number_req_fields,
                    int *number_of_fields,int *gate_sp,int *numgates,
                    int *range_to_first_gate,int swap);
int  read_nexrad_record(char buffer[READ_BUFFER_MAX],int *bytes_in_buffer,
                        int swap);
void get_tape_header(char fileBuffer[READ_BUFFER_MAX]);
void get_file_header(char fileBuffer[READ_BUFFER_MAX],int swap);
void get_message_header(char fileBuffer[READ_BUFFER_MAX],int *blockType,
                        int position);
void get_digital_radar_data_header(char fileBuffer[READ_BUFFER_MAX], 
                                   int position_of_drdh,
                                   char fields_available[MXFLD][8],
                                   int swap);
void get_VCP_info();
void get_radar_location(float *radar_lat,float *radar_lon,float *radar_alt,
                       char radarName[4] );
void  get_radar_name_(char name[4]);
/*
 *Global structures and variables.
 */

struct nexrad_data_info{
char fileBuffer[READ_BUFFER_MAX];
int  bytesBuffer;
int  bufferPtr;
int  buffer_position;
};

/*
 *Structure definitions taken from nexh.h written by Dick Oye.
 */

struct nexrad_site_info{
    char   radar_name[8];
    char   city[25];
    char   state[4];
    char   month[3];
    int    latd;
    int    latm;
    int    lats;
    int    lond;
    int    lonm;
    int    lons;
    int    site_number;
    int    hour;
    int    min;
    int    sec;
    int    day;
    int    year;
    float  altitude;
    float  frequency_mhz;
    float  short_pulse_ns;
    float  long_pulse_ns;
};

struct nexrad_volume_scan_info{
    int year;
    int month;
    int day;
    int hour;
    int minute;
    int second;
    int millisecond;
    int processing;
    int num_flds_in_sweep;
    int elevation_number;
    float fixed_angle;
};

struct CTM_info {               /* ignored */
    short CTM_word_1;
    short CTM_word_2;
    short CTM_word_3;
    short CTM_word_4;
    short CTM_word_5;
    short CTM_word_6;
};

struct nexrad_message_header {
    short message_len;          /* in 16-bit words */
    unsigned char channel_id;
    unsigned char message_type;
    short seq_num;              /* mod 0x7fff */
    short julian_date;          /* from 1/1/70 */
    int   milliseconds_past_midnight;
    short num_message_segs;
    short message_seg_num;
};


struct digital_radar_data_header {
    int    milliseconds_past_midnight; /* (15-16) */
    short julian_date;          /* (17) from 1/1/70 */
    short unamb_range_x10;      /* (18) km. */
    unsigned short azimuth;     /* (19) binary angle */
    short radial_num;           /* (20) */
    short radial_status;        /* (21) */
    unsigned short elevation;   /* (22) binary angle */
    short elev_num;             /* (23) */
    short ref_gate1;            /* (24) meters */
    short vel_gate1;            /* (25) meters */
    short ref_gate_width;       /* (26) meters */
    short vel_gate_width;       /* (27) meters */
    short ref_num_gates;        /* (28) */
    short vel_num_gates;        /* (29) */
    short sector_num;           /* (30) */
    float sys_gain_cal_const;   /* (31-32) */
    short ref_ptr;              /* (33) byte count from start of drdh */
    short vel_ptr;              /* (34) byte count from start of drdh */
    short sw_ptr;               /* (35) byte count from start of drdh */
    short velocity_resolution;  /* (36) */
    short vol_coverage_pattern; /* (37) */
    short VNV1;                 /* V & V simulator reserved */
    short VNV2;
    short VNV3;
    short VNV4;
    short ref_data_playback;    /* (42) */
    short vel_data_playback;    /* (43) */
    short sw_data_playback;     /* (44) */
    short nyquist_vel_x100;     /* (45)m/s */
    short atmos_atten_factor_x1000; /* (46) dB/km */
    short threshold_parameter;  /* (47) */
    /* c...mark */
    /* word_?? are meant as fillers and correspond to the documentation */
    short word_48;
    short word_49;
    short word_50;
    short word_51;
    short word_52;
    short word_53;
    short word_54;
    short word_55;
    short word_56;
    short word_57;
    short word_58;
    short word_59;
    short word_60;
    short word_61;
    short word_62;
    short word_63;
    short extended_header_ptr;	/* byte count from start of drdh */
};

struct rda_status_info {
   short rda_status;		/* ( 1) halfword location */
   short oper_status;		/* ( 2) */
   short cntrl_status;		/* ( 3) */
   short aux_pwr_gen_state;	/* ( 4) */
   short atp;			/* ( 5) */
   short ref_cal_corr;		/* ( 6) */
   short dte;			/* ( 7) */
   short vcp;			/* ( 8) */
   short rds_cntl_auth;		/* ( 9) */
   short intefr_det_rate;	/* (10) */
   short op_mode;		/* (11) */
   short intefr_suppr_unit;	/* (12) */
   short arch2status;		/* (13) */
   short arch2vols;		/* (14) */
   short rda_alarms;		/* (15) */
   short command_ak;		/* (16) */
   short ch_cntrl_stat;		/* (17) */
   short spol_blnk_stat;	/* (18) */
   short bypass_map_date;	/* (19) */
   short bypass_map_time;	/* (20) */
   short notch_map_date;	/* (21) */
   short notch_map_time;	/* (22) */
   short tps_stat;		/* (23) */
   short spare1;		/* (24) */
   short spare2;		/* (25) */
   short spare3;		/* (26) */
   short alarm_codes[14];	/* (27-40) */
};

/*
 *Structures for the volume coverage pattern.
 */
struct nexrad_VCP_items {
    int fixed_angle;
    int wave_type;
    int is_prf_num;
    int is_pulse_count;
    int az_rate;
    int item_6;
    int id_prf_num;
    int id_pulse_count;
    int item_9;
    int item_10;
    int item_11;
    int item_12;
    int item_13;
    int item_14;
    int item_15;
    int item_16;
    int item_17;
};


struct nexrad_VCP_header {
    int item_1;
    int pattern_type;
    int VCP_num;
    int num_sweeps;
    int item_5;
    int pulse_flag;
    struct nexrad_VCP_items *swp_info;
    struct nexrad_VCP_header *next;
};


