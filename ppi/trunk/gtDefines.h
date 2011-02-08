/******************************************************************************\
DESCRIPTION:
AUTHORS:
        Coherent Technologies,Inc.
        Gary Cox (303)449-8736
CHANGES:
	4-15-92: Created to handle graphicsTools area defines.
\******************************************************************************/
#define LARGE 1
#define SMALL 0
#define MAXLENGTH 113
#define MAXWORDS 32
#define AZIM 1
#define ELEV 0
#define CW 1
#define CCW 0
#define XLARGEFONTSIZE 10
#define XSMALLFONTSIZE 6
#define RDFILECOUNT 32
#define STOP_REMOTE_DISPLAY 0
#define WRITE_REMOTE_FILES 1
#define RDSCALEFACTOR 0.25
#define RDSCALEFACTOR0 0.25
#define RDOFFSET -32.
#define RDOFFSET0 -32.

#define LD_MAX_N_FLDS 4

/*
struct TITLESTRUCT {
	char *Title[G_TITLELENGTH];
	int  *TitleFlag;
};
*/

struct lidar_header{
	char system_name[32];		/* project name */
	int latitude;			/* in 1/100,000 degrees */	
	int longitude;			/* in 1/100,000 degrees */
	int altitude;			/* in meters */
	unsigned short energy;		/* in mJoules */
	unsigned short diameter;	/* optical diameter in cm */
	int focus;			/* focal range in meters */
	unsigned short wavelength;	/* nanometers */
	unsigned short pulselength;	/* nanoseconds */
	unsigned short scanmode;	/* exactly like ray header
					scanmode */
	char notes[62];			/* any additional data or comments */
};

struct processor_header {
	unsigned short mode;		/* processing mode 
					49=Copy, 51=AScope2M, 53=Moments2M,
					62=VAD2M, ...  */
	unsigned short data_src;	/* source of data 0=real live,
					1=simulated, 2=playback */
	char file_name[32];		/* rtlp recorded file name 
					rtlp - realtime lidar processor */
	unsigned short record_mode;	/* 0-raw (impossible here), 1-results,
					2-both	*/
	unsigned short record_count;	/* number of results recording */
	unsigned short atpoints;	/* number of points being 
					acquired by analytek */
	unsigned short npoints;		/* number of points being 
					processed */
	unsigned short nstart;		/* where npoints starts 
					inside atpoints */
	unsigned short navg;		/* number of shots being 
					averaged */
	unsigned short nmonitor;	/* number of points being 
					processed as the monitor 
					channel */
	unsigned short nmonfft;		/* number of points of the
					fft on the monitor channel */
	unsigned short atmode;		/* analytek mode 0 =2 micron,
					1 =1 micron, 3 =cw */
	unsigned short graphmode;	/* graphicsmode 20-25 PPI 
					modes, 30-37 RHI modes */
	unsigned short rangeortime;	/* current display mode range(15) or 
					time(16) , valid for atrgs only*/
	unsigned short azavg;		/* average azimuth in 1/100 
					degrees */
	unsigned short elavg;		/* average elevation in 1/100 
					degrees */
	unsigned short azmin;		/* minimum azimuth in 1/100 
					degrees */
	unsigned short elmin;		/* minimum elevation in 1/100 
					degrees */
	unsigned short azmax;		/* maximum azimuth in 1/100 
					degrees */
	unsigned short elmax;		/* maximum elevation in 1/100 
					degrees */
	unsigned short numofrecords;	/* number of records recorded
					per processing loop */
	float atsampfreq;		/* analytek sampling 
					frequency in MHz 2-500 */
	float attrigdelay;		/* analytek trigger delay in microsecs */
	float attransdelay;		/* analytek trans delay in microsecs */
	float at1stdelay;		/* analytek 1st delay in microsecs */
	float atprf;			/* pulse repetition frequency in Hz */
	float atrgs;			/* range gate separation in 
					meters or seconds */
	int recordlength;		/* number of records for each answer */
	unsigned short numofanswers;	/* number of answers recorded */
	short scale[LD_MAX_N_FLDS];	/* scale factor for each answer *100*/
	short offset[LD_MAX_N_FLDS];	/* offset for each answer *100 */
	unsigned short data_size;	/* data size in number of 
					bits. 8,16 or any number*/
	unsigned short range;		/* range of the center of the
					first gate in meters */
	unsigned short g_width;		/* gate size in 1/16 meters */
	unsigned short g_centers;	/* range between center points of two 
					consecutive gates in 1/16 meters */
	unsigned short n_gates;		/* number of gates */
	unsigned short n_data_points;	/* number of points of each 
					moment to be sent PPP */
	unsigned short filler;		/* alignment filler */
};

struct moments_header {
	unsigned short nfft;		/* samples per range gate */
	unsigned short ngates;		/* number of range gates */
	float fwidth;			/* frequency width factor */
	float athreshold;		/* amplitude threshold */
	unsigned short takelog;		/* output log or not */
	unsigned short sub;		/* spectrum noise averaging and 
					subtraction */
	unsigned short zeropad;		/* zeropad processing points */
	unsigned short procflag;	/* processing mode flag */
	unsigned short procthresh;	/* processing threshold */
	unsigned short numberofflatregions; /* number of regions to
					flatten up to 4 */
	unsigned short minflat[4];	/* minimum gate number to 
					start flattening */
	unsigned short maxflat[4];	/* maximum gate number to 
					start flattening */
};

struct ascope_header {
	unsigned short mode;		/* mode of ascope processing 
					0-regular, 1-snr wideband in dB, 
					2-backscatter (1/msr), 3-log RCB, 
					4-peak detection (range), 
					5-regular smoothed, 
					6-snr wideband with store, 
					7-backscatter with store, 
					8-log RCB with store, 
					9-peak detection (intensity), 
					10-regular smoothed with store */
	unsigned short lolevel;		/* local oscillator noise level */
	unsigned short window;		/* ascope window for averaging */
	unsigned short rmin;		/* minimum range for hard target detect*/
	unsigned short rmax;		/* maximum range for hard target detect */
	unsigned short filler;		/* add space to get 4 byte boundary */
	float extinction;		/* account for extinction factor */
	float pulsewidth;		/* pulewidth in nsec */
	float focus;			/* focal range */
	float calcorrect;		/* calibration correction factor */
	float threshold;		/* intensity threshold */
	float rangestart;		/* starting range */
	float rangeinc;			/* incremental range */
};

struct vad_header {
	unsigned short nfft;		/* samples per range gate */
	unsigned short ngates;		/* number of range gates */
	unsigned short nvad;		/* number of vad scan points */
	unsigned short sub;		/* spectrum noise averaging and 
					subtraction */
	float vadtheta;			/* vad cone half angle */
	float vadthetam;		/* vad cone mean azimuth 
					angle(compass) */
	float vadphim;			/* vad cone mean elevation angle 
					0 degrees horizontal, 90 vertical */
	float fwidth;			/* frequency width factor */
	float athreshold;		/* amplitude threshold */
	unsigned short zeropad;		/* zeropad processing points */
	unsigned short procflag;	/* processing mode flag */
	unsigned short procthresh;	/* processing threshold */
	unsigned short numberofflatregions; /* number of regions to
					flatten up to 4 */
	unsigned short minflat[4];	/* minimum gate number to 
					start flattening */
	unsigned short maxflat[4];	/* maximum gate number to 
					start flattening */
};

/* the mandatory lidar data header */
struct lidar_ray_header {
	unsigned short length;		/* data length in bytes of this ray
					including all headers */
	unsigned char f_cnt;		/* frame number in a volume */
	unsigned char scanmode;		/* 0-PPI, 1-RHI, 2-Coordinate,
					3-stare */
	unsigned int time;		/* in unix time format */
	unsigned int record_number;	/* Record number being recorded */
	unsigned short azavg;		/* average azimuth value
					during shot averaging in 
					1/100 degrees */
	unsigned short elavg;		/* average elevation value during
					shot averaging in 1/100 degrees */
	unsigned short beamwidth_az;	/* average azimuth beamwidth during
					shot averaging in 1/100 degrees */
	unsigned short beamwidth_el;	/* average elevation beamwidth during
					shot averaging in 1/100 degrees */
	unsigned short n_fields;	/* number of fields */
	unsigned short r_h_pt;		/* if non-zero, offset of the optional 
					lidar header, processor header, params
					header */
	unsigned short f_pt[LD_MAX_N_FLDS]; /* offsets of the field data */
};


    /* This structure is used in the convert to/from Unix time routines */
typedef struct {
    int    year,month,day,hour,min,sec;
    int    unix_time;
} date_time_t;

