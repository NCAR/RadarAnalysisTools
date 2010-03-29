#ifndef THEDATE
#define THEDATE "<20-Jun-1995 17:30:53><UTC>"
#endif
/*
 * NXP3.H -- Header file for C programs using the P3 radar tapes (since 
 * October, 1988).
 */

#ifndef NXP3_H
#define NXP3_H  1

#include <stdio.h>

#include "nxshort.h"

/* Define value to be used in converting raw velocities to signed velocities,
 * using Posititive-is-receding convention, to agree with ground-based
 * radars.
 */
#define NX_POSITIVE_RECEDING 1 /* positive velocities are receding */

/* Define P3 radars from 0 to ... (<= (NX_MAX_RADARS -1))
 */
#define NX_P3_LF    0
#define NX_P3_TA    1

/*
 * Define max record sizes.
 */
#define HEADER_REC_BUFFER_SIZE 2048 /* max bytes in header record */
#define BYTES_FIELD_ENTRY  sizeof (short int)  \
		       /* bytes to store one short-int in field buffer*/
#define FIELD_BUFFER_MAX  NX_MAX_GATES*NX_MAX_FIELDS*BYTES_FIELD_ENTRY \
		   /* bytes in buffer for uncompressed fields */
#define NX_MAX_P3BUFF         8192 /* maxbytes to read from P3 data tape*/


/* Define scale factors for the data fields.
 */
#define NX_FACTOR_P3DZ 128 
#define NX_FACTOR_P3VE 128 
#define NX_FACTOR_P3SW 128 


/*
 * Struct to contain either a pointer to FILE or an integer file descriptor,
 * depending upon whether the /dev file was opened with fopen (for FILE pointer)
 * or with open (for file descriptor: this is UNIX, not ANSI C).
 */
typedef struct
{
    short int   sel;     /*=0 for pointer to FILE, =1 for int file descriptor */
    union
    {   FILE    *fp;
        int      fDesc;
    } U;
} FileOpening; 


/*
 * P3 - More Header Block
 * 
 * This header block contains info not read directly from the P3 raw tape.
 * Some parts of this info are read and/or interactively input.  Other parts
 * are calculated as the program runs.
 */
typedef struct
{  short int     volScan;          /* volume scan                      */
   short int     sweep;            /* sweep number                     */
   short int     sweepNumRead;     /* sweep number read                */
   char          flightIDoverride[9];/*Flight identifier override; only used
                                    *when not null, not all blanks     */
   char          radarName[20];    /* radar name                       */ 
   char          siteName[20];     /* site name                        */ 
   char          fieldTapeName[20];/* field tape name                  */ 
   NxDate        startDate;        /* starting date for this run, UTC  */
   NxTime        startTime;        /* starting time for this run       */
   NxDate        endDate;          /* ending date for this run, UTC    */
   NxTime        endTime;          /* ending time for this run         */
   UInt          sweepMode;        /* sweep mode: using UF defs
                                    * 0:cal, 1:ppi, 2:cop,3:rhi,...    */
   float         startAz;          /* starting azimuth (adjusted for roll
                                      for tail) for this run           */
   float         endAz;            /* ending azimuth (adjusted for roll
                                    * for tail) for this run           */
   short int     sweepDef;         /* New Sweep definition
              = -1 to use Sweep number read, i.e., new sweep if the sweep
                  number read changes.
              = 0 to use big azimuth change, i.e., new sweep if the absolute
                  value of change in azimuth (adjusted for roll for tail) > 180.
                  This can be used to get a new sweep when azimuth changes from
                  359 to 0 (for clockwise sweeping) or it changes from
                  0 to 359 (for counter-clockwise sweeping).  The difference
                  of the azimuths is taken; note that this is different than
                  the change used for sweepDef = 2.
              = 1 to use sector definition of sweep, i.e., there is a new
                  sweep if there is a change of direction of the sweeping.
              = 2 to use a medium azimuth change for new sweep.  This
                  definition will use the degrees of rotation needed to go
                  from old azimuth to new azimuth as the change.  This is
                  different than just taking the difference.  E.g., if
                  the old azimuth is 359 and the new is 1, then the change is
                  2 which is not enough to define a new sweep (absolute value
                  of change must be > 20 degrees).  Going from 22 to 1
                  would cause a new sweep number to be generated.
	      = 3 to use a change in the sign of tilt for new sweep.
              Please note: the rays used with sweepDef in checking for end-of-
                  sweep are only those rays meeting the time "window" and
                  azimuth "window" requirements.  So setting startAz to 30, 
                  endAz to 150, and sweepDef to 0 would result in NO 
                  end of sweeps being seen.                            */
   int           rangeDelCor;      /* range delay correction in meters */
   float         azCor;            /* azimuth correction in degrees    */
   float         elevCor;          /* elevation correction in degrees  */
   float         cantCor;          /* canting angle correction in degrees*/
   float         pitchCor;         /* pitch correction in degrees*/
   short int     PAngFlag;         /* pointing angle correction flag:
                                    * 0: no pointing angle corrections
                                    *    for velocity will be made;
                                    * 1: pointing angle corrections will
                                    *    be made:
                                    *    for contamination
                                    *    due to ground speed when the
                                    *    tilt is not 0., for contamination
                                    *    due to P3's vertical speed for any
                                    *    ray with any vertical component.*/
   short int     SWidthFlag;       /* spectral width flag:
                                    * 0: don't save spectral width data;
                                    * 1: save spectral width data.     */
   short int     TSeriesFlag;      /* time series flag:
                                    * 0: don't do time series (normal);
                                    * 1: do time series (unusual).     */
   char          oneSecFile[81];   /* name of file with one second data
				    * from P3 flight.  This string will
				    * null if no file will be used.    */
   FILE         *fpOneSec;         /* file pointer to file with one second data,
				    * NULL if no file or file not opened.*/
   short int     horBeamWdth;      /* hor beam width deg (X 128)       */
   short int     verBeamWdth;      /* ver beam width deg (X 128)       */
   short int     numThreshs;       /* num thresholds for this field    */
   short int     receiverBandwidth;/* NOTE: kHz X64                    */
   UInt          polarizationXmitted;/* 0:hor 1:vert 2:circ 3:ellip 4:h&v*/
   UInt          polarizationReceivd;/* 0:hor 1:vert 2:circ 3:ellip 4:h&v*/
   short int     radarConstant;    /* units of dBm   (X128)            */
   short int     noisePower;       /* (dBm)  (X128)                    */
   short int     receiverGain;     /* (dBm)  (X128)                    */
   short int     peakPower;        /* (dBm) kw  (X64)                  */
   short int     antennaGain;      /* (dBm)     (X128)                 */
   short int     pulseDuration;    /* micro seconds (X128)             */
   long int      prt;              /* pulse rep time (x128) microsecs  */
   short int     nyquistVel;       /* nyquist velocity (X128) m/s      */
   short int     missingDataFlag;  /* missing data flag for data arrays*/


} P3MoreHb;

/*
 * P3 - General Info Header Block
 *
 * Contents of the first 100 16-bit words read from the most recent
 * P3 Tape Header Record plus a little more.  (see MARS ROS:
 * Prog. Des. Manual 21 Oct 1988 II-1 to II-14, III-20 to III-21 for more info).
 */
typedef struct
{  short int     headerFlag;       /*1Should be 0                      */
   short int     headerLen;        /*2number of bytes in tape header rc==2048*/
   short int     tapeNum;          /*3tape number                      */
   short int     versNum;          /*4Header format version number     */
   short int     resVersNum;       /*5Reserved format version num (0?) */
   short int     year;             /*6Year for Header record           */
   short int     month;            /*7Month for Header record          */
   short int     day;              /*8Day for Header record            */
   short int     hour;             /*9Hour for Header record           */
   short int     minute;           /*10Minute for Header record        */
   short int     second;           /*11Seconds for Header record       */
   char          menuLF[17];       /*12-19File Name for LF Setup Menu  */
   char          menuTA[17];       /*20-27File Name for TA Setup Menu  */
   char          menuDa[17];       /*28-35File Name for data Menu      */
   short int     word36;           /*36Spare                           */
   short int     navSystem;        /*37 0:ONE, 1: INE1, 2:INE2         */
   short int     driveLU;          /*38LU of tape drive recorded on    */
   short int     airCrID;          /*39Aircraft identifier, 42, 43, 0  */
   char          flightID[9];      /*40-43Flight identifier            */
   short int     dataRHLen;        /*44 16-bit words data record header(5)*/
   short int     rayHLen;          /*45 16-bit words ray header (22)   */
   short int     minAhead;         /*46Minutes ahead of GMT            */
   short int     status[8];        /*47-54Power up test results        */
   short int     dispCon[15];      /*55-69Display configuration        */
   short int     words70[10];      /*70-79Words 70-79                  */
   short int     lastRCU;          /*80Last RCU short form status      */
   char          projID[17];       /*81-88Project identifier           */
   short int     words89[12];      /*89-100Spare                       */
/* The rest are not read from the header record, but are input somehow */
   short int     MFilesFlag;       /* read-multiple-file flag:       
                                    * 0: only one file will be read;  
                                    * >1: multiple files will be read if 
                                    *    necessary until end time reached
				    *    or MFilesFlag successive EOF's */
   short int     thisRadar;        /* = NX_P3_TA or NX_P3_LF            */
   short int     radarMode;        /* = 0 to only accept LF rays, = 1 to
				    * accept only TA rays, =2 to accept both*/
   short int     badRecsAcceptable;/* number of bad records that can be
				    * read before job abort            */
   short int     bytesP3Buff;      /* bytes in P3 buffer, >=0.
				    *    =0 forces another record read */
   short int     headerRead;       /* 0: no header record read (or there
				    *    is reason like tape EOT or 
				    *    repositioning to demand new header
				    * 1: P3 header record has been read.*/
   short int     readRadHeaders;   /* 0: when header record is read only
				    *    save radar header info for radar(s)
				    *    for which rays are sought.
				    * 1: save both LF and TA radar headers
				    *    when header record read.
				    * Note: saving either radar header
				    *  means allocation for space for 
				    *  a P3RadInHb structure should have been
				    *  made.                            */
   short int     reasonStopped;    /* When reading for another P3 ray has
				    * stopped, there could be one of several
				    * reasons.
				    * =0, reason unknown or N.A.,
				    * =1, end time/date passed,
				    * =2, end of file hit,
				    * =3, end of information hit,
				    * =4, user interrupted.             */
} P3GenInHb;

/*
 * P3 - Radar Information Header Block
 *
 * Contents of the block of 300 16-bit words read from the most recent
 * P3 Tape Header Record.  For LF (lower fuselage) this is from words 101-400
 * and for TA (tail) this is from words 401-700.  (see MARS ROS:
 * Prog. Des. Manual 21 Oct 1988 II-1 to II-14, III-20 to III-21 for more info).
 */
typedef struct
{         /* Signal processor configuration */
   short int     sampSize;         /*+001Number of samples per meas    */
   short int     dspFlags;         /*+002  Bit0: range normalization
                                    *      Bit1: doppler channel speckle remover
                                    *      Bit2: log channel speckle remover
                                    *      Bit3: pulse at end of ray
                                    *      Bit4: pulse at beginning of ray
                                    *      Bit5: ?
                                    *      Bit6: use AGC (TA only)
                                    *      Bit7: ?
                                    *      Bits8-9:  0:single PRF,
                                    *                1:dual PRF 2/3,
                                    *                2:dual PRF 3/4     
                                    *      Bits10-15: ?                */
   short int     refSlope;         /*+003Reflectivity slope
                                    *           1/4096 of dB/ A/D count*/
   short int     refNThresh;       /*+004Reflectivity noise threshold
                                    *           1/16 dB above noise    */
   short int     clutCThresh;      /*+005Clutter correction threshold
                                    *           signed 1/16 dB         */
   short int     sqiThresh;        /*+006SQI threshold
                                    *           same units as DSP      */
   short int     powerThresh;      /*+007Power threshold for width   
                                    *           signed 1/16 dBZ        */
   short int     calibRef;         /*+008Calibration reflectivity    
                                    *           1/16 dBZ               */
   short int     decayCode;        /*+009AGC decay code                */
   short int     stabDelay;        /*+010Dual-PRF stabilization delay  */
   short int     flagsURef;        /*+011Threshold flags for uncorrected
                                    *           reflectivity           */
   short int     clutFilter;       /*+012Reserved for systems with
                                    *           clutter filters        */
   short int     flagsVel;         /*+013Threshold flags for velocity  */
   short int     flagsWid;         /*+014Threshold flags for width     */
   short int     dataMode;         /*+015Data mode: 1:processed data
                                    *               2:time series      */
   short int     wordsP16[5];      /*+016-020Spare                     */
   short int     wordsP21[20];     /*+021-040For systems w/2 receivers */
            /* Range mask configuration */
   short int     rangeKm;          /*+041Range first bin, kilometer
                                    *    portion(see word+046)        
                                    * (in version 0 this was in meters!)*/
   short int     varRange;         /*+042Flag for variable range bin
                                    *    spacing (0:fixed, 1:variable) */
   short int     binSpacing;       /*+043Step between input bins(meters)*/
   short int     inBins;           /*+044Number of input bins           */
   short int     rangeAve;         /*+045Range averaging state (1,2,3,4)
                                    *(undefined in variable range mode)*/
   short int     rangeDm;          /*+046Range first bin, fractional
                                    * portion, decimeters(see word+041)*/    
   short int     wordsP47[2];      /*+047-48Reserved for addition specs*/
   short int     outBins;          /*+049Number of output range bins,
                                    *  should be 512 for variable range*/
   short int     wordP50;          /*+050Spare                         */
            /* Noise sample information */
   short int     wordsP51[10];     /*+051-60Noise sample info          */
            /* DSP Diagnostics */
   short int     wordsP61[10];     /*+061-70DSP diagnostics            */
            /* Miscellaneous information */
   short int     wordP71;          /*+071Spare                         */
   short int     waveLen;          /*+072Wavelength in 1/100 of cm     */
   short int     pulseWidth;       /*+073Pulse width in 1/100 microsecs*/
   short int     prf;              /*+074PRF                           */
   short int     clutNum;          /*+075For clutter filter number     */
   short int     dssFlag;          /*+076Digital Signal Simulator Flag
                                    *   0:off, 1:on                    */
   short int     trNum;            /*+077T/R number                    */
   short int     transPow;         /*+078Transmit Power                */
   short int     gainCon;          /*+079Gain control flag 0:full gain
                                    *           1:STC, 2:AGC           */
   short int     wordsP80[11];     /*+080-90Spare                      */
            /* Antenna Scanning Information */
   short int     scanMode;         /*+091Antenna scan mode             */
   short int     sweepSp;          /*+092Antenna sweep speed,
                                    *    in 1/10 of RPM, signed        */
   short int     nomTiltAng;       /*+093 nominal tilt angle,
                                    * degreesX128 (even though it was
                                    * stored on tape as "binary angle")*/
   short int     sectCen;          /*+094Sector Center, degrees        */
   short int     sectWidth;        /*+095Sector Width, degrees         */
   short int     wordsP96[5];      /*+096-100Spare                     */
            /* Real Time Display Color Configuration & Other */
   short int     wordsP101[200];   /*+101-300Display color config & spare*/
} P3RadInHb;

/*
 * P3 - Data Header Block
 */
typedef struct
{  short int     fieldsThisRay;    /* fields this ray to be saved 
                                    *  <= fields available              */
   short int    *bufferSt;         /* pointer to start of buffer which
                                    * holds the data for the fields     */
   struct
   {  char       fieldName[3];     /* "VE", "DZ", "SW"                  */
      short int *fdPos;            /* pointer to field data             */
      short int  scaleFactor;      /* field data scale factor           */
   }
              fieldArray[NX_MAX_FIELDS];
} P3DataHb;


/*
 * P3 - Ray Header Block
 * All except pressureAltitude, tailTransVel, and tailVertVel can be found
 * or derived from the ray header in the P3 data record (with the help
 * from the time zone in the Header record).
 */
typedef struct
{  short int word1;                /* word 1 (from 1-22) of ray header   */
   short int code;                 /* high byte word 2 of ray header     */
   short int rayCode;              /* high byte word 4 of ray header     */
   NxDate    date;                 /* date                   UTC=GMT     */
   NxTime    time;                 /* time                   UTC=GMT     */
   long  int rawAzimuth;           /* raw azm, deg (X128) raw values diff*/
   short int rawElevation;         /* raw elv, deg (X128) than grnd based*/
   long  int compassHeading;       /* degrees (X128)                     */
   long  int drift;                /* degrees (X128)                     */
   long  int roll;                 /* degrees (X128                      */
   long  int pitch;                /* degrees (X128)                     */
   long  int corAzimuth;           /*      corrected azimuth angle (X128)
                                    * roll-corrected for tail,
                                    * heading-corrected for LF           */
   long  int tilt;                 /* pitch/drift-corrected ele angl X128*/
   long  int groundSpeed;          /* horiz speed along track (m/s X128) */
   short int verticalSpeed;        /* vertical speed (m/s X128)          */
   long  int latitude;             /* latitude of aircraft degX10000     */
   long  int longitude;            /* longitude of aircraft degX10000    */
   short int radarAltitude;        /* ac height above ground (m)         */
   short int pressureAltitude;     /* ac height above sea level (m)      */
   short int tailTransVel;         /*(m/s X128) H comp (L-R) rel tail vel*/
   short int tailVertVel;          /*(m/s X128) Z comp (Up-D)rel tail vel*/
   long  int windDir;              /* degrees (X128)                     */
   short int windSpeed;            /* m/s (X128)                         */
} P3RayHb;


typedef struct
{
   P3RadInHb  * radIn;
   P3MoreHb   * more;
   P3DataHb   * data;
   P3RayHb    * ray;
} P3Radar;


/*
 * Typedef structure to pass info for files/tapes.
 */

typedef struct {
    char * fileName;  /* file or tape device file name, NULL if not opened */
    FileOpening fileOpening; /*struct containing info if fileName is not NULL*/
    char   recBuffer[FIELD_BUFFER_MAX];/* if non-NULL this contains buffer for
			  all fields' data for most recent logical record*/
    int    recBufferLen; /* if recBuffer is non-NULL,
                        then = number of characters in recBuffer,
                        otherwise recBufferLen = 0*/
    char   headerRecBuffer[HEADER_REC_BUFFER_SIZE];/* if non-NULL this contains
			most recent p3header record or header record read in
                        as "starter" header record*/
    int    headerRecBufferLen; /* if headerRecBuffer is non-NULL,
                        then = number of characters in headerRecBuffer;
                        otherwise headerRecBufferLen = 0*/
    P3GenInHb *genIn; /* for general info data fields from header rec */
    P3Radar   Radar[NX_MAX_RADARS]; /* more info for the P3 radars */
} P3FileInfo;



/* function prototypes */
char * sourceInfo_nxp3();
int  getNxStartEndDT(char * string,NxStartEndDT * dt);
int  HeaderRecCorUnload (char *buffpt,int *goodGates, int *maxGates, int *PAcor,
	 float *velCor, P3RayHb *ray,P3MoreHb *more, P3RadInHb *radIn,
	 P3GenInHb *genIn, short int minAhead);
int  P3RayHeaderUnload (char *buffpt,P3RayHb *ray,P3MoreHb *more,
         short int radar, short int minAhead);
#endif
/* do not add after this line */
