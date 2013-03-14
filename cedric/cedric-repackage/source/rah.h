/*
 * File rah.h: general C definitions and 
 * and many function prototypes for librah_C
 * used by Robert A. Hueftle, NOAA/NSSL, at NCAR/MMM, Boulder,
 * and friends.
 */

#ifndef THEDATE
#define THEDATE "<09-Dec-1997 23:18:58><UTC>"
#endif


#ifndef RAH_HEADER
#define RAH_HEADER
#include <stdio.h> 
#include "nxp3.h"

/*********global variables and definitions *******************/
#define impossibleExit(exp) \
    { \
    fprintf(stderr,"Programming error %s, File %s Line %d " \
         " compiled %s  %s !!\n", \
         exp, __FILE__,__LINE__,__DATE__,__TIME__); \
    exit(1); \
    }
#define BYTE_STORAGE_DAT 0 /* 0 for p3rams .dat stored HiLo, 1 for LoHi */
#define FORTRAN_OVERHEAD 4 /* Bytes in HP Fortran rec-len-flag which \
			      surrounds each "normal" logical record*/
#define FORTRAN_INT 4 /* There are 4 bytes in the default Fortran integers */
#define FORTRAN_FLOAT 4 /* There are 4 bytes in the default Fortran Reals */
#define KM_DEG   111.19        /* km per degree at equator */
#define MAXIMUM(A,B) ((A) >= (B) ? (A) : (B))
#define MINIMUM(A,B) ((A) <= (B) ? (A) : (B))
#define NEAREST_INT(A) ((int) ((A) +  ((A)>= 0 ? .5 : (-.5))))
#define PEN_DOWN 2             /* for plotting routines: move with pen down */
#define PEN_UP   3             /* for plotting routines: move with pen up */
#define PI 3.1415926535897  
#define PI180 PI/180.0           /* to convert degrees to radians */
#define PI_HALF  PI * 0.5
#define DEG_TO_RAD (PI/180.0)       /* to convert degrees to radians */
#define RAD_TO_DEG (180.0/PI)       /* to convert radians to degrees */

typedef int FortranInt; /* sizeof FortranInt should = FORTRAN_INT!! */
typedef FortranInt FortranLogical; /* Fortran logical uses Fortran integer */
typedef int FortranOverhead; /* sizeof FortranOverHead \
				should = FORTRAN_OVERHEAD!! */
typedef float FortranFloat; /* sizeof FortranFloat  should = FORTRAN_FLOAT */


/*
 * Typedef structure to pass info taken from one flight (lat-long-by-seconds)
 * file (Note:  much of the info in flight file not stored here).
 */
typedef struct {
    char    * fileName;    /* name of file read for info */
    int       aircraft;    /* = 42 for P3 "H", 43 for P3 "I", 0 if unknown */
    long int  seconds;     /* number of seconds from start to finish: if
			      seconds >0, then the following lat/long
			      arrays should be malloc'ed for seconds +1 */
    float   * lat;         /* malloc'ed array for latitude values every second*/
    float   * lon;         /* malloc'ed array for longitude values every sec*/
    NxStartEndDT seDT;     /* start/end date/times */
} FlOneInfo;


/*
 * Typedef structure to allow circularly-linked list for FlOneInfo structs.
 */
typedef struct flNode * FlNodePtr;
typedef struct flNode {
    FlOneInfo inf;
    FlNodePtr previous;  /* last node if beginning of list */
    FlNodePtr next;      /* first node if end of list */
} FlNode;

/*
 * Typedef structure for circularly-linked list for FlOneInfo structs.
 */
typedef struct {
    FlNodePtr first;     /* points to first FlNode in list, NULL if null list*/
    int       count;     /* number of nodes in circular linked list headed by
				 FlNode pointed to by first */
} FlInfo;

/*
 * Typedef structure to pass info taken from one geography file, and
 * applied to a drawing area for a particular grid.  This allows quicker
 * replotting than if all the points from the geography file are kept.
 */
typedef struct {
    char    * fileName;    /* name of file read for info */
    long int  points;      /* number of points from start to finish: if
			      points >0, then the following lat,lon,pen
			      arrays should be malloc'ed for points */
    float   * lat;         /* latitude */
    float   * lon;         /* longitude */
    short int * pen;       /* pen position is PEN_UP or PEN_DOWN */
} GeoOneInfo;

/*
 * Typedef structure to allow circularly-linked list for GeoOneInfo structs.
 */
typedef struct geoNode * GeoNodePtr;
typedef struct geoNode {
    GeoOneInfo inf;
    GeoNodePtr previous;  /* last node if beginning of list */
    GeoNodePtr next;      /* first node if end of list */
} GeoNode;

/*
 * Typedef structure to containing circularly-linked list for 
 * GeoOneInfo structs.
 */
typedef struct {
    GeoNodePtr first;    /* points to first GeoNode in list, NULL if null list*/
    int       count;     /* number of nodes in circular linked list headed by
				 GeoNode pointed to by first */
} GeoInfo;



typedef struct {
/*
 * These definitions should agree with the hdr file radar records (2,...)
 * written by program windsyn.
      Write (3) Time_Current1(i), Nameif(i),
     #       Imax1(i), Jmax1(i), Kmax1(i),
     #       Sx1(i), Sy1(i), Sz1(i), Flid1(i), Olat1(i), Olon1(i),
     #       Z01(i), (Itime_Limits1(l,i),l=1,12), Nmosm1(i),
     #       (Init_Time1(l,i),l=1,6), Su1(i), Sv1(i), Project1(i),
     #       Rotcr(i), Eloff(i), Cant(i), Namdf1(i), Xniq1(i),
     #       Rlat1(i), Rlon1(i), Type1(i), IDum, IDum, IDum, IDum

 */
#define file_time_len    24  /* undefined at end of this typedef */
#define flightId_len     8   /* undefined at end of this typedef */
#define name_file_len    50  /* undefined at end of this typedef */
#define project_len      16  /* undefined at end of this typedef */
#define type_len         3   /* undefined at end of this typedef */
    char           file_time[file_time_len +1]; /* +1 for NULL */
    char           namei_file[name_file_len + 1];     /* +1 for NULL ; 
                                   for name of interpreted radar file,
				   e.g., from fast_interp (.for) */
    FortranInt     imax;
    FortranInt     jmax;
    FortranInt     kmax;
    FortranFloat   sx;
    FortranFloat   sy;
    FortranFloat   sz;
    char           flightId[flightId_len +1]; /* +1 for NULL */
    FortranFloat   olat;
    FortranFloat   olon;
    FortranFloat   z0;
    FortranInt     timeLimits[12]; /* yy mm dd hh mm ss start & end of data*/
    FortranInt     nmosm;
    FortranInt     initTime[6];/* time reference for storm motion yymmddhhmmss*/
    FortranFloat   su;
    FortranFloat   sv;
    char           project[project_len +1]; /* +1 for NULL */
    FortranFloat   rotcr;
    FortranFloat   eloff;
    FortranFloat   cant;
    char           named_file[name_file_len + 1];     /* +1 for NULL ; 
                                   for name of ".cor" file, i.e., with
				   unfolded polar coordinate radar data */
    FortranFloat   xniq;
    FortranFloat   rlat;
    FortranFloat   rlon;
    char           type[type_len +1]; /* +1 for NULL */
/*
 * These next data items are not read directly but are derived from t
 * previous data fields.
 */
    int            aircraft; /* = 42 for H, 43 for I, 0 if unknown */
    NxStartEndDT   seDT;     /* start/end date/times (from timeLimits) */
    NxDate         cDate;    /* central date (in reference to storm motion) */
    NxTime         cTime;    /* central time (in reference to storm motion) */
#undef file_time_len
#undef flightId_len
#undef name_file_len
#undef project_len
#undef type_len
} HdrRdrOneInfo;


/*
 * Typedef structure to allow circularly-linked list for HdrRdrOneInfo structs.
 */
typedef struct hdrRdrNode * HdrRdrNodePtr;
typedef struct hdrRdrNode {
    HdrRdrOneInfo inf;
    HdrRdrNodePtr previous;  /* last node if beginning of list */
    HdrRdrNodePtr next;      /* first node if end of list */
} HdrRdrNode;

/*
 * Typedef structure to containing circularly-linked list for 
 * HdrRdrOneInfo structs.
 */
typedef struct {
    HdrRdrNodePtr first;    /* points to first HdrRdrNode in list,
			       NULL if null list*/
    int       count;     /* number of nodes in circular linked list headed by
				 HdrRdrNode pointed to by first */
} HdrRdrInfo;

typedef struct {
    char * fileName;  /* file name, NULL if none */
    FILE * fPtr;      /* pointer to stream if file is opened or NULL */
    char * fileBuffer;/* if non-NULL this contains image of file; if the
			 programmer is modifying the file then the contents
			 of this buffer can be different from the
			 file on disk: see fileBufferModified*/
    int    fileBufferLen; /* if fileBuffer is non-NULL,
			then = number of characters in fileBuffer.
                         fileBufferLen is undefined if fileBuffer is NULL*/
} WholeFile;


/*
 ******************* Function prototypes *********************
 *  These are the routines in librah_C except for the ciof routines and
 *    the ioHP routines.
 */
int       aircraftNumber(char acChar);
char      *allocDirname(char * string);
FlOneInfo * allocFlInfo(FlInfo * flInfo,char * fileName,long int entries,
             int debug);
GeoOneInfo * allocGeoInfo(GeoInfo * geoInfo,char * fileName,long int entries,
             int debug);
HdrRdrOneInfo * allocHdrRdrInfo
          (HdrRdrInfo * hdrRdrInfo, int debug);
void      angleCoverage(float startTrigAngleDeg, float angleChange,
		    float * minx, float * miny, float * maxx, float * maxy);
int       arch2Lookup(char * stationID, int  * wban, char ** stationCity,
                char ** stationState, float * latitude, float * longitude, 
                float * elevationM, float * towerM);
void      azConXYZlf (float Azm,float  El,float Track,float Drift,float Pitch,
               float Roll,float * GrAz,float * GrEl,float * Xval,
               float * Yval,float * Zval);
void      azConXYZta (float Azm,float El, float Track,float Drift,float Pitch,
                 float * GrAz,float * GrEl,float * Tilt,
                 float * Xval,float * Yval, float * Zval);
double    azDif(double arg1, double arg2, double maxDif);
char      *basename(char * string);
void      binaryList(int ii, int pos, int len, char * string);
int       clipSegIn(int * i1,int * j1, int * i2,int * j2, int imin,int imax,
              int jmin,int jmax);
short int conP3BinAng(short int, short int);
int convertASCII(char string[], int bytes, char subchar);
unsigned long expandHex(unsigned long hex, int hexChars, int hexMaxChars);
float     floatP3BinAng(short int );
int       fprintfASCII(FILE * stream,char string[],
          int bytes, char subchar);
void      freeFlNode(FlInfo * flInfo,FlNode * removePtr, int debug);
void      freeHdrRdrNode(HdrRdrInfo * hdrRdrInfo,HdrRdrNode * removePtr,
             int debug);
void      freeLastFlNode(FlInfo * flInfo, int debug);
void      freeLastHdrRdrNode(HdrRdrInfo * hdrRdrInfo, int debug);
void      freeGeoNode(GeoInfo * geoInfo,GeoNode * removePtr, int debug);
short int getBinFixed(short int *fieldPtr, long bin1MDist, long thisMDist,
		     short int binSpacing,short int bins,short int missingData);
short int getBinVariable(short int *fieldPtr, long bin1MDist,
		     long thisMDist, short int missingData );
int       getHHMMSS(char * string,int * hh,int * mm,int *ss);
float     getInterp(float val1, float val2,
	      float weight,float baddata, int decibel);
float     getInterp4(float val11, float val12, float val21, float val22,
	      float weightX, float weightY,float baddata,int needPoints,
	      int decibel);
int       getLatLongASC(char * string,double * lat, double * lon);
int       getLatLongDat(char * string,double * lat, double * lon,
              int byteStorageDat);
short int GetP3DZ(short int *,short int, short int,short int);
short int GetP3SW(short int *,short int, short int, float,short int);
short int GetP3VE(short int *,short int, short int, float, short int,
            float, float, float, float, short int, short int);
int       getStartNxStartEndDT(char * string,NxStartEndDT * dt);
short int getTrimLine(FILE *fp,char *buffer,int length, char ignore);
int       getYYYYMMDDHHMMSSdat(char * buffer,int * yyyy,int * mm,
            int * dd,int * hh,int *  mmin,int * ss,int byteStorageDat);
int       icheck (int  i,int j,int imin,int imax,int jmin,int jmax);
void      iswap (int * i1, int * i2);
int       jul70ymd(int jul70, int * year, int * month, int * day, 
                   int * julianDay);
int       julDay(int year, int month, int day, int * julianDay);
int       julDayFlightYYYYMMDD(int julDay, char *flightID,int *tYear, 
	     int *tMonth, int *tDay);
int       julDayMMDD(int julDay, int year, int *tMonth, int *tDay);
void      latLonLabels(int minutes, char positive, char negative,
                  int  stepMinutes, char degStr[], char minStr [] );
void      latLongTOij(long int points, float lat[],float lon[],
            int i[],int j[], int i0, int j0, float lat0,float lon0,
            float latAve, int yInverse, int accurLat,
            float gridX, float gridY, float sx, float sy,
            long int seconds0, float uMotion, float vMotion);
void      latLongTOijpen(long int points, float lat[],float lon[],
            short int penLL[], int i[],int j[], short int penIJ[],
            int i0, int j0, float lat0,float lon0,
            float latAve, int yInverse, int accurLat,
            float gridX, float gridY, float sx, float sy);
int       mallocateAddString(char ** stringPtr, char * addString);
float     maxClippedRadius(float minx, float miny, float maxx, float maxy);
int       millisecsHMSM(int dayMilli, int * hours, int * minutes, int * seconds,
          int * milliSecs);
void      NxAbort (int abortNumber,char * module,char * message);
void      NxChangeTime (NxDate *,NxTime *,short int);
void      NxChangeTimeSeconds (NxDate *date,NxTime *time,long int change);
int       NxFullYear (int year);
short int NxOkTime (NxDate, NxTime);
long int  NxSecondsDiff2_1 (NxDate date1, NxTime time1, NxDate date2,
            NxTime time2);
short int NxTimeOnBefore (NxDate, NxTime, NxDate, NxTime);
int       P3HeaderGenUnload (char *,P3GenInHb *,int );
int       P3HeaderRadUnload (char *,P3RadInHb *,int );
int       P3RayHeaderUnload (char *,P3RayHb *,P3MoreHb *,short int,short int);
void      packSInt (char *,short int [],int,int);
void      packInt (char *bufpt,int Int[],int count, int swap);
void      packLInt (char *bufpt,long int LInt[],int count, int swap);
void      packSInt (char *bufpt,short int SInt[],int count, int swap);
long int  secondsDiff2_1(
             int year1,int month1,int day1,int hour1,int minute1,int second1,
             int year2,int month2,int day2,int hour2,int minute2,int second2);
int       sscanfFInt(char * string, int skip,int grab, int * integerR,
	     int defaultInt);
char      stdY_N (char *,char );
int       storeFlInfoASC(FlOneInfo * flOnePtr, char * buffer,
             int     bytesLine, int lineCount,  int debug);
int       storeFlInfoASCfast(FlOneInfo * flOnePtr, char * buffer,
             int     bytesLine, int lineCount,  int debug);
int       storeFlInfoDatFast(FlOneInfo * flOnePtr, char * buffer,
             int bytesRecord, int recordCount, int debug,int byteStorageDat);
int       strlenTrim(char * string);
void      strncpyC_F(char * dest, int destlen, char * ct);
void      strncpyF_C(char * dest, char * ct,int ctlen);
char      *strtoupper (char *string);
void      switchBytesInt(int * target);
void      switchBytesLongInt(long int * target);
int       tooClose(int x1,int y1,int x2, int y2, int minDistance);
char      *trailingBlanks(char * string);
void      trimBlanks(char string[]);
int       withinWindow(float latC,float longC,float lat1,float long1,
             float lat2,float long2, int cross180);
#endif /* End of "#ifndef RAH_HEADER" */
/* 
 * Don't add lines after this.
 */
