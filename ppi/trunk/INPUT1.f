c
c----------------------------------------------------------------------X
c
      SUBROUTINE INPUT1(INDAT,IFMT,NETWORK,IRATYP,ICORD,
     X                  X0,Y0,XRD,YRD,H0,AZC,BAZ,ANGXAX,IFORBLK)
C
C     IFMT   - Format of input data file
C     NETWORK- Experiment or network name
C     IRATYP - NAME OF THE RADAR WHOSE MEASUREMENTS ARE TO BE PROCESSED
C     ICORD  - NAME OF THE ORIGIN; EITHER 'EXP ORGN' OR THE NAME OF ANY
C              RADAR IN THE NETWORK (NETWORK), INCLUDING IRATYP
C     XRD,YRD- Coordinates of radar
C     X0,Y0  - Coordinates of radar in rotated system
C     IFORBLK- Input blocking flag (0) COS-blocked, (1) Fortran-blocked
C              Parameter 2: IBLK='F' or ' ' in column 8.
C     AZC    - Correction to azimuth (New azim = Old azim + azcor)
C              Usually AZCOR is the correction to the azimuth angle to 
C              rotate local radar azimuths to be parallel to true north 
C              through origin.  Rotation is done in CONFILL1, CONFILL2,
C              CONTUR, CONVERT, PLTPROJ, and SAMPLOC rather than simply
C              adding AZC to input azimuth angles.  Rotations in these
C              routines requires reversing the sign on AZC.  Input AZC
C              .gt. 0 implies a clockwise rotation of data.
C
C   VARIABLE  DESCRIPTION                  FIELD  NOTES
C   --------  -----------                  -----  -----
C
C   IEXP      EXPERIMENT NUMBER             P5    1- CCOPE (DEFAULT)
C                                                 2- JAWS
C                                                 3- CYCLES
C                                                 4- MAYPOLE-83
C                                                 5- LAKE SNOW-84
C                                                 6- MAYPOLE-84
C                                                 7- PHOENIX-84
C                                                 8- NIMROD-78
C                                                 9- SOCORRO-84
C                                                10- PRESTORM-85
C                                                11- GALE-86
C                                                12- MIST-86
C                                                13- CINDE-87
C                                                14- GERMAN
C                                                15- TAMEX
C                                                16- PROFS
C                                                17- TDWR
C                                                18- HARP90
C                                                19- WISP91
C                                                20- NEXRAD90
C                                                21- NDTP89
C                                                22- CAPE91
C                                                23- FEST92
C                                                24- RAPS92
C                                                25- RAPS93
C                                                26- SCMS95
C
C   NMRAD     RADAR NUMBER                  P6    1 = CHILL
C                                                 2 = CP-2
C                                                 3 = CP-3
C                                                 4 = CP-4
C                                                 5 = NOAA-C
C                                                 6 = NOAA-D
C                                                 7 = NOAA-K
C                                                 8 = SWR-75
C                                                 9 = FL-2
C                                                10 = UND
C                                                11 = NOR
C                                                12 = CIM
C                                                13 = POLD
C                                                14 = TOGA
C                                                15 = CCAA
C                                                16 = SPAN
C                                                17 = MIT
C                                                18 = MHR
C                                                19 = FL-2C
C
C   NRMX - MAXIMUM NUMBER OF RADARS IN THE TABLES
C   NEXP - MAXIMUM NUMBER OF EXPERIMENTS IN THE TABLES
C
      PARAMETER (NRMX=19,NEXP=26)
      CHARACTER*8 INDAT(10)
      CHARACTER*8 NAMES(NRMX),KEXP(NEXP)
      CHARACTER*8 IFMT,NETWORK,IRATYP,ICORD
      CHARACTER*1 IBLK
      DIMENSION HTRAD(NRMX,NEXP),XCOORD(NRMX,NEXP),
     X          YCOORD(NRMX,NEXP),AZCOR(NRMX,NEXP),BASEAZ(NEXP)
      DATA HTRAD/815.,788.,768.,812.,815.,837.,812.,802.,         11*0.,
     2             0.,1602.,1570.,1619.,                          15*0.,
     3                                                            19*0.,
     4             0.,1602.,                                      17*0.,
     5             0.,0.,213.,209.,                               15*0.,
     6             0.,1750.,0.,1540.,                             15*0.,
     7             0.,0.,1598.,1540.,1598.,1513.,                 13*0.,
     8             241.,0.,228.,200.,                             15*0.,
     9             0.,0.,1783.,2136.,3214.,1802.,                 13*0.,
     X             0.,0.,489.,438., 6*0., 370.,413.,               7*0.,
     1             0.,0., 10., 10., 11*0., 100., 30.,              2*0.,
     2             0.,264.,184.,205., 4*0., 202.,213.,             9*0.,
     3             0.,1750.,1620.,0.,1676.,1665.,2*0.,1725.,1604., 9*0.,
     4             12*0.0, 605.,                                   6*0.,
     5             3*0., 9., 9*0., 206., 27.,                      4*0.,
     6             0.,1750.,                                      17*0.,
     7             8*0.,1727.,1603.,                               9*0.,
     8                                                            19*0.,
     9             0.,0.,1.74,14*0.,1.6,                             0.,
     X             17*0.,1.6,                                        0.,
     1             2*0.,1750.,650.,                               15*0.,
     2             0.,9.,12.,12.,4*0.,45.,46.,6*0.,35.,0.,45.          ,
     3             2*0.,423.,351.,                                15*0.,
     4             1420.,1646.,15*0.,1585.,                          0.,
     5             1420.,1646.,15*0.,1585.,                          0.,
     6             9.,                                            18*0./
      DATA XCOORD/26.54,0.,74.91,83.03,53.3,51.77,28.14,2.48,     11*0.,
     2             0.,0.,14.15,10.43,                             15*0.,
     3             0.,0.,7.66,0.,                                 15*0.,
     4                                                            19*0.,
     5             0.,0.,-32.91,0.,                               15*0.,
     6             0.,0.,0.,27.929,                               15*0.,
     7             0.,0.,0.,10.8,0.,4.46,                         13*0.,
     8             57.107,0.,0.,41.273,                           15*0.,
     9             0.,0.,5.775,-19.997,-0.52,6.193,               13*0.,
     X             0.,0.,-27.509, 7*0., 0.,-31.724,                7*0.,
     1             0.,0.,-38.800, 0., 11*0., 2.198, -221.183,      2*0.,
     2             0.,0.,-9.62,0.4, 4*0., 2.42,-8.53,              9*0.,
     3             0.,-27.2,0.,0.,27.62,12.0,0.,0.,11.401,8.664,   9*0.,
     4             12*0.0, 0.0 ,                                   6*0.,
     5             3*0., 0.0, 9*0., -14.661, 45.191,               4*0.,
     6                                                            19*0.,
     7             8*0.,0.0,-2.6,                                  9*0.,
     8             0.,0.,10.56,                                   16*0.,
     9             0.,0.,-37.1,                                   16*0.,
     X                                                            19*0.,
     1             2*0.,-27.2,0.,                                 15*0.,
     2       0.,-3.5,-19.4,0.,4*0.,-57.7,-44.4,6*0.,-62.6,0.,-57.7     ,
     3             2*0.,0.,44.25,                                 15*0.,
     4             10.8,-37.2,                                    17*0.,
     5             10.8,-37.2,                                    17*0.,
     6                                                            19*0./
      DATA YCOORD/52.2,0.,45.76,17.41,27.99,0.42,24.95,-2.15,     11*0.,
     2             0.,0.,-11.19,-25.45,                           15*0.,
     3             0.,0.,-42.4,0.,                                15*0.,
     4                                                            19*0.,
     5             0.,0.,20.82,0.,                                15*0.,
     6             0.,0.,0.,6.321,                                15*0.,
     7             0.,0.,0.,1.4,0.,12.58,                         13*0.,
     8             -18.099,0.,0.,37.795,                          15*0.,
     9             0.,0.,16.640,11.141,0.651,-13.682,             13*0.,
     X             0.,0.,53.373, 7*0., 0.,26.65,                   7*0.,
     1             0.,0.,-14.71, 0., 11*0., 291.593, -138.718,     2*0.,
     2             0.,0.,-22.83,-14.11, 4*0., -21.71,-11.72,       9*0.,
     3             0.,20.9,0.,0.,2.63,-0.02,0.,0.,-7.786,12.656,   9*0.,
     4             12*0.0, 0.0,                                    6*0.,
     5             3*0., 0.0, 9*0., -41.255, 48.173,               4*0.,
     6                                                            19*0.,
     7             8*0.,0.0,20.5,                                  9*0.,
     8             0.,0.,-13.96,                                  16*0.,
     9             0.,0.,8.4,                                     16*0.,
     X                                                            19*0.,
     1             2*0.,20.9,0.,                                  15*0.,
     2             0.,58.4,12.6,0.,4*0.,13.3,22.0,6*0.,29.8,0.,13.3    ,
     3             2*0.,0.,27.36,                                 15*0.,
     4             63.5,8.4,                                      17*0.,
     5             63.5,8.4,                                      17*0.,
     6                                                            19*0./
      DATA AZCOR/-.254,0.,-.725,-.925,-.508,-.499,-.270,-.024,    11*0.,
     2                19*0.,
     3                19*0.,
     4                19*0.,
     5                19*0.,
     6                19*0.,
     7                19*0.,
     8           -0.4536,0.,0.,-0.3337,                           15*0.,
     9            0.0,0.0,0.0,0.0,0.0,1.55,                       13*0.,
     X            0.,0.,0.1945,                                   16*0.,
     1            0.0,0.0,.246,                                   16*0.,
     2                19*0.,
     3            0.0,0.0,0.0,0.0,-1.1,                           14*0.,
     4                19*0.,
     5                19*0.,
     6                19*0.,
     7                19*0.,
     8                19*0.,
     9                19*0.,
     X                19*0.,
     1                19*0.,
     2                19*0.,
     3                19*0.,
     4                19*0.,
     5                19*0.,
     6                19*0./
      DATA BASEAZ/17*0.,142.9,4*0.,58.27,3*0./
      DATA NAMES/'CHL     ','CP2     ','CP3     ','CP4     ',
     X           'NOC     ','NOD     ','NOK     ','SKW     ',
     X           'FL2     ','UND     ','NOR     ','CIM     ',
     X           'POLD    ','TOGA    ','CCAA    ','SPAN    ',
     X           'MIT     ','MHR     ','FL-2C   '/
      DATA KEXP/'CCOPE   ','JAWS    ','CYCLES  ','MAY83   ',
     X          'LAKE SNO','MAY84   ','PHOENIX ','NIMROD  ',
     X          'SOCORRO ','PRESTORM','GALE    ','MIST    ',
     X          'CINDE   ','GERMAN  ','TAMEX   ','PROFS   ',
     X          'TDWR    ','HARP    ','WISP    ','NEXRAD90',
     X          'NDTP    ','CAPE    ','FEST    ','RAPS92  ',
     X          'RAPS93  ','SCMS    '/

      DATA PI,TORAD,TODEG/3.141592654,0.017453293,57.29577951/

c      WRITE(6,9)(INDAT(I),I=1,10)
c 9    FORMAT(1X,10A8)
      READ(INDAT,11)IFMT,IBLK,NETWORK,IRATYP,ICORD,BAZM,XRD,YRD,H0,AZC
 11   FORMAT(/A7,A1/A8/A8/A8/F8.0/F8.0/F8.0/F8.0/F8.0)

C     If input is fortran-blocked (IBLK='F'), set IFORBLK=1; otherwise, 
C     the input is assumed to be COS-blocked (IFORBLK=0)
C     See RD*.f routines.
C
      IF(IBLK.EQ.'F')THEN
         IFORBLK=1
      ELSE
         IFORBLK=0
      END IF

      IEXP=IFIND(NETWORK,KEXP,NEXP)
      IF(IEXP.NE.0)THEN
         WRITE(6,13)NETWORK,IEXP
 13      FORMAT(1X,' REQUESTED NETWORK: ',A8,
     +        ' FOUND NETWORK NUMBER=',I4,
     +        ' IN THE LIST')
      ELSE
         WRITE(6,14)NETWORK
 14      FORMAT(1X,' REQUESTED NETWORK: ',A8,' NOT FOUND IN LIST')
      END IF
      CALL DMPCHAR(KEXP,NEXP)
      IF(ICORD.EQ.'AIRBRNE ')THEN
         X0=XRD
         Y0=YRD
      END IF
      IF(IEXP.EQ.0)THEN
         X0=XRD
         Y0=YRD
         IF(ICORD.EQ.'ORGN    ')ICORD='EXP ORGN'
      END IF
      IRAD=IFIND(IRATYP,NAMES,NRMX)
      IF(IRAD.NE.0)THEN
         WRITE(6,15)IRATYP,IRAD
 15      FORMAT(1X,' REQUESTED RADAR: ',A8,' FOUND RADAR NUMBER=',I4,
     +        ' IN THE LIST')
      ELSE
         WRITE(6,16)IRATYP
 16      FORMAT(1X,' REQUESTED RADAR: ',A8,' NOT FOUND IN LIST')
      END IF
      CALL DMPCHAR(NAMES,NRMX)
      IF(IRAD.EQ.0)THEN
         X0=XRD
         Y0=YRD
         IF(ICORD.EQ.'ORGN    ')ICORD='EXP ORGN'
         GO TO 20
      END IF
      ICOR=IFIND(ICORD,NAMES,NRMX)
C * * * * * * * * * * * * * * *
      IF(IRAD.EQ.ICOR)THEN
         XRD=0.0
         YRD=0.0
      ELSE
         ICORD='EXP ORGN'
         XRD=XCOORD(IRAD,IEXP)
         YRD=YCOORD(IRAD,IEXP)
      END IF
C * * * * * * * * * * * * * * *
      X0=XRD
      Y0=YRD
      H0=0.001*HTRAD(IRAD,IEXP)
      AZC=AZCOR(IRAD,IEXP)

C     IF THE INPUT BAZ .NE. 0, COMPUTE THE RADAR POSITION (X0,Y0) IN
C     ROTATED COORDINATE SYSTEM USING ITS POSITION (XRD,YRD) AND ROT.
C
 20   IF(BAZM.EQ.0.0)THEN
         IF(IEXP.NE.0)THEN
            BAZ=BASEAZ(IEXP)
         ELSE
            BAZ=0.0
         END IF
      ELSE
         BAZ=BAZM
      END IF
      IF(BAZ.EQ.0.0)THEN
         ANGXAX=90.0
      ELSE
         ANGXAX=BAZ-90.0
         ROT=180.0-BAZ
         IF(ANGXAX.LT.0.0)ANGXAX=ANGXAX+360.0
         IF(ANGXAX.GT.360.0)ANGXAX=ANGXAX-360.0
         SINB=SIN(TORAD*ROT)
         COSB=COS(TORAD*ROT)
         X0= XRD*COSB+YRD*SINB
         Y0=-XRD*SINB+YRD*COSB
      END IF
      PRINT 30,IFMT,NETWORK,IRATYP,ICORD,BAZ,ANGXAX,AZC,
     +     XRD,YRD,H0,X0,Y0,H0
 30   FORMAT(
     +/1X,'   DATA FORMAT: ',A8,'  EXP NAME: ',A8,'  RADAR: ',A8,
     +/1X,'  COORD ORIGIN: ',A8,'  BASELINE AZIMUTH:',F6.1,
     +/1X,'   AZIM +X-DIR: ',F6.1,'    ROTATE DATA: ',F6.1,' deg',
     +/1X,'   RADAR COORD: (XRD,YRD,HT)=',3F8.2,
     +/1X,' ROTATED COORD: ( X0, Y0,H0)=',3F8.2,/)

      AZC=-AZC
      RETURN
      END
