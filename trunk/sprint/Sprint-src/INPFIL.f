      SUBROUTINE INPFIL(KRD,INPTST,IAZFLG,AZCOR,USER_DEF_ORIGIN,
     X                  RADAR_CHOSEN,IROV)
C
C     IROV= (-1) APPEND, (0) NORMAL PROCESSING, (1) RUNOVER
C
C+++++LISTING ENABLED+++++
C     CDIR$ LIST
C
C        FILLS COMMON /CINP/
C        FORMAT OF INP CARD
C      
C   VARIABLE  DESCRIPTION                  FIELD  NOTES
C   --------  -----------                  -----  -----
C
C   KOMM      'INP'                         P1    COMMAND
C   IUN       UNIT NUMBER                   P2
C   ITAP      TAPE NAME                     P3
C   ISKP      FILES TO SKIP BEFORE READING  P4
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
C                                                18- HARP
C                                                19- WISP90
C                                                20- WISP91
C                                                21- CAPE
C                                                22- FEST92
C                                                23- TOGA COARE
C                                                24- TRMM
C                                                25- DENVER
C                                                26- WISP94
C                                                27- SCMS
C                                                28- MAP
C                                                29- NONE
C
C   NMRAD     RADAR NUMBER                  P6    1 = CHILL
C                                                 2 = CP-2 (DEFAULT)
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
C                                                19 = ELDR-AFT
C                                                20 = ELDR-FOR
C                                                21 = MELB
C                                                22 = DARW-TOGA
C                                                23 = DARW-LAS
C                                                24 = CAMANO
C                                                25 = WICHITA
C                                                26 = KFTG (Denver)
C                                                27 = KWAJ
C                                                28 = LEMA
C                                                29 = NONE
C
      INCLUDE 'SPRINT.INC'
c      PARAMETER (MAXEL=150,NID=129+3*MAXEL)

C     NEXP - Number of actual experiments in the table, not including 'NONE'
C     NRMX - Number of actual radars in the table, not including 'NONE'
C
      PARAMETER (NEXP=28,NRMX=28)
      DIMENSION HTRAD(NRMX,NEXP),XCOORD(NRMX,NEXP),YCOORD(NRMX,NEXP),
     X     AZCORT(NRMX,NEXP)
      CHARACTER*4 NAMES(NRMX+1)
      COMMON /CINP/IUN,ISKP,ORLAT,ORLON
      COMMON /CINPC/ NMRAD,ITAP,TAPE_UNIT
      CHARACTER*4 NMRAD
      CHARACTER*8 ITAP
      CHARACTER*8 TAPE_UNIT
      COMMON /AIRBRN/ ALATS(MAXEL),ALONS(MAXEL),IALTFLG,THE_TILT
      COMMON /IDBLK/ID(NID)
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X     ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      CHARACTER*8 KRD(10)
      INTEGER USER_DEF_ORIGIN

C        1         2         3         4         5         6         7
C--------0---------0---------0---------0---------0---------0---------0--
      DATA HTRAD/815.,788.,768.,812.,815.,837.,812.,802.,         20*0.,
     2             0.,1602.,1570.,1619.,                          24*0.,
     3                                                            28*0.,
     4             0.,1602.,                                      26*0.,
     5             0.,0.,213.,209.,                               24*0.,
     6             0.,1750.,0.,1540.,                             24*0.,
     7             0.,0.,1598.,1540.,1598.,1513.,                 22*0.,
     8             241.,0.,228.,200.,                             24*0.,
     9             0.,0.,1783.,2136.,3214.,1802.,                 22*0.,
     X             0.,0.,489.,438., 6*0., 370.,413.,              16*0., 10th Exp
     1             0.,0., 10., 10., 11*0., 100., 30.,             11*0.,
     2             0.,264.,184.,205., 4*0., 202.,213.,            18*0.,
     3             0.,1750.,1620.,0.,1676.,1665.,2*0.,1725.,1604.,18*0.,
     4             12*0.0, 605.,                                  15*0.,
     5             3*0., 9., 9*0., 206., 27.,                     13*0.,
     6             0.,1750.,                                      26*0.,
     7             8*0.,1727.,1603.,                              18*0.,
     8                                                            28*0.,
     9             2*0.,1646.,14*0.,1585.,                        10*0.,
     X             1420.,1646.,1646.,1646.,5*0.,1505.,7*0.0,1585.,10*0., 20th Exp
     1             0.0,9.0,12.0,12.0,4*0.0,45.0,46.0,6*0.0,35.0,  11*0.,
     2             2*0.0,423.0,351.0,                             24*0.,
     3                                                            28*0.,
     4             20*0.,34.,30.,40.,                              5*0.,
     5             20*0.,34.,0.,0.,184.,                           4*0.,
     6             1420.,2*0.,1457.,2*0.,1503.,18*0.,1710.,        2*0.,
     7             0.0,9.0,0.0,0.0,4*0.0,0.0,0.0,6*0.0,0.0,       11*0.,
     8                                                            28*0./ 28th Exp
C        1         2         3         4         5         6         7
C--------0---------0---------0---------0---------0---------0---------0--
      DATA XCOORD/26.54,0.,74.91,83.03,53.3,51.77,28.14,2.48,     20*0.,
     2             0.,0.,14.15,10.43,                             24*0.,
     3             0.,0.,7.66,0.,                                 24*0.,
     4                                                            28*0.,
     5             0.,0.,-32.91,0.,                               24*0.,
     6             0.,0.,0.,27.929,                               24*0.,
     7             0.,0.,0.,10.8,0.,4.46,                         22*0.,
     8             57.107,0.,0.,41.273,                           24*0.,
     9             0.,0.,5.775,-19.997,-0.52,6.193,               22*0.,
     X             0.,0.,-27.509, 7*0., 0.,-31.724,               16*0., 10th Exp
     1             0.,0.,-38.800, 0., 11*0., 2.198, -221.183,     11*0.,
     2             0.,0.,-9.62,0.4, 4*0., 2.42,-8.53,             18*0.,
     3             0.,-27.2,0.,0.,27.62,12.0,0.,0.,11.401,8.664,  18*0.,
     4             12*0.0, 0.0 ,                                  15*0.,
     5             3*0., 0.0, 9*0., -14.661, 45.191,              13*0.,
     6                                                            28*0.,
     7             8*0.,0.0,-2.6,                                 18*0.,
     8             2*0.0,10.56,                                   25*0.,
     9             2*0.0,-37.2,                                   25*0.,
     X             10.8,-37.2,-37.2,-37.2,5*0.0,36.8,             18*0., 20th Exp
     1             0.,-3.5,-19.4,0.,4*0.,-57.7,-44.4,6*0.0,-62.6, 11*0.,
     2             2*0.0,0.0,44.25,                               24*0.,
     3                                                            28*0.,
     4                                                            28*0.,
     5                                                            28*0.,
     6             -23.7277,2*0.,0.,2*0.,-57.7503,18*0.,-16.1716,  2*0.,
     7                                                            28*0.,
     8                                                            28*0./ 28th Exp
C        1         2         3         4         5         6         7
C--------0---------0---------0---------0---------0---------0---------0--
      DATA YCOORD/52.2,0.,45.76,17.41,27.99,0.42,24.95,-2.15,     20*0.,
     2             0.,0.,-11.19,-25.45,                           24*0.,
     3             0.,0.,-42.4,0.,                                24*0.,
     4                                                            28*0.,
     5             0.,0.,20.82,0.,                                24*0.,
     6             0.,0.,0.,6.321,                                24*0.,
     7             0.,0.,0.,1.4,0.,12.58,                         22*0.,
     8             -18.099,0.,0.,37.795,                          24*0.,
     9             0.,0.,16.640,11.141,0.651,-13.682,             22*0.,
     X             0.,0.,53.373, 7*0., 0.,26.65,                  16*0., 10th Exp
     1             0.,0.,-14.71, 0., 11*0., 291.593, -138.718,    11*0.,
     2             0.,0.,-22.83,-14.11, 4*0., -21.71,-11.72,      18*0.,
     3             0.,20.9,0.,0.,2.63,-0.02,0.,0.,-7.786,12.656,  18*0.,
     4             12*0.0, 0.0,                                   15*0.,
     5             3*0., 0.0, 9*0., -41.255, 48.173,              13*0.,
     6                                                            28*0.,
     7             8*0.,0.0,20.5,                                 18*0.,
     8             2*0.0,-13.96,                                  25*0.,
     9             2*0.,8.4,                                      25*0.,
     X             63.5,8.4,8.4,8.4,5*0.,26.8,                    18*0., 20th Exp
     1             0.0,58.4,12.6,0.0,4*0.0,13.3,22.0,6*0.0,29.8,  11*0.,
     2             2*0.0,0.0,27.36,                               24*0.,
     3                                                            28*0.,
     4                                                            28*0.,
     5                                                            28*0.,
     6             38.3428,2*0.,0.,2*0.,-0.114174,18*0.,-35.0599,  2*0.,
     7                                                            28*0.,
     8                                                            28*0./ 28th Exp
C        1         2         3         4         5         6         7
C--------0---------0---------0---------0---------0---------0---------0--
      DATA AZCORT/-.254,0.,-.725,-.925,-.508,-.499,-.270,-.024,   20*0.,
     2                                                            28*0.,
     3                                                            28*0.,
     4                                                            28*0.,
     5                                                            28*0.,
     6                                                            28*0.,
     7                                                            28*0.,
     8           -0.4536,0.,0.,-0.3337,                           24*0.,
     9            0.0,0.0,0.0,0.0,0.0,1.55,                       22*0.,
     X            0.,0.,0.1945,                                   25*0., 10th Exp
     1            0.0,0.0,.246,                                   25*0.,
     2                                                            28*0.,
     3            0.0,0.0,0.0,0.0,-1.1,0.0,                       22*0.,
     4                                                            28*0.,
     5                                                            28*0.,
     6                                                            28*0.,
     7                                                            28*0.,
     8                                                            28*0.,
     9                                                            28*0.,
     X                                                            28*0., 20th Exp
     1                                                            28*0.,
     2                                                            28*0.,
     3                                                            28*0.,
     4                                                            28*0.,
     5                                                            28*0.,
     6                                                            28*0.,
     7                                                            28*0.,
     8                                                            28*0./ 28th Exp
C        1         2         3         4         5         6         7
C--------0---------0---------0---------0---------0---------0---------0--
      DATA NAMES/'CHIL','CP-2','CP-3','CP-4','NOAC','NOAD','NOAK',
     X           'SKWR','FL-2','UND ','NOR ','CIM ','POLD','TOGA',
     X           'CCAA','SPAN','MIT ','MHR ','ELDA','ELDF','MELB',
     X           'DART','DARL','KATX','KICT','KFTG','KWAJ','LEMA',
     X           'NONE'/
      DATA LSTRD,LSTXP/0,0/
      CHARACTER*12 KEXP(NEXP+1)
      CHARACTER*3 REWSTR
      DATA KEXP/'CCOPE','JAWS','CYCLES','MAYPOLE-83',
     X          'LAKE SNOW-84','MAYPOLE-84','PHOENIX-84','NIMROD-78',
     X          'SOCORRO-84','PRESTORM-85','GALE-86','MIST-86',
     X          'CINDE-87','GERMAN','TAMEX','PROFS','TDWR','HARP',
     X          'WISP-90','WISP-91','CAPE','FEST92','TOGA COARE',
     X          'TRMM','DENVER','WISP-94','SCMS','MAP',
     X          'NONE'/

C     FLAG SPECIFIES IF ALTITUDE USED IS ACTUAL OR IF IT IS TO
C     BE SET TO A USER SPECIFIED VALUE
      IALTFLG=0

      READ (KRD,101)TAPE_UNIT,ITAP,TMPSKP,TMPEXP,TMPRAD,REWSTR,XRAD,
     X     YRAD,ZRAD
 101  FORMAT(/A8/A8/F8.0/F8.0/F8.0/A3,5X/F8.0/F8.0/F8.0)
      IUN = 0
      IF(TAPE_UNIT(1:1) .NE. '/') THEN
        CALL C_ATOI(TAPE_UNIT,IUN)
      END IF
      ISKP = TMPSKP
      IEXP = TMPEXP
      NUMRAD = TMPRAD
C
      ISKP = MAX0(ISKP,0)
      IF(IEXP.LE.1) THEN
         IF (IEXP.EQ.1) THEN
            TMPLAT=462555.6
            TMPLON=1055610.5
            IF(NUMRAD.GT.8) THEN
               PRINT 103
 103           FORMAT(/1X,'+++  FATAL ERROR, RADAR NUMBER ',
     X              'OUT OF RANGE  +++')
               PRINT 105,'NO.RADAR',NUMRAD
 105           FORMAT (5X,'***ERROR IN INPUT COMMAND  ',A10,'=',I10)
               STOP
            END IF
         ELSE IF (IEXP.EQ.0) THEN
            IF(USER_DEF_ORIGIN .EQ. 0) THEN
               PRINT 106
 106           FORMAT(
     X             /20X,'+++ CCOPE PROJECT AND CP-2 WILL BE ASSUMED +++'
     X             //5X,' Contact software engineer if your experiment',
     X                  ' is not in the documentation or',
     X              /5X,' use ORIGIN command to set radar and origin',
     X                  ' names, latitudes and longitudes.',
     X              /5X,' If you included an ORIGIN command, it may',
     X                  ' not have executed yet.  Check the',
     X              /5X,' final CEDRIC header to be sure all position',
     X                  ' information is correct.'/)
               IEXP=1
               NUMRAD=2
               TMPLAT=462555.6
               TMPLON=1055610.5
               ID(46)=HTRAD(NUMRAD,IEXP)
               ID(47)=XCOORD(NUMRAD,IEXP)*100
               ID(48)=YCOORD(NUMRAD,IEXP)*100
               ID(49)=AZCORT(NUMRAD,IEXP)*1000
               AZCOR=AZCORT(NUMRAD,IEXP)
            ELSE
               PRINT 108
 108           FORMAT(/5X,
     X         '+++ USING USER-DEFINED ORIGIN or NEXRAD NETWORK +++'/)
               IEXP=NEXP+1
               NUMRAD=NRMX+1
            END IF
         ELSE IF (IEXP.LT.0) THEN
            IEXP=NEXP+1
c            PRINT 107
c 107        FORMAT(5X,'+++ TAKING RADAR COORD. FROM INPUT CARD +++')
            TMPLAT=0.0
            TMPLON=0.0
            IF(NUMRAD.LE.0)THEN
               NUMRAD=NRMX+1
               NMRAD=NAMES(NUMRAD)
            END IF
         END IF
      ELSE IF(IEXP.EQ.2) THEN
         TMPLAT=0.0
         TMPLON=0.0
         IF(NUMRAD.LT.2.OR.NUMRAD.GT.4) THEN
         PRINT 105,'NO.RADAR',NUMRAD
         STOP
         ENDIF
      ELSE IF(IEXP.EQ.3) THEN
         TMPLAT=0.0
         TMPLON=0.0
         IF(NUMRAD.LT.3.OR.NUMRAD.GT.4) THEN
         PRINT 105,'NO.RADAR',NUMRAD
         STOP
         ENDIF
      ELSE IF(IEXP.EQ.4) THEN
         TMPLAT=0.0
         TMPLON=0.0
         IF(NUMRAD.NE.2) THEN
         PRINT 105,'NO.RADAR',NUMRAD
         STOP
         ENDIF
      ELSE IF(IEXP.EQ.5) THEN
         TMPLAT=431551.0
         TMPLON=860116.0
         IF(NUMRAD.LT.3.OR.NUMRAD.GT.4) THEN
         PRINT 105,'NO.RADAR',NUMRAD
         STOP
         ENDIF
      ELSE IF(IEXP.EQ.6) THEN
         TMPLAT = 395700.3
         TMPLON = 1051140.7
         IF (NUMRAD.NE.2.AND.NUMRAD.NE.4) THEN
         PRINT 105,'NO. RADAR',NUMRAD
         STOP
         END IF
      ELSE IF(IEXP.EQ.7) THEN
         TMPLAT=0.0
         TMPLON=0.0
         IF(NUMRAD.LE.2.OR.NUMRAD.GE.7) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.8) THEN
         TMPLAT = 413638.92
         TMPLON = 882407.92
         IF(NUMRAD.NE.1.AND.NUMRAD.NE.3.AND.NUMRAD.NE.4) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.9) THEN
         TMPLAT = 335710.8
         TMPLON = 1070629.05
         IF(NUMRAD.LT.3.OR.NUMRAD.GT.6) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.10) THEN
         IF(NUMRAD.EQ.3.OR.NUMRAD.EQ.4) THEN
            TMPLAT = 374432.0
            TMPLON = 974619.0
         ELSE IF(NUMRAD.EQ.11.OR.NUMRAD.EQ.12) THEN
            TMPLAT = 351411.43
            TMPLON = 972747.9
         ELSE
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.11) THEN
         TMPLAT = 351410.0
         TMPLON = -753218.0
         IF(NUMRAD.NE.3.AND.NUMRAD.NE.4.AND.NUMRAD.NE.16
     X        .AND.NUMRAD.NE.17)  THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.12) THEN
         TMPLAT = 0.0
         TMPLON = 0.0
         IF((NUMRAD.LT.2.OR.NUMRAD.GT.4).AND.
     X      (NUMRAD.LT.9.OR.NUMRAD.GT.10)) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.13) THEN
         TMPLAT = 0.0
         TMPLON = 0.0
         IF(NUMRAD.EQ.1.OR.NUMRAD.EQ.7.OR.NUMRAD.EQ.8.OR.
     X      NUMRAD.GT.10) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.14) THEN
         TMPLAT = -480500.0
         TMPLON = 111700.0
         IF(NUMRAD.NE.13) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.15) THEN
         TMPLAT = 243844.0
         TMPLON = -1204540.0
         IF(NUMRAD.NE.4.AND.NUMRAD.NE.14.AND.NUMRAD.NE.15) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.16) THEN
         TMPLAT = 394139.0
         TMPLON = -1044425.0
         IF(NUMRAD.NE.2) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.17) THEN
         TMPLAT = 395700.0
         TMPLON = -1051141.0
         IF(NUMRAD.NE.9.AND.NUMRAD.NE.10) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.18) THEN
         TMPLAT = 194322.0
         TMPLON = 1550249.0
         IF(NUMRAD.NE.3.AND.NUMRAD.NE.4) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.19) THEN
         TMPLAT = 0.0
         TMPLON = 0.0
         IF (NUMRAD.NE.3 .AND. NUMRAD.NE.18) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF (IEXP.EQ.20) THEN
         TMPLAT = 0.0
         TMPLON = 0.0
         IF (NUMRAD.NE.1 .AND. NUMRAD.NE.2 .AND. NUMRAD.NE.3 .AND.
     X       NUMRAD.NE.4 .AND. NUMRAD.NE.10 .AND. NUMRAD.NE.18) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF (IEXP.EQ.21) THEN
         TMPLAT=281346.0
         TMPLON=804408.0
         IF (NUMRAD.NE.2 .AND. NUMRAD.NE.3 .AND. NUMRAD.NE.4 .AND.
     X       NUMRAD.NE.9 .AND. NUMRAD.NE.10 .AND. NUMRAD.NE.17) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF (IEXP.EQ.22) THEN
         TMPLAT=392751.32
         TMPLON=960240.38
         IF (NUMRAD.NE.3 .AND. NUMRAD.NE.4) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF (IEXP.EQ.23) THEN
         IF (NUMRAD.NE.19 .AND. NUMRAD.NE.20) THEN
            PRINT 105, 'NO RADAR', NUMRAD
            STOP
         END IF
      ELSE IF (IEXP.EQ.24) THEN
         TMPLAT=0.0
         TMPLON=0.0
         IF (NUMRAD.NE.21 .AND. NUMRAD.NE.22 .AND. NUMRAD.NE.23
     X       .AND. NUMRAD.NE.27) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF (IEXP.EQ.25) THEN
         TMPLAT=0.0
         TMPLON=0.0
      ELSE IF (IEXP.EQ.26) THEN
         TMPLAT=400607.2
         TMPLON=1042125.2
      ELSE IF (IEXP.EQ.27) THEN
         TMPLAT=284518.0
         TMPLON=804626.0
      ELSE IF (IEXP.EQ.28) THEN
         TMPLAT=0.0
         TMPLON=0.0
         IF (NUMRAD.NE.28) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF((IEXP.GT.NEXP)) THEN
         IF(USER_DEF_ORIGIN .EQ. 0) THEN
            PRINT 106
         ELSE
            PRINT 108
         END IF
         IEXP=1
         TMPLAT=462555.6
         TMPLON=1055610.5
         IF(NUMRAD.LE.0.OR.NUMRAD.GT.8) THEN
            PRINT 103
            PRINT 105,'NO.RADAR',NUMRAD
            STOP
         END IF
      END IF
      IF(NUMRAD.NE.LSTRD.OR.IEXP.NE.LSTXP) INPTST=1
      LSTRD1=NUMRAD
      LSTXP=IEXP
      IF ((IUN.GT.0) .OR. (TAPE_UNIT(1:1) .NE. ' ')) GOTO 30
         PRINT 104
104      FORMAT(/1X,'+++  FATAL ERROR IN THE UNIT NUMBER  +++')
         PRINT 105,'UNIT NO.',IUN
         STOP
 30   CONTINUE
      IF(REWSTR(1:1).EQ.'Y') THEN
         REWSTR='YES'
         IREW=1
         CALL INIT_COS(IUN,IREW)
      ELSE
         REWSTR='NO'
         IREW=0
         CALL INIT_COS(IUN,IREW)
      END IF
C      CALL SKPVOL(IUN,ISKP)
C
C     IF ORIGINAL EXP # WAS < 0 THEN USE USER SUPPLIED COORD. OF RADAR
C
      IF(USER_DEF_ORIGIN .EQ. 0) THEN
        ORLAT = TMPLAT
        ORLON = TMPLON
      END IF
      IF (IEXP.EQ.(NEXP+1)) THEN
         IF(USER_DEF_ORIGIN .EQ. 0) THEN
            ID(46)=ZRAD*1000
            ID(47)=XRAD*100
            ID(48)=YRAD*100
         END IF
         ID(49)=0.0
         IALTFLG=1
      ELSE IF (IEXP.GE.1 .AND. IEXP.LE.NEXP) THEN
         IF(USER_DEF_ORIGIN .EQ. 0) THEN         
           ID(46)=HTRAD(NUMRAD,IEXP)
           ID(47)=XCOORD(NUMRAD,IEXP)*100
           ID(48)=YCOORD(NUMRAD,IEXP)*100
         END IF
         IF (IAZFLG.EQ.0) THEN
C
C     SET AZIMUTH CORRECTION FROM TABLE
C
            ID(49)=AZCORT(NUMRAD,IEXP)*1000
            AZCOR=AZCORT(NUMRAD,IEXP)
         END IF
      ELSE
c         WRITE(*,*)'*** INVALID STATE REACHED IN INPFIL ***'
c         STOP
      END IF

CPUT IN THE RADAR NAME IF IT WAS NOT ALREADY SET IN THE ORIGIN COMMAND
      IF(USER_DEF_ORIGIN .EQ. 0) THEN
         IF(NUMRAD .GT. 0) THEN
            RADAR_CHOSEN = 1
            POS = INDEX(NMRAD,' ')
            IF(POS .LE. 1) NMRAD=NAMES(NUMRAD)
         END IF
      ELSE
         NUMRAD=NRMX+1
         RADAR_CHOSEN = 1
      END IF

C
C     Return if either RUNOVER or APPEND mode.
C
      IF(IROV.NE.0)RETURN

      PRINT 885
885   FORMAT(//5X,'SUMMARY OF INPUT COMMAND ')
      PRINT 887
887   FORMAT(5X,'------- -- ----- ------- ')
      PRINT 888,IUN,ITAP,REWSTR,ISKP,KEXP(IEXP),NMRAD
888   FORMAT(/20X,'         UNIT NUMBER: ',I5
     X       /20X,'           TAPE NAME: ',A6
     X       /20X,'         REWIND TAPE: ',A3
     X       /20X,'NUMBER FILES TO SKIP: ',I5
     X       /20X,'     EXPERIMENT NAME: ',A12
     X       /20X,'          RADAR NAME: ',A4//)
      IF (NMRAD.EQ.'CHIL'.AND. KEXP(IEXP).EQ. 'CCOPE') PRINT 889
 889  FORMAT (//5X,'+++ WARNING - CHILL VELOCITIES WILL BE RESCALED',
     X   ' AND THE SIGNS REVERSED +++'//)
      RETURN
      END
