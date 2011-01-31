      SUBROUTINE HEDSET(LTAP,ISCI,SF)
C
C        INITIALIZES THE CARTESIAN OUTPUT TAPE HEADER
C
C     IDMUD - 510 16-bit word volume header
C     IDMLV - 10 16-bit word level header (not used anywhere)
C     LHED  - 10 16-bit word level header 
C             (built in BLHED and written in OUTPCK)
C     ID    - NID integer values used throughout SPRINT
C             ID(1-9,14-16,21-22,25-27,35,41-42,45,46-48)
C             are written into IDMUD.
C
C     The following routines also have an ID array in them, but
C     it is not clear if this is the same ID array that is in
C     the COMMON block /IDBLK/
C                UFCART(IBLV(1,1,2)-->IDB) when CARTAP is called
C                CARTAP(IDB-->ITAPHD) when OUTPCK is called
C                OUTPCK(ITAPHD-->ID) when BLDDES is called
C                so that BLDES has an ID array in it.
C     Note: UFCART has the COMMON block ID and an unlabeled
C           COMMON with IBLV in it.
C
      INCLUDE 'SPRINT.INC'
c-----PARAMETER (MAXEL=150,NID=129+3*MAXEL)
c-----PARAMETER (MAXFLD=16)

      PARAMETER (NMD=510,NML=10)
      LOGICAL IS360
      COMMON /IDBLK/ID(NID)
      COMMON /MUDBLK/ IDMUD(NMD),IDMLV(NML)

      COMMON /CINP/IUN,ISKP,ORLAT,ORLON
      COMMON /CINPC/ NMRAD,ITAP,TAPE_UNIT
      CHARACTER*4 NMRAD
      CHARACTER*8 ITAP
      CHARACTER*8 TAPE_UNIT
      REAL*4  RTEMP,RFRACT,SEC
      INTEGER DEG,MIN,ISEC

      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X   ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      COMMON/ADJUST/INFLD(MAXFLD,3),SCLIF(MAXFLD,2),NIF,IOFLD(MAXFLD,3),
     X     SCLOF(MAXFLD,2),NOF,SCLAZ,SCLRNG,SCLEL,UNSCAZ,UNSCRG,UNSCEL,
     X     LOWAZ,IZAD,IS360,MINAZ,MAXAZ,METSPC(MAXFLD),IWFLD(MAXFLD),
     X     NFLI,SCLNYQ,UNSNYQ
      COMMON /FNAMES/ CINFLD(MAXFLD),CIOFLD(MAXFLD)
      CHARACTER*8 CINFLD, CIOFLD
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI,ILLE,ILLZ
      CHARACTER*8 CTEMP1,ISCI,LTAP
      CHARACTER*2 CTEMP
C      DATA IDMUD/510*0/
C      DATA IDMLV/10*0/
C      DATA (IDMUD(I),I=1,20)/4*0,'SP','RI','NT',8*' ','CR','T ',3*' '/
C      DATA (IDMUD(I),I=43,58)/'MD','T ',14*' '/
C      DATA (IDMUD(I),I=60,94)/0,510,'CR',16,2,3200,'OR',-32768,100,64,
C     X                        0,24*' '/
C      DATA (IDMUD(I),I=301,311)/0,2,1,0,0,'OR','IG','IN',0,0,0/
      DATA CF/ 64./
      DATA IBL/' '/
      CHARACTER*8 IWHERE(50)

      DO I=1,NMD
         IDMUD(I)=0
      END DO
      DO I=1,NML
         IDMLV(I)=0
      END DO

      CTEMP='SP'
      READ(CTEMP,77) IDMUD(5)
 77   FORMAT(A2)
      CTEMP='RI'
      READ(CTEMP,77) IDMUD(6)
      CTEMP='NT'
      READ(CTEMP,77) IDMUD(7)

      CTEMP=' '
      DO I=8,15
         READ(CTEMP,77) IDMUD(I)
      END DO
      READ(CTEMP,77) IDMUD(18)
      READ(CTEMP,77) IDMUD(19)
      READ(CTEMP,77) IDMUD(20)
      CTEMP='MD'
      READ(CTEMP,77) IDMUD(43)
      
      CTEMP='T '
      READ(CTEMP,77) IDMUD(44)
      CTEMP=' '
      DO I=45,58
         READ(CTEMP,77)IDMUD(I)
      END DO
      IDMUD(60)=0
      IDMUD(61)=510
      CTEMP='CR'
      READ(CTEMP,77) IDMUD(62)
      IDMUD(63)=16
      IDMUD(64)=2
      IDMUD(65)=3200
      CTEMP='OR'
      READ(CTEMP,77) IDMUD(66)
      IDMUD(67)=-32768
C
C     Find necessary scaling factor for coordinates
C
      QMAXCD=AMAX1(ABS(X1),ABS(Y1),ABS(X2),ABS(Y2))
      IF (QMAXCD .LT. 327.67) SF=100.
      IF (QMAXCD .GE. 327.67) SF=10.
      IF (QMAXCD .GE. 3276.7) SF=1.
      IDMUD(68)=NINT(SF)
      IDMUD(69)=64
      PRINT *,'  HEDSET: GENERAL SCALING FACTOR =',IDMUD(68)
      PRINT *,'  HEDSET: ANGLE   SCALING FACTOR =',IDMUD(69)

      IDMUD(70)=0
      CTEMP=' '
      DO I=71,94
         READ(CTEMP,77) IDMUD(I)
      END DO
      IDMUD(301)=0
      IDMUD(302)=2
      IDMUD(303)=1
      IDMUD(304)=0
      IDMUD(305)=0
      CTEMP='OR'
      READ(CTEMP,77) IDMUD(306)
      CTEMP='IG'
      READ(CTEMP,77) IDMUD(307)
      CTEMP='IN'
      READ(CTEMP,77) IDMUD(308)
      IDMUD(309)=0
      IDMUD(310)=0
      IDMUD(311)=0
C
C     Set OUTPUT coordinate system: COPL, LLE, ELEV, LLZ, or CRT
C
      IF (ICOPLANE.EQ.1 .OR. 
     X    ICOPLANE.EQ.2) THEN
C
C     Coplane scans --> Coplane scans (either user-specified or data angles)
C
         CTEMP='CO'
         READ(CTEMP,77) IDMUD(16)
         CTEMP='PL'
         READ(CTEMP,77) IDMUD(17)
      ELSE IF (ICOPLANE.EQ.0 .AND. IPPI.EQ.1) THEN
C
C     R,A,E scans --> Either Lon,Lat,E or X,Y,E (data elevation angles)
C
         IF (ILLE.EQ.1) THEN
            CTEMP='LL'
            READ(CTEMP,77) IDMUD(16)
            CTEMP='E '
            READ(CTEMP,77) IDMUD(17)
         ELSE
            CTEMP='EL'
            READ(CTEMP,77) IDMUD(16)
            CTEMP='EV'
            READ(CTEMP,77) IDMUD(17)
         END IF
      ELSE IF (ICOPLANE.EQ.0)THEN
C
C     R,A,E scans --> Either Lon,Lat,Z or X,Y,Z (height msl)
C
         IF(ILLZ.EQ.1)THEN
            CTEMP='LL'
            READ(CTEMP,77) IDMUD(16)
            CTEMP='Z '
            READ(CTEMP,77) IDMUD(17)
         ELSE 
            CTEMP='CR'
            READ(CTEMP,77) IDMUD(16)
            CTEMP='T '
            READ(CTEMP,77) IDMUD(17)
         END IF
      ELSE IF (ICOPLANE.EQ.3 .OR. 
     X         ICOPLANE.EQ.4 .OR. 
     X         ICOPLANE.EQ.5)THEN
C
C     Coplane, RHI, and Airborne --> X,Y,Z (height msl)
C
         CTEMP='CR'
         READ(CTEMP,77) IDMUD(16)
         CTEMP='T '
         READ(CTEMP,77) IDMUD(17)
      END IF
C
C        TAPE,PROJECT,SCIENTIST AND RADAR NAME
C
      WRITE (CTEMP1,500)LTAP
      READ (CTEMP1,104)(IDMUD(I),I=1,3)
 104  FORMAT(3A2)
      IDMUD(4)=IBL
      IDMUD(8)=ID(21)
      IDMUD(9)=ID(22)
      WRITE (CTEMP1,500)ISCI
 500  FORMAT(A8)
      READ (CTEMP1,104)(IDMUD(I),I=10,12)
  102 FORMAT(4A2)
      IDMUD(13)=ID(14)
      IDMUD(14)=ID(15)
      IDMUD(15)=ID(16)
      IDMUD(18)=IDMUD(1)
      IDMUD(19)=IDMUD(2)
      IDMUD(20)=IDMUD(3)
C
C        COORDINATE ORIGIN SPECS
C
      IF(ICOPLANE .EQ. 5) THEN
          I=ORLAT
          DEG = INT(I)
          IDMUD(33) = DEG
          RTEMP  = ORLAT - REAL(DEG)
          RFRACT = RTEMP * 3600.
          MIN    = INT(RFRACT)/60
          SEC    = RFRACT - 60. * FLOAT(MIN)
          SEC    = SEC * 100.
          ISEC   = INT(SEC)
          IDMUD(34) = MIN
          IDMUD(35) = ISEC 

          I = ORLON
          DEG   = 0
          DEG = INT(I)
          IDMUD(36) = DEG
          RTEMP  = ORLON - REAL(DEG)
          RFRACT = RTEMP * 3600.
          MIN    = INT(RFRACT)/60
          SEC    = RFRACT - 60. * FLOAT(MIN)
          SEC    = SEC * 100.
          ISEC   = INT(SEC)
          IF(MIN .LT. 0) MIN = - MIN
          IDMUD(37) = MIN
          IF(ISEC .LT. 0) ISEC = - ISEC
          IF( SEC .LT. 0)  THEN
              SEC = - SEC
          END IF
          R2     = SEC
          IDMUD(38) = ISEC
      ELSE      
         I=ORLAT 
         CALL UNCODE(I,IDMUD(33),IDMUD(34),IDMUD(35))
         IDMUD(35)=NINT((FLOAT(IDMUD(35))+(ORLAT-I))*SF)
         I=ORLON
         CALL UNCODE(I,IDMUD(36),IDMUD(37),IDMUD(38))
         IDMUD(38)=NINT((FLOAT(IDMUD(38))+(ORLON-I))*SF)
      ENDIF
      IDMUD(39)=0
      IDMUD(40)=ANGXAX*CF
      IDMUD(41)=0
      IDMUD(42)=0
C
C        SYSTEM AND JOB STATUS INFORMATION
C
      CALL GETENV('QSUB_REQNAME',IWHERE(1))
      READ (IWHERE(1),104)(IDMUD(I),I=45,47)
      CALL GETENV('LOGNAME',IWHERE(2))
      READ (IWHERE(2),104)(IDMUD(I),I=48,50)
      CALL DATEE(IWHERE)
      READ (IWHERE(1),102)(IDMUD(I),I=51,54)
      CALL CLOCK(IWHERE)
      READ (IWHERE(1),102)(IDMUD(I),I=55,58)
      IDMUD(67)=ID(45)
C
C        VOLUME DATE, TIME AND GRID SPECIFICATTIONS
C
      IDMUD(71)=ID(25)
      IDMUD(72)=ID(26)
      IDMUD(73)=ID(27)
      DO 5 I=1,3
         IDMUD(115+I)=ID(I)
         IDMUD(121+I)=ID(I)
         IDMUD(118+I)=ID(I+3)
         IDMUD(124+I)=ID(I+6)
    5 CONTINUE
      I=60*(IDMUD(119)*60+IDMUD(120))+IDMUD(121)
      J=60*(IDMUD(125)*60+IDMUD(126))+IDMUD(127)
      IDMUD(128)=J-I
      IDMUD(160)=NINT(X1*SF)
      IDMUD(161)=NINT(X2*SF)
      IDMUD(162)=NX
      IDMUD(163)=NINT(XD*1000.)
      IDMUD(165)=NINT(Y1*SF)
      IDMUD(166)=NINT(Y2*SF)
      IDMUD(167)=NY
      IDMUD(168)=NINT(YD*1000.)
      IDMUD(170)=NINT(Z1*1000.)
      IDMUD(171)=NINT(Z2*1000.)
      IDMUD(172)=NZ
      IDMUD(173)=NINT(ZD*1000.)
C
C        FIELD INFORMATION
C
      J=175
      IDMUD(175)=NOF
      DO 10 K=1,NOF
         READ (CIOFLD(K),117) IDMUD(J+1),IDMUD(J+2),IDMUD(J+3),
     X        IDMUD(J+4)
 117     FORMAT(4A2)
         IDMUD(J+5)=SCLOF(K,1)
         J=J+5
   10 CONTINUE
C
C        RADAR SPECS AND LANDMARK INFORMATION
C
C        MARK BRADFORD PATCHES ACCOUNT FOR SF.NE.100
      IDMUD(304)=ID(41)*SF/100.
      IDMUD(305)=ID(42)*SF/100.
      IF (ICOPLANE.EQ.5) THEN
C     SET HOUSEKEEPING FOR AIRBORNE SCANS
         CTEMP='AI'
         READ(CTEMP,77) IDMUD(312)
         CTEMP='RB'
         READ(CTEMP,77) IDMUD(313)
         CTEMP='RN'
         READ(CTEMP,77) IDMUD(314)
      ELSE
         IDMUD(312)=ID(14)
         IDMUD(313)=ID(15)
         IDMUD(314)=ID(16)
      END IF
C
C     PUT RADAR COORDINATES IN RELATIVE COORDINATE SYSTEM
C
      ATR=ATAN(1.)/45.
      XRAD=ID(47)*SF
      YRAD=ID(48)*SF
      BASANG=(ANGXAX-90.)*ATR
      XRAD1=XRAD*COS(BASANG) - YRAD*SIN(BASANG)
      YRAD1=XRAD*SIN(BASANG) + YRAD*COS(BASANG)
      IDMUD(315)=XRAD1/100.
      IDMUD(316)=YRAD1/100.
      IDMUD(317)=ID(46)
C     
C        ELEVATION INFORMATION (SCALED)
C
      IDMUD(400)=ID(35)
c-----No longer put into cedric output header
c      INFIN=(ID(35)-1)*3+IPTR_INT
c      INDEX=400
c      DO 20 I=129,INFIN,3
c         INDEX=INDEX+1
c         IDMUD(INDEX)=ID(I)
c 20   CONTINUE
c-----No longer put into cedric output header

      RETURN
      END
