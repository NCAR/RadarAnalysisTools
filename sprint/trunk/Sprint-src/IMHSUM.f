      SUBROUTINE IMHSUM(IDEV,ICC,IYES)
C
C        GENERATES A SUMMARY OF A MUDRAS 16-BIT VOLUME HEADER
C                                 IN CARTESIAN COORDINATES.
C     ID    - 510 16-bit volume header (named IDMUD elsewhere)
C     IDMLV - 10 16-bit level header
C
      PARAMETER (NMD=510, NML=10)

      COMMON /AXUNIT/ IFIXAX,IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3),
     X     ORDER_LABL
      CHARACTER*8 AXNAM
      CHARACTER*11 ORDER_LABL

C     This ID (same as IDMUD elsewhere) array is not to be confused
C     with the array ID used elsewhere in COMMON /IDBLK/ID(NID)
C
      COMMON /MUDBLK/ ID(NMD), IDMLV(NML)
      CHARACTER*1 IYES, ITAX(3)

      DATA ITAX/ 'X', 'Y', 'Z' /
      IF(IYES.NE.'Y') RETURN
      WRITE(IDEV,100)
  100 FORMAT(A1)
      IF(ID(1).NE.0) GO TO 10
      WRITE(IDEV,200)
  200 FORMAT(/5X,'NO MUDRAS FILE EXISTS AT PRESENT'/)
      RETURN
   10 CONTINUE
      SF=1./ID(68)
      CF=1./ID(69)
      WRITE(IDEV,105) (ID(I),I=1,4),ID(117),ID(118),ID(116),
     X   (ID(I),I=71,73),(ID(I),I=10,12),(ID(I),I=119,121),
     X   (ID(I),I=13,15),(ID(I),I=48,50),(ID(I),I=125,127),
     X   (ID(I),I=5,7),  (ID(I),I=51,54),(ID(I),I=101,104),
     X    ID(8),ID(9),(ID(I),I=55,58),(ID(I),I=16,20),(ID(I),I=45,47)
  105 FORMAT(//' MUDRAS (.MUD)  VOLUME HEADER',15X,4A2
     X   /'  GENERAL INFORMATION...'
     X/' DATE:      ',I2.2,2('/',I2.2),5X,'SOURCE:  ',3A2,5X,
     X 'SCIENTIST: ',3A2,
     X/' BEG TIME:  ',I2.2,2(':',I2.2),5X,'RADAR:   ',3A2,5X,
     X 'SUBMITTER: ',3A2,
     X/' END TIME:  ',I2.2,2(':',I2.2),5X,'PROGRAM: ',3A2,5X,
     X 'DATE RUN:  ',4A2,
     X/' VOL. NAME: ', 4A2,      5X,'PROJECT: ',2A2,7X,'TIME RUN:  '
     X   ,4A2,
     X/' COORD SYS: ', 2A2,      9X,'TAPE:    ',3A2,5X,'SEQUENCE:  '
     X   ,3A2)
      WRITE(IDEV,106) ID(62),ID(96),ID(301),ID(63),ID(97),ID(106),
     X                ID(65),ID(99),ID(67)
  106 FORMAT(/'  DATA CHARACTERISTICS...'
     X/'   COMPUTER:   ',2X,A2,5X,'RECS/FIELD:  ',I4,5X,'PTS/FIELD:  '
     X,I6
     X/'   BITS/DATUM: ',I4,   5X,'RECS/PLANE:  ',I4,5X,'NO. PLANES: '
     X,I6
     X/'   BLOCK SIZE: ',I4,   5X,'RECS/VOLUME: ',I4,5X,'BAD DATA:   '
     X,I6)
      N=ID(175)
      WRITE(IDEV,107) N
  107 FORMAT(/'  FIELDS PRESENT: ',I2,' ...'
     X       /4X,'NO.',3X,'NAME',7X,'SCALE FACTOR')
      K2=175
      DO 15 I=1,N
      K1=K2+1
      K2=K2+5
      WRITE(IDEV,108) I,ID(171+I*5),ID(172+I*5),ID(173+I*5),ID(174+I*5),
     X     ID(175+I*5)
  108 FORMAT(4X,I3,3X,4A2,5X,I5)
   15 CONTINUE
      N=ID(302)
      WRITE(IDEV,109) N,ID(303)
  109 FORMAT(/'  LANDMARKS PRESENT: ',I2,5X,'(',I2,' RADAR) ...'
     X   /4X,'NO.',3X,'NAME',6X,'X (KM)',4X,'Y (KM)',4X,'Z (KM)')
      K=306
      DO 20 I=1,N
      R1=ID(K+3)*SF
      R2=ID(K+4)*SF
      R3=ID(K+5)*0.001
      WRITE(IDEV,110) I,ID(K),ID(K+1),ID(K+2), R1,R2,R3
  110 FORMAT(4X,I3,3X,3A2,2F10.2,F10.3)
      K=K+6
   20 CONTINUE
      IF(ID(303).NE.1) GO TO 21
C
C        WRITE OUT RADAR SPECS IF SINGLE RADAR
C
      R1=ID(304)*SF
      R2=ID(305)*SF
      WRITE(IDEV,116) R1,R2
  116 FORMAT('  NYQUIST VELOCITY:',F8.2,7X,'RADAR CONSTANT:',F8.2)
   21 CONTINUE
      R1= ID(35)*SF
      R2= ID(38)*SF
      WRITE(IDEV,111) ID(33),ID(34),R1,ID(36),ID(37),R2
  111 FORMAT(/'  ORIGIN  LATITUDE:',I8,' DEG',I6,' MIN',F9.2,' SEC'
     X       /9X,      'LONGITUDE:',I8,' DEG',I6,' MIN',F9.2,' SEC')
      WRITE(IDEV,112)
  112 FORMAT(/'  CARTESIAN COORDINATE SYSTEM SPECIFICATIONS...',/,
     X /7X,'AXIS',5X,'MINIMUM',3X,'MAXIMUM',5X,'DELTA',4X,'NO. OF PTS.',
     X /7X,'----',5X,'-------',3X,'-------',5X,'-----',4X,'-----------')
      K=160
C        CALCULATE KILOMETERS FROM METERS FOR Z-AXIS
      CKM=1.0
      DO 25 I=1,3
C        MARK BRADFORD PATCH TO ACCOUNT FOR ID(68).NE.100
      IF(I.EQ.3) CKM=FLOAT(ID(68))/1000.
      R1=ID(K)*SF*CKM
      R2=ID(K+1)*SF*CKM
      R3=ID(K+3)*0.001
      WRITE(IDEV,114) AXNAM(I),R1,R2,R3,ID(K+2)
  114 FORMAT(5X,A8,3F10.3,I10)
      K=K+5
   25 CONTINUE
      PRINT *,' '
      PRINT *,'  GENERAL SCALING FACTOR =',ID(68)
      PRINT *,'  ANGLE   SCALING FACTOR =',ID(69)
      L1=ID(164)
      L2=ID(169)
      L3=ID(174)
      R1=ID(40)*CF
      R2=ID(41)*SF
      R3=ID(42)*SF
      WRITE(IDEV,115) ORDER_LABL,R1,R2,R3
  115 FORMAT(/3X,'AXIS ORDER: [',A11,']',
     X     /3X,'ANGLE OF THE X-AXIS RELATIVE TO NORTH: ',F9.2,4X,'DEG.',
     X     /3X,'(X,Y)  AXES ARE SPECIFIED RELATIVE TO: ',
     X     '(',F7.2,',',F7.2,')')
      RETURN
      END
