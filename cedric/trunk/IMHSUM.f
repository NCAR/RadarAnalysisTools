      SUBROUTINE IMHSUM(IDEV,ID)
C
C        GENERATES A SUMMARY OF A MUDRAS 16-BIT VOLUME HEADER
C                                 IN CARTESIAN COORDINATES.
C
      PARAMETER (NMD=510)
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      DIMENSION ID(NMD),ITAX(3)
      DATA ITAX/ 'X', 'Y', 'Z' /

C-------------------------------------------------------------
c     Use DEBUG=.TRUE. to turn on debug printout of the entire
c     CEDRIC 510 word header [ID(1-510)] for checking against 
c     Mike Dixon's Radx2Grid, RadxPrint, and CedricPrint.
c     (LJM - 12/3/2012)
c
      LOGICAL DEBUG
      DATA DEBUG/.FALSE./
C-------------------------------------------------------------

      WRITE(IDEV,100)
  100 FORMAT(' ')
      IF(ID(1).NE.0) GO TO 10
      WRITE(IDEV,200)
  200 FORMAT(/5X,'NO MUDRAS FILE EXISTS AT PRESENT'/)
      RETURN
   10 CONTINUE
      SF=1./ID(68)
      CF=1./ID(69)

      iyear = id(116)
      if(id(116) .gt. 100) iyear = MOD(ID(116),100)

      write(idev,103)(id(i),i=101,104)
 103  format('VOL. NAME=',4a2)
      WRITE(IDEV,105) (ID(I),I=1,4),ID(117),ID(118),iyear,
     X   (ID(I),I=71,74),(ID(I),I=10,12),(ID(I),I=119,121),
     X   (ID(I),I=13,15),(ID(I),I=48,50),(ID(I),I=125,127),
     X   (ID(I),I=5,7),  (ID(I),I=51,54),(ID(I),I=101,104),
     X    ID(8),ID(9),(ID(I),I=55,58),(ID(I),I=16,20),(ID(I),I=45,47)
  105 FORMAT(/' MUDRAS (.MUD)  VOLUME HEADER',15X,4A2
     X   /'  GENERAL INFORMATION...'
     X/'   DATE:      ',I2.2,2('/',I2.2),5X,'SOURCE:  ',4A2,3X,
     X     'SCIENTIST: ',3A2
     X/'   BEG TIME:  ',I2.2,2(':',I2.2),5X,'RADAR:   ',3A2,5X,
     X     'SUBMITTER: ',3A2
     X/'   END TIME:  ',I2.2,2(':',I2.2),5X,'PROGRAM: ',3A2,5X,
     X     'DATE RUN:  ',4A2
     X/'   VOL. NAME: ',4A2,             5X,'PROJECT: ',2A2,7X,
     X     'TIME RUN:  ',4A2
     X/'   COORD SYS: ',2A2,             9X,'TAPE:    ',3A2,5X,
     X     'SEQUENCE:  ',3A2)
      WRITE(IDEV,106) ID(62),ID(96),ID(301),ID(63),ID(97),ID(106),
     X                ID(65),ID(99),ID(67),ID(451),ID(452),ID(453)
  106 FORMAT(/'  DATA CHARACTERISTICS...'
     X/'   COMPUTER:   ',3X,A2,5X,'RECS/FIELD:  ',I5,5X,
     X 'PTS/FIELD:  ',I6
     X/'   BITS/DATUM: ',I5,   5X,'RECS/PLANE:  ',I5,5X,
     X 'NO. PLANES: ',I6
     X/'   BLOCK SIZE: ',I5,   5X,'RECS/VOLUME: ',I5,5X,
     X 'BAD DATA:   ',I6
     X/'   WORDS/PLANE:',I5,   4X,'WORDS/FIELD: ',I6,5X,
     X 'MAX FIELDS: ',I6)

C     Some ID words are not the same for 32- and 64-bit versions,
C     WORDSZ = 32 or 64.  Print all 510 words in the volume header
C     for comparing the 32- and 64-bit versions.  (LJM 9/17/2012)
C     Comment out the CALL ID_DEBUG under normal circumstances.

      if(debug)then
         CALL ID_DEBUG(ID,NMD)
         DEBUG=.FALSE.
      end if

      N=ID(175)
      WRITE(IDEV,107) N
  107 FORMAT(/'  FIELDS PRESENT: ',I2,' ...'
     X       /4X,'NO.',3X,'NAME',7X,'SCALE FACTOR')
      K2=175
      DO 15 I=1,N
      K1=K2+1
      K2=K2+5
      WRITE(IDEV,108) I,ID(K1),ID(K1+1),ID(K1+2),ID(K1+3),ID(K1+4)
  108 FORMAT(4X,I3,3X,4A2,5X,I5)
   15 CONTINUE
      print *,'IMHSUM: id(302),id(303)=',id(302),id(303)
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
      R1=ID(35)*SF
      R2=ID(38)*SF
      WRITE(IDEV,111) ID(33),ID(34),R1,ID(36),ID(37),R2
  111 FORMAT(/'  ORIGIN  LATITUDE:',I8,' DEG',I6,' MIN',F9.2,' SEC'
     X       /9X,      'LONGITUDE:',I8,' DEG',I6,' MIN',F9.2,' SEC')
      WRITE(IDEV,112)
  112 FORMAT(/'  CARTESIAN COORDINATE SYSTEM SPECIFICATIONS...'
     X /3X,'AXIS',11X,'MINIMUM    ',5X,'MAXIMUM     ',5X,'DELTA ',
     X 7X,'NO. OF PTS.')
      K=160
C        CALCULATE KILOMETERS FROM METERS FOR Z-AXIS
      CKM=1.0
      DO 25 I=1,3
C     MARK BRADFORD PATCH TO ACCOUNT FOR ID(68).NE.100
c--------IF(I.EQ.3 .AND. ID(68).NE.100) CKM=FLOAT(ID(68))/1000.
         IF(I.EQ.3) CKM=0.01
         R1=ID(K)*SF
         R2=ID(K+1)*SF
         R3=ID(K+3)*0.001
         
         WRITE(IDEV,114) AXNAM(I),R1,LABAXS(I,1),R2,LABAXS(I,1),
     X        R3,LABAXS(I,1),ID(K+2)
 114     FORMAT(5X,A1,6X,F10.3,1X,A3,3X,F10.3,1X,A3,4X,F8.3,1X,A3,3X,I5)
         K=K+5
 25   CONTINUE
      L1=ID(164)
      L2=ID(169)
      L3=ID(174)
      R1=ID(40)*CF
      XOR=ID(41)*SF
      YOR=ID(42)*SF
      PRINT *,"  "
      PRINT *,"  GENERAL SCALING FACTOR = ",ID(68)
      PRINT *,"    ANGLE SCALING FACTOR = ",ID(69)
      WRITE(IDEV,115) AXNAM(L1),AXNAM(L2),AXNAM(L3),R1,XOR,YOR
  115 FORMAT(/3X,'AXIS ORDER IS   ',3A3,
     X    /3X,'ANGLE OF THE X-AXIS RELATIVE TO NORTH: ',F9.2,4X,'DEG.',
     X    /3X,'(X,Y)  AXIS ARE SPECIFIED RELATIVE TO:  (',
     X                F7.2,',',F7.2,')')
      IF(ID(173).LT.0) THEN
         WRITE(IDEV,117)
  117    FORMAT(3X,'ALL FIELDS ARE ASSUMED TO BE AT THE SURFACE.')
      END IF
      RETURN
      END

      SUBROUTINE ID_DEBUG(ID,NMD)
C
C     Some ID words are not the same for 32- and 64-bit versions,
C     WORDSZ = 32 or 64.  Print all 510 words in the volume header
C     for comparing the 32- and 64-bit versions.  (LJM 9/17/2012)
C
      DIMENSION ID(NMD)

      print *,' '
      print *,'IMHSUM: id(1-510), both integer and character'
      print *,' '
c     character
      print *,'IMHSUM: id(1-20), GENERAL INFORMATION'
      do i=1,20
         write(6,776)i,id(i),id(i)
      end do
c     integer
      print *,'IMHSUM: id(21-42), INTEGER'
      do i=21,42
         write(6,777)i,id(i)
      end do
c     character
      print *,'IMHSUM: id(43-58), TIMES'
      do i=43,58
         write(6,776)i,id(i),id(i)
      end do
c     Unused
      do i=59,59
         write(6,778)i,id(i)
      end do
c     integer
      print *,'IMHSUM: id(60-61), INTEGER'
      do i=60,61
         write(6,777)i,id(i)
      end do
c     character
      do i=62,62
         write(6,776)i,id(i),id(i)
      end do
c     integer
      print *,'IMHSUM: id(63-65), INTEGER'
      do i=63,65
         write(6,777)i,id(i)
      end do
c     character
      do i=66,66
         write(6,776)i,id(i),id(i)
      end do
c     integer
      print *,'IMHSUM: id(67-69), INTEGER'
      do i=67,69
         write(6,777)i,id(i)
      end do
c     Unused
      do i=70,70
         write(6,778)i,id(i)
      end do
c     character
      print *,'IMHSUM: id(71-94), INPUT FILE NAME'
      do i=71,94
         write(6,776)i,id(i),id(i)
      end do
c     Unused
      do i=95,95
         write(6,778)i,id(i)
      end do
c     integer
      print *,'IMHSUM: id(96-100), INTEGER'
      do i=96,100
         write(6,777)i,id(i)
      end do
c     character
      print *,'IMHSUM: id(101-104), VOLUME DESIGNATION'
      do i=101,104
         write(6,776)i,id(i),id(i)
      end do
c     Unused
      do i=105,105
         write(6,778)i,id(i)
      end do
c     integer
      print *,'IMHSUM: id(106-111), INTEGER'
      do i=106,111
         write(6,777)i,id(i)
      end do
c     Unused
      do i=112,115
         write(6,778)i,id(i)
      end do
c     integer
      print *,'IMHSUM: id(116-129), INTEGER'
      do i=116,129
         write(6,777)i,id(i)
      end do
c     Unused
      do i=130,131
         write(6,778)i,id(i)
      end do
c     integer
      print *,'IMHSUM: id(132-137), INTEGER'
      do i=132,137
         write(6,777)i,id(i)
      end do
c     Unused
      do i=138,138
         write(6,778)i,id(i)
      end do
c     integer
      print *,'IMHSUM: id(139-139), INTEGER'
      do i=139,139
         write(6,777)i,id(i)
      end do
c     Unused
      do i=140,141
         write(6,778)i,id(i)
      end do
c     integer
      print *,'IMHSUM: id(142-149), INTEGER'
      do i=142,149
         write(6,777)i,id(i)
      end do
c     Unused
      do i=150,150
         write(6,778)i,id(i)
      end do
c     character
      do i=151,151
         write(6,776)i,id(i),id(i)
      end do
c     integer
      print *,'IMHSUM: id(152-175), INTEGER'
      do i=152,175
         write(6,777)i,id(i)
      end do
c     character
      NFEND=176+5*ID(175)
      print *,'IMHSUM: id(176-NFEND), FIELD NAMES AND SCALING FACTORS'
      print *,'IMHSUM: id(176-NFEND), nfend=',nfend
      do i=176,NFEND
         write(6,776)i,id(i),id(i)
      end do
c     integer
      print *,'IMHSUM: id(301-305), INTEGER'
      do i=301,305
         write(6,777)i,id(i)
      end do
c     character
      NLEND=306+6*ID(302)
      print *,'IMHSUM: id(306-NLEND), LANDMARKS'
      print *,'IMHSUM: id(306-NLEND), nlend=',nlend
      do i=306,NLEND
         write(6,776)i,id(i),id(i)
      end do
c     integer
      print *,'IMHSUM: id(396-510), RESERVED FOR PROGRAM USE'
      do i=396,510
         write(6,777)i,id(i)
      end do

 776  format('ID #',i3.3,i10,2x,a2)
 777  format('ID #',i3.3,i10)
 778  format('ID #',i3.3,i10,'--UNUSED')

      RETURN
      END
