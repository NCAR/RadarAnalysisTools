c
c----------------------------------------------------------------------X
c
      SUBROUTINE PRNTFLD(DAT,IOUT,IIN1,IIN2,IIN3,IIN4,IIN5,AZA,ELA,
     X     ITP,DROLD,FXANG,R0,IUNPR,RMIN,RMAX,ALFT,ARHT,NSCTP,NAZ,
     X     IDATE,ITM,H0,NOUT,NIN1,NIN2,NIN3,NIN4,NIN5,MXR,MXA,MXF,
     X	   BDVAL)
C
C  PRINT FIELDS (NOUT, NIN1-5) TO UNIT (IUNPR) IF THE POINT IS 
C  WITHIN KM RADIUS OF THE SAMPLE POINT (RANGE,ANGLE)=(RNGPT,ANGPT)
C
      DIMENSION DAT(MXR,MXA,MXF),AZA(MXA,2),ELA(MXA,2)
      DIMENSION ITM(MXA,2)
      CHARACTER*8 NOUT,NIN1,NIN2,SCAN(8),CORD,NSCTP
      CHARACTER*8 NIN3,NIN4,NIN5
      LOGICAL ICROSS,ISECTOR
      DATA SCAN(1),SCAN(3)/'PPI     ','RHI     '/
      DATA RE,REI/17000.0,1.17647E-04/
      DATA TORAD,TODEG/0.017453293,57.29577951/
      
      IF(NAZ.LT.10)RETURN
      READ(NSCTP,9)RISKIP,RJSKIP
 9    FORMAT(2F4.0)
      ISKIP=NINT(RISKIP)
      JSKIP=NINT(RJSKIP)
      IF(ISKIP.EQ.0)ISKIP=1
      IF(JSKIP.EQ.0)JSKIP=1
c      print *,'PRNTFLD:,iskip,jskip=',iskip,jskip
c      WRITE(IUNPR,11),IDATE,ITM
c 11   FORMAT(/,2I10)
c      print *,'PRNTFLD: iunpr=',iunpr
      IF(ITP.NE.3)THEN
         XPT=RNGPT*SIN(ANGPT*TORAD)
         YPT=RNGPT*COS(ANGPT*TORAD)
         CORD=' (R,A)='
      ELSE
         XPT=RNGPT*COS(ANGPT*TORAD)
         ZPT=RNGPT*TAN(ANGPT*TORAD)+RNGPT*RNGPT/RE+H0
         CORD=' (R,E)='
      END IF

c      IDR=NINT(RADIUS/DROLD)
c      IR0=NINT((RNGPT-R0)/DROLD)+1
c      IR1=IR0-IDR
c      IR2=IR0+IDR
c      DANG=ATAN(RADIUS/RNGPT)*TODEG
c      ANGMN=ANGPT-DANG
c        ANGMX=ANGPT+DANG

c      RMIN=2.0
c      RMAX=50.0
c      ALFT=300.0
c      ARHT=30.0
      IR1=INT(RMIN/DROLD)
      IR2=NINT(RMAX/DROLD)+1
      IF(IR1.LE.0)IR1=1
      IF(IR2.GT.MXR)IR2=MXR
c      print *,'PRNTFLD: rmin,rmax,alft,arht=',rmin,rmax,alft,arht
c      print *,'PRNTFLD: ir1,ir2=',ir1,ir2
c      print *,'PRNTFLD: iout,iin1,iin2,iin3,iin4,iin5=',
c     +     iout,iin1,iin2,iin3,iin4,iin5

C     User-requested sector brackets 000/360 deg
C
C          IF((ARHT-ALFT).LT.0.0)ICROSS=.TRUE.
C
C              ALFT  N  ARHT
C                \   |   /
C                 \  |  /
C                  \ | /
C                   \|/
C                    +
C 
c     Save until I can figure out the logic for
c     determining if current angle is within the
c     user-specified sector.
c      IF(ICROSS) .and.
c         (ANG.GE.ALFT .and. ANG.LE.360.0) .OR.
c         (ANG.GE.0.0) .and. ANG.LE.ARHT))THEN
c         ISECTOR=.TRUE.
c      ELSE
c         ISECTOR=.FALSE.
c      ENDIF


c      ANGMN=0.0
c      ANGMX=360.0
      SECT=ARHT-ALFT

      DO 110 J=1,NAZ,JSKIP
         ANG=AZA(J,1)
         IF(ANG.LT.0.)ANG=ANG+360.
c         print *,'PRNTFLD: j,ang,sect=',j,ang,sect
         IF((ARHT-ALFT) .LT. 0.0)THEN
            IF((ANG .LT. ALFT) .AND. (ANG .GT. ARHT))GO TO 105
         ELSE
            IF((ANG .LT. ALFT) .OR. (ANG .GT. ARHT))GO TO 105
         ENDIF
 50      CONTINUE
         DO 100 I=IR1,IR2,ISKIP
            R=R0+(I-1)*DROLD
            IF(ITP.NE.3)THEN
               XX=XPT+R*SIN(ANG*TORAD)
               YY=YPT+R*COS(ANG*TORAD)
            ELSE
               XX=R*COS(ANG*TORAD)
               YY=R*TAN(ANG*TORAD)+R*R/RE+H0
            END IF
            IF(IOUT.NE.0)DATOUT=DAT(I,J,IOUT)
            IF(IIN1.NE.0)DATIN1=DAT(I,J,IIN1)
            IF(IIN2.NE.0)DATIN2=DAT(I,J,IIN2)
            IF(IIN3.NE.0)DATIN3=DAT(I,J,IIN3)
            IF(IIN4.NE.0)DATIN4=DAT(I,J,IIN4)
            IF(IIN5.NE.0)DATIN5=DAT(I,J,IIN5)
            IF(DATOUT.EQ.BDVAL .OR. 
     +           DATIN1.EQ.BDVAL .OR. 
     +           DATIN2.EQ.BDVAL .OR. 
     +           DATIN3.EQ.BDVAL .OR. 
     +           DATIN4.EQ.BDVAL .OR. 
     +           DATIN5.EQ.BDVAL)GO TO 100
            WRITE(IUNPR,67)FXANG,CORD,R,ANG,NOUT,NIN1,NIN2,
     +           NIN3,NIN4,NIN5,DATOUT,DATIN1,DATIN2,DATIN3,
     +           DATIN4,DATIN5
 67         FORMAT(1X,' FX=',F5.1,A7,2F8.3,2X,6A8,6F10.3)
c            IF(IOUT.NE.0.AND.IIN1.NE.0.AND.IIN2.NE.0)THEN
c               WRITE(IUNPR,69)FXANG,CORD,R,ANG,NOUT,NIN1,NIN2,
c     +              DATOUT,DATIN1,DATIN2
c 69            FORMAT(1X,' FX=',F5.1,A7,2F8.3,2X,3A8,3F10.3)
c            ELSE IF(IOUT.NE.0.AND.IIN1.NE.0.AND.IIN2.EQ.0)THEN
c               WRITE(IUNPR,71)FXANG,CORD,R,ANG,NOUT,NIN1,
c     +              DATOUT,DATIN1
c 71            FORMAT(1X,' FX=',F5.1,A7,2F8.3,2X,2A8,2F10.3)
c            ELSE IF(IOUT.NE.0.AND.IIN1.EQ.0.AND.IIN2.EQ.0)THEN
c               WRITE(IUNPR,73)FXANG,CORD,R,ANG,NOUT,
c     +              DATOUT
c 73            FORMAT(1X,' FX=',F5.1,A7,2F8.3,2X,A8,F10.3)
c            END IF
 100     CONTINUE
 105     CONTINUE
 110  CONTINUE

      RETURN
      END




