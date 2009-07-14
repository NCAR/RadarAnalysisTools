      SUBROUTINE RDLIDAR(DEC,DECWR,WORDSZ)

      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      PARAMETER (AINMN=350.0/MXA)

      COMMON/INPUTCH/NAMFLD(MXF),IRATYP,ICORD

      CHARACTER*8 NAMFLD,IRATYP,ICORD
      CHARACTER*3 ISCTP(8)
      DATA ISCTP/'PPI','COP','RHI','VER','TAR','MAN','IDL','SUR'/

      INTEGER POW(300),VEL(300),SW(300)
      DIMENSION IBUF(500)

      IEOT=0
 5    FXOLD=-99.
      AZSUM=0.
      NAZ=1
 10   CALL GET_LIDAR(POW,VEL,NGTS,IAZ,IEL,IYR,IMON,IDAY,ITIME,
     + IR0,IDR,ISLPOW,ISLVEL,IBPOW,IBVEL,ITP,IEOT,SW,ISLSW,IBSW,
     + DEC)
      IF(ITIME.LT.IBTIME)GO TO 5
      IYR=IYR-1900
      IDATE=10000*IYR+100*IMON+IDAY

      EL=IEL*0.01
      AZ=IAZ*0.01
      R0=0.001*IR0
      DR=0.001*FLOAT(IDR)/16.
C      PRINT 102,ITIME,IDATE,AZ,EL,FXOLD,NAZ,ITP
 102  FORMAT(1X,'TIME,DATE,AZ,EL,FXOLD,NAZ,ITP= ',2I7,3F7.1,2I5)
      IF(ITP.EQ.3)THEN
         TMP=EL
         EL=AZ
         AZ=TMP
      END IF
      SCLPOW=0.01*ISLPOW
      SCLVEL=0.01*ISLVEL
      BIASP=0.01*IBPOW
      BIASV=0.01*IBVEL
      SCLSW=0.01*ISLSW
      BIASSW=0.01*IBSW
      IF((ABS(EL-FXOLD).GT.0.1.AND.FXOLD.NE.-99.).OR.IEOT.EQ.1.OR.
     +     NAZ.GT.MXA)THEN

         IF(NAZ.LE.10.AND.IEOT.EQ.0)GO TO 5
         AVGI=AZSUM/FLOAT(NAZ)
         NAZ=NAZ-1
         PRINT 103,ISCTP(ITPOLD),IDATE,ITIME,FXOLD,AZA(1,1),
     +        AZA(NAZ,1),NGTS,NAZ,AVGI
 103     FORMAT(1X,A3,2I7,2X,'FX=',F7.1,F8.1,' TO',F8.1,2X,
     +        'NGTS,NAZ,AZINC= ',2I5,F6.2)
         IFTIME=ITIME
         NANG(1)=NAZ
         FXANG=FXOLD


         IF(DR.NE.DROLD)THEN
            DROLD=DR
            R0OLD=R0
            CALL RNGST
            CALL MNMX(DROLD)
         END IF
         RETURN
      END IF
      IF(FXOLD.EQ.-99.)THEN
         FXOLD=EL
         ITPOLD=ITP
      END IF
      IF(ITPFLG(ITP).EQ.0)GO TO 10
      IF(EL.LT.FXMN(ITP).OR.EL.GT.FXMX(ITP))GO TO 10
      IF(AZ.GT.180..AND..NOT.IAZC)AZ=AZ-360.
      IF(NAZ.GT.1)THEN
         AZDIF=ABS(AZ-AZA(NAZ-1,1))
         IF(AZDIF.GT.355.)AZDIF=360.-AZDIF
         IF(AZDIF.GT.5.0)GO TO 10
         AZSUM=AZSUM+AZDIF
      END IF
      AZA(NAZ,1)=AZ
      ELA(NAZ,1)=EL
      ITM(NAZ,1)=ITIME
      DO 50 K=1,NFLDS
         IF(IFLD(K).EQ.0)GO TO 50
         IF(NAMFLD(K).EQ.'DZ      ')THEN
            CALL GBYTES(POW(1),IBUF,0,8,0,NGTS)
            DO 60 I=1,NGTS
               DAT(I,NAZ,K)=BDVAL
 60            IF(IBUF(I).GT.0)DAT(I,NAZ,K)=IBUF(I)*SCLPOW+BIASP
         ELSE IF(NAMFLD(K).EQ.'VEL     ')THEN
            CALL GBYTES(VEL(1),IBUF,0,8,0,NGTS)
            DO 70 I=1,NGTS
               DAT(I,NAZ,K)=BDVAL
 70            IF(IBUF(I).GT.0)DAT(I,NAZ,K)=IBUF(I)*SCLVEL+BIASV
         ELSE IF(NAMFLD(K).EQ.'SW      ')THEN
            CALL GBYTES(SW(1),IBUF,0,8,0,NGTS)
            DO 80 I=1,NGTS
               DAT(I,NAZ,K)=BDVAL
 80            IF(IBUF(I).GT.0)DAT(I,NAZ,K)=IBUF(I)*SCLSW+BIASSW
         END IF
 50   CONTINUE
      NAZ=NAZ+1
      GO TO 10
      END
     
         
