c
c----------------------------------------------------------------------X
c
      SUBROUTINE SMTH1D(ADAT,IFLTYP,IRAD,BDVAL,IMN,IMX,N)
C
C  PERFORM SMOOTHING (FILTERING) OF (+/-) IRAD POINTS
C
C     ADAT   - INPUT DATA TO BE FILTERED; OUTPUT FILTERED DATA
C     FDAT   - SCRATCH ARRAY FOR FILTERED DATA
C     IFLTYP - TYPE OF FILTER (UNIFORM OR TRIANGULAR)
C     IRAD   - FILTER RADIUS (FILTERS 2*IRAD+1 DATA POINTS)
C
      PARAMETER (MXT=15000)
      CHARACTER*8 IFLTYP
      CHARACTER*8 NFILT(5)
      DIMENSION ADAT(MXT),FDAT(MXT)
      DATA NFILT/'UNIF','TRIA','CRES','QUAD','EXPO'/
      DATA CNTMN,EPS/3.0,1.0E-6/

      INDX=IFIND(IFLTYP,NFILT,5)
      BIGR=(IRAD+1)**2
      IF(INDX.EQ.0)THEN
         WRITE(6,5)
    5    FORMAT(1X,'*** WARNING - UNKNOWN FILTER, RESET TO TRI ***')
         INDX=2
         IFLTYP='TRI'
      END IF

      DO 50 I=IMN,IMX
         FDAT(I)=BDVAL
         CNT=0.0
         SUMW=0.0
         WSUM=0.0
         IF(ADAT(I).EQ.BDVAL)GO TO 50
         I1=I-IRAD
         I2=I+IRAD
         IF(I1.LT.IMN.OR.I2.GT.IMX)GO TO 50
         DO 40 J=I1,I2
            IF(ADAT(J).EQ.BDVAL)GO TO 40
            SMALR=(J-I)**2
            GO TO (11,12,13,14,15),INDX
 11         DWT=1.0
            GO TO 16
 12         DWT=1-SQRT(SMALR/BIGR)
            GO TO 16
 13         DWT=(BIGR-SMALR)/(BIGR+SMALR)
            GO TO 16
 14         DWT=1-(SMALR/BIGR)
            GO TO 16
 15         DWT=EXP(-4.0*SMALR/BIGR)
 16         WSUM=WSUM+ADAT(J)*DWT
            SUMW=SUMW+DWT
            CNT=CNT+1.0
 40      CONTINUE
         IF(CNT.LT.CNTMN.OR.SUMW.LT.EPS)GO TO 50
         FDAT(I)=WSUM/SUMW
 50   CONTINUE
      IB=IMN+IRAD
      IE=IMX-IRAD
      DO 60 I=IMN,IMX
         IF(I.GE.IB.AND.I.LE.IE)THEN
            ADAT(I)=FDAT(I)
         ELSE
            ADAT(I)=BDVAL
         END IF
 60   CONTINUE
      
      RETURN
      END






