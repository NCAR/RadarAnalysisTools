c
c----------------------------------------------------------------------X
c
      SUBROUTINE SMOOTH(DAT,IOUT,IIN1,IFLTYP,C1,NPROC,BDVAL,
     X                  MNGATE,MXGATE,NGTS,NANG,VNYQ,DROLD,
     X                  TMP1,TMP2)
C
C  FUNCTION - PERFORM RANGE GATE SMOOTHING (FILTERING)
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     TMP1   - TEMPORARY STORAGE ARRAY
C     VNYQ   - NYQUIST VELOCITY FOR LOCAL UNFOLDING DURING FILTERING
C     IFLTYP - TYPE OF FILTER (UNIFORM OR TRIANGULAR)
C     C1     - FILTER INDEX (FILTERS 2*C1+1 RANGE GATES)
C     NPROC  - SPECIAL PROCESSING (LINEAR, UNFOLD)
C
      INCLUDE 'dim.inc'



      CHARACTER*8 NPROC
      DIMENSION DAT(MXR,MXA,MXF)
      DIMENSION TMP1(MXR),TMP2(MXR,MXA)
      DIMENSION WGT(50)
      COMMON/ANGLS/ASCAN(MXA),ANGINC(MXA),FXELC(MXA),FXERR(MXA),AVGI
      COMMON /FILCH/FLSPAC(MXF)
      CHARACTER*8 FLSPAC
      COMMON/FIL/IFILTER(MXF),IGATE(MXF),DXX(MXF),DYY(MXF)
      DATA EPS/1.0E-3/

      ISMTHGT=C1*2+1.
      IFILTER(IOUT)=IFLTYP
      IGATE(IOUT)=ISMTHGT
      FLSPAC(IOUT)='RNGE'
      DXX(IOUT)=DROLD*(C1+1.0)
      DYY(IOUT)=0.0
      GO TO(100,200)IFLTYP

C     UNIFORM FILTER
C
100   DO 95 IAZ=1,NANG
c         DO 90 I=MNGATE,MXGATE
         DO 90 I=1,NGTS
            TMP1(I)=BDVAL
            IF(DAT(I,IAZ,IIN1).EQ.BDVAL)GO TO 90
            CNT=0.0
            SUM=0.0
            I1=I-ISMTHGT/2
            I2=I+ISMTHGT/2
            IF(I1.LT.1.OR.I2.GT.NGTS)GO TO 90
            IF(NPROC.EQ.'UNFOLD')VEST=DAT(I,IAZ,IIN1)
            DO 80 J=I1,I2
               IF(DAT(J,IAZ,IIN1).EQ.BDVAL)THEN
                  TMP1(I)=BDVAL
                  GO TO 90
               END IF
               IF(NPROC.EQ.'LINEAR')THEN
                  D=10.0**(0.1*DAT(J,IAZ,IIN1))
               ELSE IF(NPROC.EQ.'UNFOLD')THEN
                  D=VUNF(DAT(J,IAZ,IIN1),VNYQ,VEST)
               ELSE
                  D=DAT(J,IAZ,IIN1)
               END IF
               SUM=SUM+D
               CNT=CNT+1.0
80          CONTINUE
            IF(CNT.GT.0.0)TMP1(I)=SUM/CNT
90       CONTINUE
         DO 85 J=1,MXR
            IF(NPROC.EQ.'LINEAR'.AND.TMP1(J).NE.BDVAL)THEN
               DAT(J,IAZ,IOUT)=10.0*ALOG10(TMP1(J))
            ELSE
               DAT(J,IAZ,IOUT)=TMP1(J)
            END IF
85       CONTINUE
95    CONTINUE
      RETURN

C     TRIANGULAR FILTER
C
200   IHF=C1+1.0001
      WT=0.
      SUMWGT=0.
      DO 210 I=1,IHF
         WT=WT+1.
         WGT(I)=WT
         WGT(ISMTHGT-I+1)=WT
210   SUMWGT=SUMWGT+2.*WT
      SUMWGT=SUMWGT-WT
      DO 220 I=1,ISMTHGT
220   WGT(I)=WGT(I)/SUMWGT

      DO 195 IAZ=1,NANG
c         DO 190 I=MNGATE,MXGATE
         DO 190 I=1,NGTS
            TMP1(I)=BDVAL
            IF(DAT(I,IAZ,IIN1).EQ.BDVAL)GO TO 190
            CNT=0.0
            SUM=0.0
            I1=I-ISMTHGT/2
            I2=I+ISMTHGT/2
            IF(I1.LT.1.OR.I2.GT.NGTS)GO TO 190
            IF(NPROC.EQ.'UNFOLD')VEST=DAT(I,IAZ,IIN1)
            DO 180 J=I1,I2
               IF(DAT(J,IAZ,IIN1).EQ.BDVAL)THEN
                  TMP1(I)=BDVAL
                  GO TO 190
               END IF
               IF(NPROC.EQ.'LINEAR')THEN
                  D=10.0**(0.1*DAT(J,IAZ,IIN1))
               ELSE IF(NPROC.EQ.'UNFOLD')THEN
                  D=VUNF(DAT(J,IAZ,IIN1),VNYQ,VEST)
               ELSE
                  D=DAT(J,IAZ,IIN1)
               END IF
               SUM=SUM+D*WGT(J-I1+1)
               CNT=CNT+WGT(J-I1+1)
180         CONTINUE
            IF(ABS(CNT-1.0).LE.EPS)TMP1(I)=SUM
190      CONTINUE
         DO 185 J=1,MXR
            IF(NPROC.EQ.'LINEAR'.AND.TMP1(J).NE.BDVAL)THEN
               DAT(J,IAZ,IOUT)=10.0*ALOG10(TMP1(J))
            ELSE
               DAT(J,IAZ,IOUT)=TMP1(J)
            END IF
185      CONTINUE
195   CONTINUE
      RETURN
      END
