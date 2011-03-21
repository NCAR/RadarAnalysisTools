c
c----------------------------------------------------------------------X
c
      SUBROUTINE STDEV(DAT,IOUT,IIN1,C1,C2,C3,BDVAL,MNGATE,MXGATE,NGTS,
     X                 NANG,AZA,ELA,DROLD,R0,NPROC,FSPACE,DETREND,ITP,
     X                 VNYQ,AVGI,TMP1,TMP2,MXR,MXA,MXF,IQUAL)
C
C  FUNCTION - COMPUTE THE UNBIASED STANDARD DEVIATION OF A FIELD OVER
C             A REGION WITH RANGE DIMENSIONS = 2*C1+1 GATES AND ANGLE
C             DIMENSIONS = 2*C2+1 BEAMS.
C     VNYQ   - NYQUIST VELOCITY FOR LOCAL UNFOLDING
C     NPROC  - SPECIAL PROCESSING (LINEAR, UNFOLD)
C     IQUAL  - (0) Normal standard deviation, 
C              (1) QUAL = 1 - 3*VAR(VEL)/VNYQ*VNYQ
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     TMP2   - TEMPORARY STORAGE ARRAY
C
      DIMENSION TMP1(MXR),TMP2(MXR,MXA)
      DIMENSION DAT(MXR,MXA,MXF)
      DIMENSION AZA(MXA,2),ELA(MXA,2)
      CHARACTER*8 FSPACE,NPROC
      CHARACTER*1 DETREND
      DATA TORAD/0.017453293/

      AVGI=ABS(AVGI)
      IF(AVGI.LE.0.)RETURN
      IR=NINT(C1)
      JA=NINT(C2)
      IF(FSPACE.EQ.'CART    ')IR=NINT(C1/DROLD)

C     OUTER LOOPS OVER ALL OUTPUT INDICES
C
      DO 100 II=MNGATE,MXGATE
         I1=II-IR
         I2=II+IR
         IF(I1.LT.MNGATE)I1=1
         IF(I2.GT.(MXGATE-1))I2=MXGATE-1
         IF(FSPACE.EQ.'CART    ')THEN
            R=R0+(II-1)*DROLD
            IF(R.LE.0.)R=0.001
            JA=NINT(C2/(R*AVGI*TORAD))
            IF(JA.LE.0)JA=1
         END IF

c--------print *,'STDEV: i1,ii,i2=',i1,ii,i2
         DO 90 JJ=1,NANG
            DAT(II,JJ,IOUT)=BDVAL
            IF(DAT(II,JJ,IIN1).EQ.BDVAL)GO TO 90
            IF(NPROC.EQ.'UNFOLD')THEN
               IF(DAT(II,JJ,IIN1).NE.BDVAL)THEN
                  VEST=DAT(II,JJ,IIN1)
               ELSE
                  VEST=0.0
               END IF
            END IF
            J1=JJ-JA
            J2=JJ+JA
            IF(J1.LT.1)J1=1
            IF(J2.GT.NANG)J2=NANG
c-----------print *,'       j1,jj,j2=',j1,jj,j2

C           Calculate the trend over the small region (I1->I2,J1->J2)
C
            IF(DETREND.EQ.'Y')THEN
               CALL DETRND(DAT,IIN1,II,JJ,I1,I2,J1,J2,AZA,DROLD,R0,
     X              ITP,BDVAL,TMP2,IFLG,NPROC,VEST,VNYQ,MXR,MXA,MXF)
               IF(IFLG.EQ.1)GO TO 90
            END IF
            CNT=0.0
            SUM=0.0
            SUMSQ=0.0

C           INNER LOOPS OVER REGION TO GET LOCAL STANDARD DEVIATION
C
            DO 80 I=I1,I2
               DO 70 J=J1,J2
                  IF(DAT(I,J,IIN1).EQ.BDVAL)GO TO 70
                  IF(NPROC.EQ.'LINEAR')THEN
                     D=10.0**(0.1*DAT(I,J,IIN1))
                  ELSE IF(NPROC.EQ.'UNFOLD')THEN
                     D=VUNF(DAT(I,J,IIN1),VNYQ,VEST)
                  ELSE
                     D=DAT(I,J,IIN1)
                  END IF
                  IF(DETREND.EQ.'Y')THEN
                     IF(TMP2(I,J).EQ.BDVAL)GO TO 70
                     IF(NPROC.EQ.'LINEAR')THEN
                        D=D-10.0**(0.1*TMP2(I,J))
                     ELSE
                        D=D-TMP2(I,J)
                     END IF
                  END IF
                  SUM=SUM+D
                  SUMSQ=SUMSQ+D*D
                  CNT=CNT+1.0
   70          CONTINUE
   80       CONTINUE

C           CALCULATE THE LOCAL REGION STATISTICS AND STORE IN THE OUTPUT
C
c-----------print *,'STDEV: IQUAL, C3, VNYQ=',iqual,c3,vnyq
            IF(CNT.GE.C3)THEN
               DBAR=SUM/CNT
               DVAR=(SUMSQ-CNT*DBAR*DBAR)/(CNT-1.0)
               IF(DVAR.GE.0.0)THEN
                  IF(IQUAL.EQ.0)THEN
                     DAT(II,JJ,IOUT)=SQRT(DVAR)
                  ELSEIF(IQUAL.EQ.1)THEN
                     VNYQ2=VNYQ*VNYQ
                     DAT(II,JJ,IOUT)=1.0-3.0*DVAR/VNYQ2
                  ENDIF
               END IF
            END IF
   90    CONTINUE
  100 CONTINUE

      RETURN
      END
